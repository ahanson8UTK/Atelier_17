# --- Packages: install missing + load quietly ----------------------------------
pkgs <- c("remotes","WDI","readr","dplyr","tidyr","tibble","purrr","stringr",
          "lubridate","zoo","xts","ggplot2","vars","progress","shiny","shinyWidgets")
miss <- setdiff(pkgs, rownames(installed.packages()))
if(length(miss)) install.packages(miss, repos = "https://cloud.r-project.org", quiet = TRUE)
if(!"fbi"   %in% rownames(installed.packages())) remotes::install_github("cykbennie/fbi", quiet = TRUE)
if(!"dfms"  %in% rownames(installed.packages())) try(install.packages("dfms", repos="https://cloud.r-project.org", quiet=TRUE), silent=TRUE)
if(!"MARSS" %in% rownames(installed.packages())) install.packages("MARSS", repos="https://cloud.r-project.org", quiet=TRUE)

suppressPackageStartupMessages({
  library(fbi); library(WDI); library(readr); library(dplyr); library(tidyr); library(tibble)
  library(purrr); library(stringr); library(lubridate); library(zoo); library(xts)
  library(ggplot2); library(vars); library(progress); library(shiny); library(shinyWidgets)
  ok_dfms <- requireNamespace("dfms", quietly = TRUE); if(ok_dfms) library(dfms)
  ok_marss <- requireNamespace("MARSS", quietly = TRUE)
})

# --- 0) Local FRED‑MD path (no downloads) -------------------------------------
FRED_MD_PATH <- "fred_md.csv"    # <- place the FRED‑MD monthly CSV here
if(!file.exists(FRED_MD_PATH)) stop("Could not find fred_md.csv at: ", normalizePath(FRED_MD_PATH, mustWork = FALSE))

# --- 1) Read & transform FRED‑MD (monthly) via fbi -----------------------------
fred_monthly <- fbi::fredmd(file = FRED_MD_PATH, transform = TRUE) |> rm_outliers.fredmd()
# --- convert away the 'fredmd' class, normalize the date column, then filter/arrange ---
fred_monthly_df <- fred_monthly |>
  (function(df) {
    df <- as.data.frame(df)                  # drop 'fredmd' class
    if ("sasdate" %in% names(df) && !("date" %in% names(df))) {
      names(df)[names(df) == "sasdate"] <- "date"
    }
    df
  })() |>
  tibble::as_tibble() |>
  dplyr::mutate(date = as.Date(date)) |>
  dplyr::filter(date >= as.Date("1960-01-01")) |>
  dplyr::arrange(date)

# Continue as before:
X_m  <- fred_monthly_df
X_xts <- xts::xts(as.matrix(dplyr::select(X_m, -date)), order.by = X_m$date)

# Anchor series codes (INDPRO, FEDFUNDS, PCEPI, S&P P/E)
data("fredmd_description", package = "fbi")
code_for <- function(pattern){
  fredmd_description |> filter(str_detect(description, regex(pattern, ignore_case=TRUE))) |> slice(1) |> pull(fred)
}
code_INDPRO   <- "INDPRO"
code_FEDFUNDS <- "FEDFUNDS"
code_PCEPI    <- "PCEPI"
code_PE       <- "NONREVSL"

# --- 2) Annual welfare series (sources used by State‑of‑the‑Nation) ------------
wdi_codes <- c(
  life_expectancy = "SP.DYN.LE00.IN",
  under5_mort    = "SH.DYN.MORT",
  gini           = "SI.POV.GINI",
  suicide        = "SH.STA.SUIC.P5"
)
wdi_us <- WDI(country = "US", indicator = wdi_codes,
              start = 1960, end = year(max(index(X_xts))), extra = FALSE) |>
  as_tibble() |> rename(year = year) |> arrange(year)

# Life satisfaction (Our World in Data, Cantril ladder)
owid_ls_url <- "https://ourworldindata.org/grapher/happiness-cantril-ladder.csv"
ls_raw <- readr::read_csv(owid_ls_url, show_col_types = FALSE) |> filter(Entity == "United States")
val_col <- setdiff(names(ls_raw), c("Entity","Code","Year"))[1]
life_sat <- ls_raw |> transmute(year = .data[["Year"]], life_satisfaction = .data[[val_col]]) |> arrange(year)

if (!all(names(wdi_codes) %in% names(wdi_us))) {
  # rename(new = old)
  ren_map <- setNames(names(wdi_codes), unname(wdi_codes))
  wdi_us  <- dplyr::rename(wdi_us, !!!ren_map)
}

# Now build the annual welfare table (use fully qualified dplyr verbs)
welfare_annual <- wdi_us |>
  dplyr::select(dplyr::any_of(c("year", names(wdi_codes)))) |>
  dplyr::left_join(life_sat, by = "year") |>
  dplyr::arrange(year) |>
  dplyr::mutate(
    under5_mort = -under5_mort,  # good is up
    suicide     = -suicide
  )

# --- 3) Embed annual welfare at monthly frequency (observed only in December) --
to_monthly_stub <- function(df_year_value, start_date, end_date){
  months <- seq(from = as.Date(start_date), to = as.Date(end_date), by = "month")
  yy <- tibble(date = months, year = year(months), month = month(months)) |>
    left_join(df_year_value, by = "year") |>
    transmute(date, value = ifelse(month == 12, value, NA_real_))
  xts::xts(yy$value, order.by = yy$date)
}
start_date <- min(index(X_xts)); end_date <- max(index(X_xts))
w_xts <- list(
  life_expectancy   = to_monthly_stub(dplyr::transmute(welfare_annual, year = as.integer(year), value = as.numeric(life_expectancy)),   start_date, end_date),
  under5_mort       = to_monthly_stub(dplyr::transmute(welfare_annual, year = as.integer(year), value = as.numeric(under5_mort)),       start_date, end_date),
  gini              = to_monthly_stub(dplyr::transmute(welfare_annual, year = as.integer(year), value = as.numeric(gini)),              start_date, end_date),
  suicide           = to_monthly_stub(dplyr::transmute(welfare_annual, year = as.integer(year), value = as.numeric(suicide)),           start_date, end_date),
  life_satisfaction = to_monthly_stub(dplyr::transmute(welfare_annual, year = as.integer(year), value = as.numeric(life_satisfaction)), start_date, end_date)
)
for (nm in names(w_xts)) colnames(w_xts[[nm]]) <- nm
Y_xts <- cbind(X_xts, do.call(cbind, w_xts))

# Robust extractor: returns a tibble(date, f1, ..., fr) with T = length(dates)
extract_dfms_factors <- function(fit, r, dates){
  X <- NULL
  # try common slots
  for(nm in c("F","Ft","factors","Factors","Fhat","F_hat")) {
    if(!is.null(fit[[nm]])) { X <- fit[[nm]]; break }
  }
  # try fitted() method
  if (is.null(X)) X <- tryCatch(fitted(fit, type = "factors"), error = function(e) NULL)
  # try generic state/smooth slots seen in some builds
  if (is.null(X)) {
    for(nm in c("states","Xs","x_smoothed","x.smooth","Smoothed","Xs.smooth","Xt_smooth")){
      if(!is.null(fit[[nm]])) { X <- fit[[nm]]; break }
    }
  }
  if (is.null(X)) stop("dfms factors not found; available names: ", paste(names(fit), collapse = ", "))
  
  X <- as.matrix(X)
  # orient as T x r
  if (nrow(X) == r && ncol(X) == length(dates)) X <- t(X)
  
  # pad/trim to monthly index length
  Tlen <- length(dates)
  if (nrow(X) < Tlen) {
    X <- rbind(matrix(NA_real_, Tlen - nrow(X), ncol(X)), X)
  } else if (nrow(X) > Tlen) {
    X <- tail(X, Tlen)
  }
  
  # build tibble with explicit names (avoid rename_with)
  out <- tibble(date = as.Date(dates))
  for (j in seq_len(r)) out[[paste0("f", j)]] <- X[, j]
  out
}

# --- 4) EM Dynamic Factor Model (monthly), K = 4 factors -----------------------
set.seed(123)
ENGINE <- if (ok_dfms) "DFMS" else if (ok_marss) "MARSS" else "PCA"

if (ENGINE == "DFMS") {
  r <- 4; p_lags <- 2
  dfm_fit <- dfms::DFM(Y_xts, r = r, p = p_lags)   # prints "Converged after ..." on success
  F_hat   <- extract_dfms_factors(dfm_fit, r = r, dates = index(Y_xts))
  # loadings if available; otherwise we’ll estimate later via regressions
  C_load  <- tryCatch(dfm_fit$C, error = function(e) NULL)
  if (is.null(C_load)) C_load <- matrix(NA_real_, nrow = ncol(Y_xts), ncol = r,
                                        dimnames = list(colnames(Y_xts), paste0("f",1:r)))
} else if (ENGINE == "MARSS") {  # fallback EM state-space
  r <- 4
  Y <- t(coredata(Y_xts)); rownames(Y) <- colnames(Y_xts)
  Z <- matrix(list(0), nrow = nrow(Y), ncol = r); for(i in 1:nrow(Y)) for(j in 1:r) Z[i,j] <- paste0("z", i, "_", j)
  A <- "zero"; R <- "diagonal and unequal"
  B <- matrix(list(0), r, r); for(j in 1:r) B[j,j] <- paste0("phi", j)
  U <- "zero"; Q <- "diagonal and unequal"; x0 <- "unequal"; V0 <- "diagonal and unequal"
  fit <- MARSS::MARSS(Y, model = list(Z=Z,A=A,R=R,B=B,U=U,Q=Q,x0=x0,V0=V0), silent = TRUE, control = list(maxit=1000))
  sm  <- MARSS::MARSSkfss(fit)
  # build tibble(date, f1..fr) explicitly
  F_hat <- tibble(date = index(Y_xts))
  for (j in 1:r) F_hat[[paste0("f", j)]] <- as.numeric(t(sm$xtt)[, j])
  C_load <- coef(fit, type="matrix")$Z
} else {                         # last-ditch PCA (no EM)
  r <- 4
  Zs  <- scale(Y_xts)
  pcs <- prcomp(na.omit(Zs))$x
  FF  <- matrix(NA_real_, nrow = nrow(Zs), ncol = r)
  FF[as.numeric(rownames(pcs)), ] <- pcs[, 1:r, drop=FALSE]
  F_hat <- tibble(date = index(Y_xts))
  for (j in 1:r) F_hat[[paste0("f", j)]] <- FF[, j]
  C_load <- matrix(NA_real_, nrow = ncol(Y_xts), ncol = r,
                   dimnames = list(colnames(Y_xts), paste0("f",1:r)))
}


# --- 5) Anchor & order factors: INDPRO, FEDFUNDS, PCEPI, S&P P/E (robust) ----
# Ensure plain data.frames
F_hat_df <- as.data.frame(F_hat)
X_m_df   <- as.data.frame(X_m)

# 5a) Build anchor reference table from your fred_md panel
grab_anchor <- function(code) {
  if (!code %in% names(X_m_df)) return(rep(NA_real_, nrow(X_m_df)))
  as.numeric(scale(X_m_df[[code]]))
}
Aref <- data.frame(
  date     = X_m_df[["date"]],
  INDPRO   = grab_anchor(code_INDPRO),
  FEDFUNDS = grab_anchor(code_FEDFUNDS),
  PCEPI    = grab_anchor(code_PCEPI),
  PE       = grab_anchor(code_PE)
)

# 5b) Strip 'date' from factors WITHOUT dplyr::select (avoids masking issues)
stopifnot("date" %in% names(F_hat_df))
F_core <- F_hat_df[, setdiff(names(F_hat_df), "date"), drop = FALSE]

# 5c) Inner-join by date, drop rows with missing anchors, and build matrices
FA <- merge(
  x = data.frame(date = F_hat_df$date, F_core, check.names = FALSE),
  y = Aref,
  by = "date",
  all = FALSE
)
FA <- FA[stats::complete.cases(FA[, c("INDPRO","FEDFUNDS","PCEPI","PE")]), ]

# Factor matrix (columns assumed f1..fr); anchor matrix (four anchors)
F_cols <- grep("^f\\d+$", names(FA), value = TRUE)
if (length(F_cols) == 0) stop("No factor columns named f1..fr found in F_hat.")
F_mat <- as.matrix(FA[, F_cols, drop = FALSE])
A_mat <- as.matrix(FA[, c("INDPRO","FEDFUNDS","PCEPI","PE"), drop = FALSE])

# 5d) Correlations, greedy assignment, ordering = [INDPRO, FEDFUNDS, PCEPI, PE]
cors   <- stats::cor(F_mat, A_mat, use = "pairwise.complete.obs")
assign <- apply(abs(cors), 1, which.max)                       # anchor index best matching each factor
order_map <- match(c("INDPRO","FEDFUNDS","PCEPI","PE"), colnames(A_mat))[order(assign)]
F_ord <- F_mat[, order_map, drop = FALSE]

# Flip signs so correlations with anchors are positive
for (j in seq_len(ncol(F_ord))) {
  if (stats::cor(F_ord[, j], A_mat[, j], use = "pairwise.complete.obs") < 0) F_ord[, j] <- -F_ord[, j]
}

# 5e) Name anchored factors and re‑align to the full factor date index
F_named <- data.frame(
  date        = FA$date,
  F_INDPRO    = F_ord[, 1],
  F_FEDFUNDS  = F_ord[, 2],
  F_PCEPI     = F_ord[, 3],
  F_PE        = F_ord[, 4],
  check.names = FALSE
)
# Merge back to the full F_hat date span so later steps don’t lose rows
F_named <- merge(data.frame(date = F_hat_df$date), F_named, by = "date", all.x = TRUE)

# --- 6) VAR on factors + Cholesky identification -------------------------------
stopifnot("date" %in% names(F_named))

# drop the date column using base indexing
Yfac <- as.data.frame(F_named)[ , !(names(F_named) %in% "date"), drop = FALSE]

# keep rows with all factors observed (after anchoring there can be NAs)
Yfac <- Yfac[stats::complete.cases(Yfac), , drop = FALSE]

# optional: sanity check
# print(dim(Yfac)); print(head(colnames(Yfac)))

# -- VAR order selection and estimation -----------------------------------------
sel   <- vars::VARselect(as.matrix(Yfac), lag.max = 4, type = "const")
p_var <- sel$selection["AIC(n)"]
if (is.na(p_var) || length(p_var) == 0) p_var <- 2

VAR_fit <- vars::VAR(as.matrix(Yfac), p = p_var, type = "const")

get_Amats <- function(vfit){
  K <- length(vfit$varresult); p <- vfit$p; k_names <- names(vfit$varresult)
  A <- vector("list", p)
  for(L in 1:p){
    A_L <- matrix(NA_real_, K, K, dimnames = list(k_names, k_names))
    for(i in seq_along(k_names)){
      cf <- coef(vfit$varresult[[i]])
      A_L[i, ] <- cf[paste0(k_names, ".l", L)]
    }
    A[[L]] <- A_L
  }
  A
}
irf_factors <- function(vfit, h = 60){
  A <- get_Amats(vfit); p <- length(A); K <- nrow(A[[1]])
  Theta <- array(0, dim = c(K,K,h+1)); diag(Theta[,,1]) <- 1
  for(H in 2:(h+1)){
    acc <- matrix(0, K, K)
    for(L in 1:min(p, H-1)) acc <- acc + A[[L]] %*% Theta[,,H-L]
    Theta[,,H] <- acc
  }
  Su <- crossprod(residuals(vfit)) / nrow(residuals(vfit))
  P  <- t(chol(Su))  # u_t = P * eps_t (Cholesky; order = anchored Slow–R–Fast)
  lapply(1:K, function(j){ sapply(0:h, function(H) Theta[,,H+1] %*% P[,j, drop=FALSE]) |> matrix(nrow = K) })
}
H <- 60
irfF_list <- irf_factors(VAR_fit, h = H)

# --- 7) Map factor IRFs to welfare via measurement loadings --------------------
series_names   <- colnames(Y_xts)
welfare_names  <- c("life_expectancy","under5_mort","gini","suicide","life_satisfaction")
w_idx <- match(welfare_names, series_names)
# --- Unconditional SDs (per welfare variable; use observed months, ignore NAs) ---
sd_welfare <- setNames(
  sapply(welfare_names, function(w) stats::sd(as.numeric(Y_xts[, w]), na.rm = TRUE)),
  welfare_names
)
# Guard against zeros / non-finite
sd_welfare[!is.finite(sd_welfare) | sd_welfare <= 0] <- 1


C_load <- if(exists("C_load")) C_load else matrix(NA_real_, nrow = ncol(Y_xts), ncol = ncol(Yfac))
if(is.null(rownames(C_load))) rownames(C_load) <- series_names
if(any(is.na(C_load))){
  F_full <- as.matrix(as.data.frame(F_named)[ , !(names(F_named) %in% "date"), drop = FALSE])
  for(i in seq_along(series_names)){
    yi <- as.numeric(Y_xts[, i]); ok <- stats::complete.cases(yi, F_full)
    if(sum(ok) > 50) C_load[i, 1:ncol(F_full)] <- coef(lm(yi[ok] ~ F_full[ok, ] - 1))
  }
}

irf_y <- setNames(vector("list", length(welfare_names)), welfare_names)
for(w in seq_along(welfare_names)){
  z_w <- matrix(C_load[w_idx[w], 1:ncol(Yfac)], nrow = 1)
  irf_y[[w]] <- lapply(1:ncol(Yfac), function(j) as.numeric(z_w %*% irfF_list[[j]]))
}

# --- 8) Wild bootstrap (B = 1000) for 90% IRF bands ---------------------------
set.seed(42)
B <- 1000
prog <- progress_bar$new(total = B, format = "boot [:bar] :percent eta: :eta")
F_mat <- as.matrix(Yfac); K <- ncol(F_mat); p <- VAR_fit$p; Tn <- nrow(F_mat)
Uhat  <- residuals(VAR_fit)
const <- sapply(VAR_fit$varresult, function(m) coef(m)["const"])
A_list  <- get_Amats(VAR_fit); A_stack <- do.call(cbind, A_list)
sim_VAR <- function(const, A_stack, U, y0){
  p <- ncol(A_stack)/nrow(A_stack); K <- nrow(A_stack); Tn <- nrow(U)
  Y <- matrix(NA_real_, nrow = Tn + p, ncol = K); Y[1:p, ] <- y0
  for(t in 1:Tn){
    lag_stack <- do.call(c, lapply(1:p, function(L) Y[p + t - L, ]))
    Y[p + t, ] <- const + A_stack %*% lag_stack + U[t, ]
  }
  Y[(p+1):(p+Tn), , drop=FALSE]
}
boot_store <- lapply(welfare_names, function(.) lapply(1:K, function(.) matrix(NA_real_, nrow = H+1, ncol = B)))
names(boot_store) <- welfare_names
y0 <- F_mat[1:p, , drop=FALSE]

for(b in 1:B){
  prog$tick()
  mult  <- sample(c(-1,1), size = nrow(Uhat), replace = TRUE)  # Rademacher
  Ustar <- Uhat * mult
  Ystar <- sim_VAR(const, A_stack, Ustar, y0)
  Vb <- try(VAR(Ystar, p = p, type = "const"), silent = TRUE)
  if(inherits(Vb, "try-error")) next
  irfF_b <- irf_factors(Vb, h = H)
  for(w in seq_along(welfare_names)){
    z_w <- matrix(C_load[w_idx[w], 1:K], nrow = 1)
    for(j in 1:K){
      boot_store[[w]][[j]][ , b] <- as.numeric(z_w %*% irfF_b[[j]])
    }
  }
}
qfun <- function(mat){ list(lower = apply(mat, 1, quantile, 0.05, na.rm=TRUE),
                            upper = apply(mat, 1, quantile, 0.95, na.rm=TRUE)) }
bands <- lapply(welfare_names, function(w) lapply(1:K, function(j) qfun(boot_store[[w]][[j]])))
names(bands) <- welfare_names

# --- Put IRFs and bands in σ-units (divide by unconditional SD of each welfare series) ---
for(w in seq_along(welfare_names)){
  s <- sd_welfare[[ welfare_names[w] ]]
  for(j in 1:K){
    irf_y[[w]][[j]]            <- irf_y[[w]][[j]] / s
    bands[[w]][[j]]$lower      <- bands[[w]][[j]]$lower / s
    bands[[w]][[j]]$upper      <- bands[[w]][[j]]$upper / s
  }
}

# --- 9) Save bundle for the Shiny app -----------------------------------------
shock_labels <- c("Shock to F1 (INDPRO)","Shock to F2 (FEDFUNDS)",
                  "Shock to F3 (PCEPI)","Shock to F4 (Credit)")
store <- list(H = H, welfare_names = welfare_names, shock_labels = shock_labels, irf_y = irf_y, bands = bands)
dir.create("irf_app", showWarnings = FALSE); saveRDS(store, file = "irf_app/irf_store.rds")

# --- 10) Write the Shiny app (reads precomputed IRFs/bands) --------------------
app_code <- '
suppressPackageStartupMessages({ library(shiny); library(ggplot2); library(dplyr) })
store <- readRDS("irf_store.rds")
ui <- fluidPage(
  titlePanel("IRFs on Welfare (precomputed)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("shock", "Impulse (structural shock):",
                  choices = setNames(1:4, store$shock_labels), selected = 1),
      selectInput("welfare", "Response (welfare measure):",
                  choices = setNames(store$welfare_names, store$welfare_names),
                  selected = store$welfare_names[1]),
      sliderInput("h", "Horizon (months):", min = 0, max = store$H, value = store$H, step = 1)
    ),
    mainPanel(
      plotOutput("irfPlot", height = "430px"),
      tags$hr(),
      p("Shaded = 90% wild bootstrap. Welfare observed annually (Dec). EM state-space smooths monthly factors using the full panel.")
    )
  )
)
server <- function(input, output, session){
  output$irfPlot <- renderPlot({
    j <- as.integer(input$shock); w <- input$welfare; h <- as.integer(input$h)
    y   <- store$irf_y[[w]][[j]][1:(h+1)]
    low <- store$bands[[w]][[j]]$lower[1:(h+1)]
    up  <- store$bands[[w]][[j]]$upper[1:(h+1)]
    df <- tibble(h = 0:h, irf = y, low = low, up = up)
    ggplot(df, aes(h, irf)) +
      geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.2) +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      geom_line(linewidth = 0.7) +
      labs(x = "Months after shock", y = "Response (standardized units)",
           title = paste0(names(store$shock_labels)[j], " → ", w)) +
      theme_minimal(base_size = 13)
  })
}
shinyApp(ui, server)
'
writeLines(app_code, con = "irf_app/app.R")

message("Build complete. Launch the app with: shiny::runApp('irf_app/app.R')")

# ----------------------------------- To Do -----------------------------------
# 1. Do you believe GDP is a sufficient statistic for the health of a country's
#    economy? Why or why not? Be persuasive
# 2. You have four shocks and five potential "welfare" proxies. Examine how each
#    each of the four shocks impacts each of the five welfare measures.
#   a. For now don't worry about magnitudes. Do all of the IRF signs on impact meet
#      your prior expectations? If anything stand out, note it here.
#   b. Now focus on the magnitudes. These are in standard deviations of the underlying
#      series. In other words ~68% of the variable's fluctuations lie between -1 and 1.
#      What do the magnitudes tell you about the economic significance of these shocks
#      on our welfare measures?
# 3. Summarize your takeaways from today's class both here and on your reflections.

