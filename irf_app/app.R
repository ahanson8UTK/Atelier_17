
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

