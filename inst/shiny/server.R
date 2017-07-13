
server <- function(input, output){
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu Item", icon =icon("calendar"))
    )
  })

  output$plot1 <- renderPlot({
    df <- rnorm(input$slider)
    hist(df, main = "Histogram")
  })

  output$auc <- renderUI({

  })

}
