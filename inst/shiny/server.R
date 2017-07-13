library(shiny)
library(dygraphs)
library(datasets)
# path_files <- "C:/Users/sbohora/Documents/GitHub/sAUC/R/"
# lapply(list.files(path = file.path(path_files), pattern = "[.]R$", recursive = TRUE), function(x) source(paste0(path_files,"/",x)))

source("C:/Users/sbohora/Documents/GitHub/sAUC/R/compute-auc.R")
source("C:/Users/sbohora/Documents/GitHub/sAUC/R/compute-inverse.R")
source("C:/Users/sbohora/Documents/GitHub/sAUC/R/simulation-one-predictor.R")

shinyServer(function(input, output){
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu Item", icon =icon("calendar"))
    )
  })

  # output$plot1 <- renderPlot({
  #   df <- rnorm(input$iteration)
  #   hist(df, main = "Histogram")
  # })
  
  predicted <- reactive({
    hw <- HoltWinters(ldeaths)
    predict(hw, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  output$dygraph <- renderDygraph({
    dygraph(predicted(), main = "Predicted Deaths/Month") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      dyOptions(drawGrid = input$showgrid)
  })

  # output$auc <- renderUI({
  #   if (input$sidebar_menu == "auc_reg"){
  #     inputPanel(selectInput("teamA", label = NULL))
  #   }

  # })
}
)


y <- simulate_one_predictor(100, 50, 25)
y
