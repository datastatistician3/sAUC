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

  output$result1 <- DT::renderDataTable({
    iter <- input$realization
    m <- input$number_treatment
    p <- input$number_control
    result_simulate <- simulate_one_predictor(iter = iter, m = m, p = p)

    df <- (as.data.frame(cbind(result_simulate$meanbeta, result_simulate$meanvar, result_simulate$meansd, result_simulate$ci_betass, result_simulate$all_coverage, result_simulate$iter)))
    names(df) <- c("Beta Estimates", "Variance of Beta", "S.E. of Beta","Confidence Interval on Beta", "Coverage Probability", "Iterations")
    dt <- DT::datatable(
      df,
      caption = htmltools::tags$caption(
        style = "font-size:110%",
        'Table 1. Results of the Simulation on sAUC with one discrete covariate'),
      rownames = FALSE)
  })
  }
  )



