library(shiny)
library(dplyr)
library(magrittr)
# library(dygraphs)
# library(datasets)
# path_files <- "C:/Users/sbohora/Documents/GitHub/sAUC/R/"
# lapply(list.files(path = file.path(path_files), pattern = "[.]R$", recursive = TRUE), function(x) source(paste0(path_files,"/",x)))

# source("C:/Users/sbohora/Documents/GitHub/sAUC/R/compute-auc.R")
# source("C:/Users/sbohora/Documents/GitHub/sAUC/R/compute-inverse.R")
# source("C:/Users/sbohora/Documents/GitHub/sAUC/R/simulation-one-predictor.R")

shinyServer(function(input, output){
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu Item", icon =icon("calendar"))
    )
  })

  result_of_simulate <- reactive({
    iter <- input$realization
    m <- input$number_treatment
    p <- input$number_control
    b0 <- input$b0
    b1 <- input$b1
    b2 <- input$b2
    simulate_one_predictor(iter = iter, m = m, p = p, b0 = b0, b1 = b1, b2 = b2)
  })

  output$result1 <- DT::renderDataTable({
    result_simulate <- result_of_simulate()
    df <- (as.data.frame(cbind(result_simulate$meanbeta, result_simulate$meanvar, result_simulate$meansd, result_simulate$ci_betass, result_simulate$all_coverage, result_simulate$iter)))
    names(df) <- c("Beta Estimates", "Variance of Beta", "S.E. of Beta","Confidence Interval on Beta", "Coverage Probability", "Iterations")
    dt <- DT::datatable(
      df,
      caption = htmltools::tags$caption(
        style = "font-size:150%",
        'Table 1. Results of the Simulation on sAUC with one discrete covariate'),
      rownames = c("B0", "B1", "B2"))
  })

  output$result_plot_beta <- renderPlot({
    simulated_betas <- result_of_simulate()
    dddd <- as.data.frame(simulated_betas$m_betas)

    data_long <- gather(dddd, Parameter, values, factor_key=TRUE)
    data_long$Parameter <- with(data_long, ifelse(Parameter == "V1","0", 
                                                  ifelse(Parameter =="V2","1", "2")))
    mu <- data_long %>%
      dplyr::group_by(Parameter) %>%
      dplyr::summarize(mean_beta = mean(values)) %>% as.data.frame()

    # Change colors by groups
    ggplot(data_long, aes(x=values, color=Parameter, fill=Parameter)) +
      geom_histogram(aes(y=..density..), position="identity", alpha=0.5) +
      geom_density(alpha=0.6) + 
      facet_grid(.~Parameter, labeller = label_bquote(cols = beta[.(Parameter)])) +
      geom_vline(data=mu, aes(xintercept=mean_beta, color=Parameter),linetype="dashed") +
      scale_color_manual(values=c("red", "green", "purple")) +
      # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
      labs(x="Estimates", y = "Density") +
      theme_classic() + 
      theme(text = element_text(size=20)) + 
      theme(legend.position="none")
  })
})
