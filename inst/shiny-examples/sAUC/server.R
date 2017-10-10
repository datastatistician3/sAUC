library(shiny)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(DT)
library(plotROC)
library(psych)
library(Hmisc)
library(googlesheets)

# source("globals.R")

shinyServer(function(input, output){
  # output$menu <- renderMenu({
  #   sidebarMenu(
  #     menuItem("Menu Item", icon =icon("calendar"))
  #   )
  # })

  # Create reactive to read data
  data <- reactive({
    if (input$use_inbuilt_data){
      sAUC::fasd
    } else {
      input_file <- input$file
      if(is.null(input_file)){return()}
      read.table(
        file = input_file$datapath,
        sep = input$sep,
        header = input$header,
        stringsAsFactors = input$string_factors
      )
    }
  })

  observe({
    if(input$use_inbuilt_data ==TRUE){
    shinyjs::show("download_data")
    shinyjs::show("reset")
    shinyjs::show("show_model")
    shinyjs::show("describe_file")
    shinyjs::hide("box_inbuilt_data")
    shinyjs::show("reset_file")
    shinyjs::hide("add_about_sauc")
  } else if (input$use_inbuilt_data ==FALSE){
    shinyjs::hide("download_data")
    shinyjs::hide("reset")
    shinyjs::hide("show_model")
    shinyjs::hide("describe_file")
    shinyjs::show("box_inbuilt_data")
  }
  })

  observeEvent(input$file, {
    shinyjs::show("download_data")
    shinyjs::show("show_model")
    shinyjs::show("describe_file")
    shinyjs::show("reset")
    shinyjs::hide("box_inbuilt_data")
  })

  observeEvent(input$reset, {
    shinyjs::reset("file")
    shinyjs::hide("download_data")
    shinyjs::hide("show_model")
    shinyjs::hide("describe_file")
    shinyjs::alert("Thank you using sAUC method. Please provide feedback using Feedback Form on the left!")
    shinyjs::hide("reset")
    shinyjs::show("box_inbuilt_data")
    shinyjs::reset("use_inbuilt_data")
    shinyjs::hide("reset_file")
    shinyjs::show("add_about_sauc")
  })

  output$add_about_sauc <- renderUI({
    tags$iframe(src = 'README.html', # put .html to /www
                          width = '100%', height = '800px',
                          frameborder = 0, scrolling = 'auto')
  })

  #The following set of functions populate the column selectors
  output$choose_response <- renderUI({
    df <- data()
    if (is.null(df)) return(NULL)

    items=names(df)
    names(items)=items
    selectInput(
      inputId = "response",
      label = "Choose response*:",
      choices = items)
  })

  output$choose_group <- renderUI({
    df <- data()
    if (is.null(df)) return(NULL)

    items=names(df)
    names(items)=items
    selectInput(
      inputId = "group_var",
      label = "Choose group*:",
      choices  = names(data())[!names(data()) %in% input$response],
      selected = names(data())[!names(data()) %in% input$response][1])
  })

  output$independent <- renderUI({
  checkboxGroupInput(inputId = "independent",
                     label =  "Independent Variables*:",
                     choices = names(data())[!names(data()) %in% c(input$response, input$group_var)],
                     selected = names(data())[!names(data()) %in% c(input$response, input$group_var)][1])
  })

  # observe({
  #   if(length(output$independent))
  #   shinyjs::alert("You should have at least one discrete covariate in the model.")
  # })

  run_sAUC <- reactive({
    if (is.null(data())) return(NULL)
    ds <- data()
    cov_variables <- c(input$independent,input$group_var)
    ds[, cov_variables] <- lapply(ds[, cov_variables], function(x) factor(x))
    sAUC::sAUC(formula = as.formula(paste(input$response," ~ ",paste(input$independent,collapse="+"))),
               treatment_group = input$group_var, data = ds)

  })

  # observe({
  #   if (length(independent) == 0){
  #     shinyjs::alert("You should have at least one discrete covariate in the model.")
  #   }
  # })

  output$model_result <- DT::renderDataTable({
    mod_result <- run_sAUC()
    result_model <- DT::datatable(as.data.frame(mod_result$"Model summary"),
                caption = htmltools::tags$caption(
                  style = "font-size:120%",
                  strong('Model results'), '{Note: left-side of model is:', mod_result$"model_formula","}"),
                  options = list(pageLength = 6, dom = 'tip'), rownames = TRUE)
    return(result_model)
  })

  output$download_model_result = downloadHandler('sAUC-model-results.csv', content = function(file) {
    mod_result <- run_sAUC()
    dt_model_results <- as.data.frame(mod_result$"Model summary")
    write.csv(dt_model_results[, , drop = FALSE], file, row.names = TRUE)
  })

  #ROC curve
  output_roc_plot <- reactive({
    ds_roc <- data()
    auc_variables <- c(input$independent,input$group_var)
    ds_roc[, auc_variables] <- lapply(ds_roc[, auc_variables], function(x) factor(x))

    if (sum(length(input$independent)) == 1){
    rocplot1 <- ggplot(ds_roc, aes_string(d = input$group_var, m = input$response, color = input$independent)) +
      geom_roc(show.legend = FALSE) +
      geom_rocci()

    direct_label(rocplot1,  labels = unique(levels(factor(ds_roc[[input$independent]])))) + style_roc() +
      scale_x_continuous("1 - Specificity") +
      scale_y_continuous("Sensitivity") +
      ggtitle(paste("ROC curve for", input$independent)) +
      theme(plot.title = element_text(hjust = 0.5), text=element_text(size=17)) +
      annotate("text", x = 0.50, y = .1,
             label = paste("AUCs are: ", paste(round(calc_auc(rocplot1)$AUC, 2), collapse = " and ")))
    } else {
      rocplot2 <- ggplot(ds_roc, aes_string(d = input$group_var, m = input$response)) +
      geom_roc(show.legend = FALSE) +
      geom_rocci()

   direct_label(rocplot2,  labels = unique(levels(factor(ds_roc[[input$group_var]])))) + style_roc() +
      scale_x_continuous("1 - Specificity") +
      scale_y_continuous("Sensitivity") +
      ggtitle(paste("ROC curve for", input$group_var)) +
      theme(plot.title = element_text(hjust = 0.5), text=element_text(size=17)) +
      annotate("text", x = 0.50, y = .1,
             label = paste("AUC = ", paste(round(calc_auc(rocplot2)$AUC, 2))))
    }
  })

  output$roc_plot <- renderPlot({
    print(output_roc_plot())
  })

  output$download_roc_plot <- downloadHandler(
    filename = function() { paste("sAUC-ROC-curve", '.png', sep='') }, content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = output_roc_plot(), device = device)
})


  # Display orginal data
  output$show_input_file <- renderTable({
    if(is.null(data())){return()}
    input$file
  })

    # Display orginal data
  output$show_data <- DT::renderDataTable({
    if(is.null(data())){return()}
    datatable(data(), filter = 'top', options = list(
      pageLength = 8
    ))
  })

  # Display summary of the original data
  output$summaryy <- DT::renderDataTable({
    ds <- data()
    numeric_columns <- names(ds)[sapply(ds, function(x) is.numeric(x))]
    if(is.null(ds)){return()}
    summary_table <- as.data.frame(round(psych::describe(ds[numeric_columns])))
    names(summary_table) <- Hmisc::capitalize(names(summary_table))
    datatable(summary_table,
              caption = htmltools::tags$caption(
                style = "font-size:200%",
                htmltools::strong(paste("Table 1: Descriptive summary"))),
              options = list(pageLength = 6, dom = 'tip'), rownames = TRUE)
  })

  output_hist_plot <- reactive({
    ds_plot_response <- data()
    ggplot(data=ds_plot_response, aes_string(input$response)) +
    geom_histogram(bins = 10, fill = "blue") +
    labs(title = paste0("Histogram for ", input$response)) +
    theme(plot.title = element_text(hjust = 0.5), text=element_text(size=17)) +
    labs(x=input$response, y="Count") +
    theme(plot.title = element_text(hjust = 0.5))
  })

  output$hist_plot <- renderPlot({
    print(output_hist_plot())
  })

  output$download_hist_plot <- downloadHandler(
    filename = function() { paste("sAUC-histogram", '.png', sep='') },
    content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = output_hist_plot(), device = device)
  })

  output_bar_plot <- reactive({
    ds_read_cat <- data()
    cat_vars <- c(input$independent,input$group_var)
    ds_plot_cat <- ds_read_cat[,cat_vars]
    ds_plot_cat[, cat_vars] <- lapply(ds_plot_cat[, cat_vars], function(x) factor(x))

    ds_cat <- ds_plot_cat %>% tidyr::gather(cat_variables, value)
    re_from <- "\\b([[:lower:]])([[:lower:]]+)"
    ds_cat$cat_variables <- gsub(re_from, "\\U\\1\\L\\2" ,ds_cat$cat_variables, perl=TRUE)

    cat_plot <- ggplot(ds_cat,aes(x = value)) +
      facet_wrap(~ cat_variables, scales = "free_x") +
      geom_bar(stat ="count", fill = "#990033")  +
      ggtitle("Barplots for discrete variables") +
      theme(plot.title = element_text(hjust = 0.5), text=element_text(size=17)) +
      xlab("Covariates") + geom_text(stat='count', aes(label = ..count..), vjust = -1)
      ylab("Frequency")
      cat_plot
  })

  output$bar_plot <- renderPlot({
    print(output_bar_plot())
  })

  output$download_bar_plot <- downloadHandler(
    filename = function() { paste("sAUC-bar-plot", '.png', sep='') },
    content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = output_bar_plot(), device = device)
  })

  output$describe_file <- renderUI({
    if (is.null(data())){
      h3("Data are not uploaded yet. Please do so now if you'd like to run Semiparametric AUC Regression model.", style = "color:red")
    } else {
      tabsetPanel(
        tabPanel(
          title = "Data",
          dataTableOutput("show_data")),
        tabPanel(
          title = "Summary",
          dataTableOutput("summaryy")),
        tabPanel(
          title = "Plots",
          column(
            width = 4,
            plotOutput("hist_plot"),
            p(class = 'text-center', downloadButton('download_hist_plot', 'Download Histogram'))
            ),
          column(
            width = 8,
            plotOutput("bar_plot"),
            p(class = 'text-center', downloadButton('download_bar_plot', 'Download Bar Plot'))
          )
        ),
        tabPanel(
          title = "About file",
          tableOutput("show_input_file"))
        )
    }
  })

  result_of_simulate <- reactive({
    iter <- input$realization
    m <- input$number_treatment
    p <- input$number_control
    b0 <- input$b0
    b1 <- input$b1
    b2 <- input$b2
    sAUC::simulate_one_predictor(iter = iter, m = m, p = p, b0 = b0, b1 = b1, b2 = b2)
  })

  simulation_results <- reactive({
    result_simulate <- result_of_simulate()
    df <- (as.data.frame(cbind(result_simulate$meanbeta, result_simulate$meanvar, result_simulate$meansd,
                               result_simulate$ci_betass, result_simulate$all_coverage, result_simulate$iter)))
    names(df) <- c("Beta Estimates", "Variance of Beta", "S.E. of Beta","Confidence Interval on Beta", "Coverage Probability", "Iterations")
    df
  })

  output$result1 <- DT::renderDataTable({
    df_results <- simulation_results()
    dt <- DT::datatable(
      df_results,
      caption = htmltools::tags$caption(
        style = "font-size:150%",
        'Table 1. Results of the Simulation on sAUC with one discrete covariate'),
      options = list(pageLength = 6, dom = 'tip'), rownames = c("\u03b20", "\u03b21", "\u03b22"))
    return(dt)
  })

  output$download_simu_result = downloadHandler('sAUC-simulation-results.csv', content = function(file) {
    simu_result <- simulation_results()
    write.csv(simu_result[, , drop = FALSE], file, row.names = c("B0", "B1", "B2"))
  })

  output_plot_beta <- reactive({
    simulated_betas <- result_of_simulate()
    dddd <- as.data.frame(simulated_betas$m_betas)
    data_long <- gather(dddd, Parameter, values, factor_key=TRUE)
    data_long$Parameter <- with(data_long, ifelse(Parameter == "V1","0",
                                                  ifelse(Parameter =="V2","1", "2")))
    mu <- data_long %>%
      dplyr::group_by(Parameter) %>%
      dplyr::summarize(mean_beta = mean(values)) %>% as.data.frame()

    # Create normal curve to overlay to plot
    # calculate mean and sd by group
    stats <- aggregate(values~Parameter, data_long, function(x) c(mean=mean(x), sd=sd(x)))
    stats <- data.frame(Parameter=stats[,1],stats[,2])
    x <- with(data_long, seq(min(values), max(values), len=100))
    dfn <- do.call(rbind,lapply(1:nrow(stats),
                                function(i) with(stats[i,],data.frame(Parameter, x, y=dnorm(x,mean=mean,sd=sd)))))

    # Change colors by groups
    ggplot(data_long, aes(x=values, color=Parameter, fill=Parameter)) +
      geom_histogram(aes(y=..density..), position="identity", alpha=0.7, bins = 50) +
      geom_density(alpha=0.6, size = 0.9, adjust = 0.6) +
      facet_grid(.~Parameter, labeller = label_bquote(cols = beta[.(Parameter)])) +
      geom_vline(data=mu, aes(xintercept=mean_beta, color=Parameter),linetype="dashed") +
      scale_color_manual(values=c("blue", "red", "maroon")) +
      # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
      labs(x="Beta Estimates", y = "Density") +
      theme_classic() +
      theme(text = element_text(size=20)) +
      theme(legend.position="none") +
      geom_line(data=dfn, aes(x, y), alpha = 0.3, size= 1.2, colour = "black")
  })

  output$result_plot_beta <- renderPlot({
    print(output_plot_beta())
  })

  output$download_simu_plot <- downloadHandler(
    filename = function() { paste("sAUC-simulation-plot", '.png', sep='') },
    content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 600, units = "in")
        ggsave(file, plot = output_plot_beta(), device = device)
  })

  # output$download_cv <- downloadHandler(
  #   filename = "som-bohora-cv.pdf",
  #   content = function(file) {
  #     file.copy("www/bohora-cv.pdf", file)
  #   })

   output$download_data <- downloadHandler(
    filename = function() { paste('fasd', '.csv', sep='') },
    content = function(file) {
      write.csv(data(), file)
    })

   ## ======================
   googleform_embed_link <- "https://docs.google.com/forms/d/e/1FAIpQLSeK0vk4Crgaej5yYG4rjKFzvrN8IIL94SsCMDnHihvqdaqu2g/viewform?usp=sf_link#start=embed"
   # googleform_data_url <- "https://docs.google.com/spreadsheets/d/1QsCeoKtTPUf8FZ_UH3vhMSAhwX4XfyqG5eTasuPn3cs/pubhtml"
   # ## ======================
   #
   # ss <- gs_url(googleform_data_url, lookup = FALSE, visibility = "public")

   output$googleForm <- renderUI({
     tags$iframe(
       id = "googleform",
       src = googleform_embed_link,
       width = 525,
       height = 700,
       frameborder = 0,
       marginheight = 0)
   })

   output$googleFormData <- DT::renderDataTable({
     input$refresh
     ss_dat <- gs_read(ss) %>%
       mutate(Timestamp = Timestamp %>%
                as.POSIXct(format = "%m/%d/%Y %H:%M:%S", tz = "PST8PDT")) %>%
       select(-2,-3,-4) %>%
       arrange(desc(Timestamp))

     DT::datatable(ss_dat, rownames = F)
   })
})
