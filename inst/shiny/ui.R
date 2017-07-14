library(shiny)
library(shinydashboard)
path_files <- "C:/Users/sbohora/Documents/GitHub/sAUC/R/"
lapply(list.files(path = file.path(path_files), pattern = "[.]R$", recursive = TRUE), function(x) source(paste0(path_files,"/",x)))

header <- dashboardHeader(
  title = "Semiparametric Area Under the Curve (sAUC) Regression Model with Discrete Covariates by Som Bohora",
  disable = FALSE,
  titleWidth = "1400px",
  dropdownMenu(
    type = "messages",
    messageItem(
      from = "Som",
      message = "Stay tuned."),
    messageItem(
      from = "New User",
      message = "How do I register?",
      icon = icon("question"),
      time = "13:45"),
    messageItem(
      from = "Support",
      message = "The new server is ready.",
      icon = icon("life-ring"),
      time = "2014-12-01")),
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "5 new uers today",
      icon = icon("users")),
    notificationItem(
      text = "12 items delivered",
      icon("truck"),
      status = "success"),
    notificationItem(
      text = "Server load at 86%",
      icon = icon("exclamation-triangle"),
      status = "warning")),
  dropdownMenu(
    type = "task",
    badgeStatus = "success",
    taskItem(value = 90,
             color = "green","Documention"),
    taskItem(value = 17, color = "aqua","Project X"),
    taskItem(value = 75, color = "yellow","Server deployment"),
    taskItem(value = 80, color = "red","Overall project")))

# Define the overall UI
dashboardPage(
  skin = "green",
  header = header,
  dashboardSidebar(
    disable = FALSE,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem(
        text = "AUC Regression",
        tabName = "auc_reg",
        icon = icon("bar-chart")),
      menuItem(
        text = "AUC Simulation",
        tabName = "auc_simulate",
        icon = icon("line-chart"),
        badgeLabel = "Perform",
        badgeColor = "green"),

      menuItem(
        text = "Source Code",
        tabName = "get_code",
        # href = "https://github.com/sbohora/sAUC/tree/master/inst/shiny",
        icon = icon("code"),
        menuSubItem(
          text = "server.R",
          tabName = "server_code"),
        menuSubItem(
          text = "ui.R",
          tabName = "ui_code")
          ),
      menuItem(
        text = "About Me",
        tabName = "about_me",
        icon = icon("picture-o")),
      menuItem(
        text = "Feedback",
        tabName = "feedback",
        icon = icon("comment-o")),
      sidebarMenuOutput("menu"),
      tags$hr(),
      menuItem(
        text = "",
        href = "https://github.com/sbohora/sAUC",
        badgeLabel = "Github Repo for sAUC package",
        icon = icon("github")
      ),
      menuItem(
        text = "",
        href = "https://github.com/sbohora/sAUC",
        badgeLabel = "Web site for sAUC package",
        icon = icon("tv")
      ),
      tags$hr(),
     menuItem(
       text = "Contact Me",
       tabName = "contact",
       icon = icon("volume-control-phone")),
     tags$body(
       a(
         class = "addtwitter",
         href = "https://twitter.com/SomBohora",
         target = "_blank",
         img(src = "twitter-logo.png", height="30", width="30")
         ),
       a(
         class = "addemail",
         href = "mailto:energeticsom@gmail.com",
         img(src = "email-logo.png", height="30", width="30")
        ),
       a(
         class = "addgithub",
         href = "https://github.com/sbohora",
         target = "_blank",
         img(src = "github-logo.png", height="30", width="30")
        )
      ),
     sidebarSearchForm(
        textId = "searchText",
        buttonId = "searchButton",
        label = "Search..."
      )
    )

  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "auc_reg",
        fluidRow(
          column(
            width = 3,
            box(
            title = "Please enter following parameters for simulation", status = "primary",
            width = 12L,
            height = 8L,
            background = NULL,
            numericInput(
              inputId = "realization",
              label = "Number of Realizations",
              value = 100),
            sliderInput(
              inputId = "number_treatment",
              label = "Number of Observations in Treatment Group: ", 1, 1000, 20),
            sliderInput(
              inputId = "number_control",
              label = "Number of Observations in Control Group: ", 1, 1000, 30)

            )

            ),
          column(
            width = 3,
            box(
              title = "True regression parameters",
              width = NULL,
              status = "primary",
              collapsible = TRUE,
              background = NULL,

              p("Below are the true parameter values for the current simulation. Please change if you'd like"),
              numericInput(
                inputId = "b0",
                label = "True value of Beta 0: ",
                value = 0.15),
              numericInput(
                inputId = "b1",
                label = "True value of Beta 1: ",
                value = 0.50),
              numericInput(
                inputId = "b2",
                label = "True value of Beta 2: ",
                value = 1.00)
            )
          ),
          column(
            width = 6,
            box(
            title = "Result of Simulation",
            width = 15,
            status = "primary",
            div(style = 'overflow-x: scroll', DT::dataTableOutput('result1')),
            collapsible = TRUE,
            background = NULL)
        )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              title = "Distribution of beta estimates from the above simulation",
              width = 15,
              status = "primary",
              div(style = 'overflow-x: scroll', plotOutput('result_plot_beta')),
              collapsible = TRUE, background = "maroon")
          )

        )
      )
    )

  ) #End the dashboardBody

) #End the dashboardPage,

