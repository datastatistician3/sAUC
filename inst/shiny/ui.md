


Below is the `ui.R` code

```r
library(shiny)
library(shinydashboard)
path_files <- "C:/Users/sbohora/Documents/GitHub/sAUC/R/"
# lapply(list.files(path = file.path(path_files), pattern = "[.]R$", recursive = TRUE), function(x) source(paste0(path_files,"/",x)))

header <- dashboardHeader(
  title = tags$p(strong(style ="font-size: 24px;color: white","Semiparametric Area Under the Curve (sAUC) Regression Model with Discrete Covariates by Som Bohora")),
  disable = FALSE,
  titleWidth = "1500px",
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
  title = "sAUC",
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
        tabName = "auc_simulate",
        fluidRow(
          column(
            width = 3,
            box(
            title = tags$p(strong(style ="font-size: 18px;","Please enter following parameters for simulation")), status = "primary",
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
              title = tags$p(strong(style ="font-size: 18px;","True regression parameters")),
              width = NULL,
              status = "primary",
              collapsible = TRUE,
              background = NULL,
              p("Below are the true parameter values for the current simulation. You can change 'em if you want."),
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
            title = tags$p(strong(style ="font-size: 18px;","Result of Simulation")),
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
              collapsible = TRUE, background = "red")
          )

        )
      ),
      tabItem(
        tabName = "auc_reg",
         fluidRow(
          column(
            width = 3,
        box(#title="Input file",
            width = 12,
            status = "success",
            fileInput("file", "Upload the file to be analyzed"),
            helpText("Default maximum file size is 5MB"),
            tags$hr(),
            h5(helpText("Select the read.table parameters below")),
            checkboxInput(
              inputId = 'header', label = "Header TRUE?", value = TRUE),
            checkboxInput(
              inputId = 'string_factors', label = "String as Factors?", value = FALSE),
            radioButtons(
              inputId = 'sep',
              label = 'Separator',
              choices = c(Comma = ",", Semicolon = ';', Tab = '\t', Space=''), selected = ',')
          )),
        column(
          width = 9,
        box(
          width = 20,
          uiOutput("describe_file")
        )
        )
         ),
        fluidRow(
          column(
            width = 3,
        box(
          title = tags$p(strong(style ="font-size: 18px;color: green","Data analysis")),
          width = 12,
          uiOutput("choose_response"),
          uiOutput("choose_group"),
          uiOutput("independent"),
          textOutput("input_oupt")
          )
        ),
        column(
            width = 6,
        box(
          title = tags$p(strong(style ="font-size: 18px;color: green","Model Results")),
          width = 20,
          dataTableOutput("model_result")
          )
        )
      )
      ),
      tabItem(
        tabName = "server_code",
        fluidRow(
          column(
            width = 8,
            box(
              width = 12,
              # uiOutput("markdown")
              includeMarkdown("server.md")
              # includeHTML("server.html")
              # HTML(markdown::markdownToHTML(knitr::knit('server.Rmd', quiet = FALSE)))
              # tags$iframe(
              #   seamless = "seamless",
              #   style="height:400px; width:100%; scrolling=yes",
              #   src = "https://websitesetup.org/css3-cheat-sheet/")
            )
          )
        )
      ),
      tabItem(
        tabName = "about_me",
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,
              status = "primary",
              collapsible = TRUE,
              p("This is about me."),
              tags$a(href = "http://ouhsc.edu/bbmc/team/", img(src = "som.jpg", height = 200, width = 200))
              , br()
              , br(),
              span("I am a Research Biostatistician at ",
                   span(tags$a(href = "http://ouhsc.edu/bbmc/team/", "The Department of Pediatrics, The University of Oklahoma Health Sciences Center. "),
                        style = "color:blue"),
              "I received my MApStat and MS in Biostatistics from LSU and OUHSC, respectively. In addition to BBMC, I work as a statistician and data programmer
              in a number of pediatric research projects. I was trained in biostatistics and epidemiology, and has research experience in Fetal Alcohol Spectrum
              Disorders (FASD), HIV/AIDS clinical trials and child maltreatment prevention. I am interested in the applications of statistical computing and simulation,
              data analytics, dynamic reporting, and real-time data decision making. I use mainly R, python, and Julia programming languages.", style = "font-size:120%")
            )
          )
        )
      ),
      tabItem(
        tabName = "feedback",
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,
              status = "primary",
              collapsible = TRUE,
              solidHeader = TRUE,
              span("I can be contacted at",
              a("energeticsom@gmail.com", href = "mailto:energeticsom@gmail.com"),
              br(),
              br(),
              p("Please report any bugs about the sAUC", span(tags$a(href = "https://github.com/sbohora/sAUC", "here"))),
              br(),
              shiny::hr(),
              em(
                span("Created by "),
                a("Som B. Bohora", href = "mailto:energeticsom@gmail.com"),
                span(", July 2017"),
                br(),
                br()
                ), style = "font-size:140%")
            )
          )
        )
      )
    )

  ) #End the dashboardBody

) #End the dashboardPage,
```