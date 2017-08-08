library(shiny)
library(shinydashboard)
library(shinyjs)

header <- dashboardHeader(
  title = tags$a(tags$p(strong(style ="font-size: 24px;color: white","Semiparametric Area Under the Curve (sAUC) Regression Model with Discrete Covariates by Som Bohora")), href = "https://github.com/sbohora/sAUC", target ="_blank"),
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
      time = "2017-08-01")),
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
             color = "aqua","Documention"),
    taskItem(value = 75, color = "yellow","Server deployment")
    ))

# Define the overall UI
dashboardPage(
  # skin = "green",
  header = header,
  title = "sAUC",
  dashboardSidebar(
    disable = FALSE,
    sidebarMenu(
      id = "sidebar_menu",
      # menuItem(
        # text = "Introduction",
        # tabName = "intro",
        # icon = icon("book")),
      menuItem(
        text = "AUC Regression",
        tabName = "auc_reg",
        icon = icon("bar-chart")),
      menuItem(
        text = "Example",
        tabName = "example",
        icon = icon("book")),
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
          tabName = "ui_code"),
        menuSubItem(
          text = "globals.R",
          tabName = "global_code")
          ),
      menuItem(
        text = "sAUC in Python",
        tabName = "sauc_python",
        icon = icon("file-code-o")),
      menuItem(
        text = "sAUC in Julia",
        tabName = "sauc_julia",
        icon = icon("code-fork")),
      menuItem(
        text = "About Me",
        tabName = "about_me",
        icon = icon("picture-o")),
      menuItem(
        text = "Feedback",
        tabName = "feedback",
        icon = icon("comment-o")),
      menuItem(
        text = "Discussion Board",
        tabName = "disqus_here",
        icon = icon("info")),
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
        href = "https://sbohora.github.io/sAUC",
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
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tags$style(type="text/css",
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
  ),
    tabItems(
      tabItem(
        tabName = "auc_simulate",
        fluidRow(
          column(
            width = 3,
            box(
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "info",
            title = tags$p(strong(style ="font-size: 18px;","Please enter following parameters for simulation")),
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
              background = NULL,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "info",
              p("Below are the true parameter values for the current simulation. You can change 'em if you want."),
              numericInput(
                inputId = "b0",
                label = HTML("True value of &beta;<sub>0</sub>:"),
                value = 0.15),
              numericInput(
                inputId = "b1",
                label = HTML("True value of &beta;<sub>1</sub>:"),
                value = 0.50),
              numericInput(
                inputId = "b2",
                label = HTML("True value of &beta;<sub>2</sub>:"),
                value = 1.00)
            )
          ),
          column(
            width = 6,
            box(
            title = tags$p(strong(style ="font-size: 18px;","Result of Simulation")),
            width = 15,
            div(style = 'overflow-x: scroll', DT::dataTableOutput('result1')),
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "info",
            background = NULL),
            p(class = 'text-center', downloadButton('download_simu_result', 'Download Simulation Results'))
        )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              title = "Distribution of beta estimates from the above simulation",
              width = 15,
              div(style = 'overflow-x: scroll', plotOutput('result_plot_beta')),
              background = "red",
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "info"),
              p(class = 'text-center', downloadButton('download_simu_plot', 'Download Simulation Plot'))
          )
        )
      ),
      tabItem(
        tabName = "auc_reg",
         fluidRow(
          column(
            width = 3,
        box(
          title="File upload",
          width = 12,
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "info",
          fileInput("file", "Upload a data file to be analyzed"),
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
          ),
        shinyjs::hidden(
          downloadButton('download_data', 'Download Data'),
          actionButton("reset", "Reset Analysis", style = "color: white;
                     background-color: blue;")
        )),

        column(
          width = 9,
        box(
          width = 20,
          height = 20,
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "info",
          uiOutput("describe_file")
        )
        )
         ),
        br(),
        br(),
        shinyjs::hidden(
        fluidRow(
          id = "show_model",
          column(
            width = 3,
        box(
          title = tags$p(strong(style ="font-size: 18px;color: green","Data analysis")),
          width = 12,
          ollapsible = TRUE,
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "warning",
          uiOutput("choose_response"),
          uiOutput("choose_group"),
          uiOutput("independent"),
          textOutput("input_oupt")
          )
        ),
        column(
            width = 5,
        box(
          title = tags$p(strong(style ="font-size: 18px;color: green","Model Results")),
          width = 14,
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "warning",
          dataTableOutput("model_result")
          ),
        p(class = 'text-center', downloadButton('download_model_result', 'Download Model Results')),
        p(class = 'text-right', downloadButton('download_roc_plot', 'Download ROC curve plot'))
        ),
        column(
          width = 4,
        box(
          # title = tags$p(strong(style ="font-size: 18px;color: green","ROC curve")),
          width = 14,
          height = 5,
          # collapsible = TRUE,
          # solidHeader = TRUE,
          # status = "warning",
          plotOutput("roc_plot", height = 350, width = 500)
        )
        )
        )
      )),
      tabItem(
        tabName = "example",
        fluidRow(
          column(
            width = 9,
            box(
              width = 20,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "info",
              title = "Example using Semiparametric AUC regression method",
              background = NULL,
              # includeMarkdown("example.md")
              # includeHTML("server.html")
              # HTML(markdown::markdownToHTML(knitr::knit('server.Rmd', quiet = FALSE)))
              # includeHTML("example.html")
              tags$iframe(src = 'example.html', # put .html to /www
              width = '100%', height = '1000px',
              frameborder = 0, scrolling = 'auto')
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
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "info",
              background = NULL,
              # includeMarkdown("server.md")
              tags$iframe(src = 'server.html', # put .html to /www
              width = '100%', height = '1000px',
              frameborder = 0, scrolling = 'auto')
              # includeHTML("example.html")
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
        tabName = "ui_code",
        fluidRow(
          column(
            width = 8,
            box(
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "info",
              background = NULL,
              # includeMarkdown("ui.md")
              tags$iframe(src = 'ui.html', # put .html to /www
              width = '100%', height = '1000px',
              frameborder = 0, scrolling = 'auto')
              # includeHTML("example.html")
            )
          )
        )
      ),
      tabItem(
        tabName = "global_code",
        fluidRow(
          column(
            width = 8,
            box(
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "info",
              background = NULL,
              # includeMarkdown("ui.md")
              tags$iframe(src = 'globals.html', # put .html to /www
                          width = '100%', height = '1000px',
                          frameborder = 0, scrolling = 'auto')
              # includeHTML("example.html")
            )
          )
        )
      ),
      tabItem(
        tabName = "sauc_python",
        fluidRow(
          column(
            width = 9,
            box(
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "info",
              background = NULL,
              # includeMarkdown("server.md")
              tags$iframe(src = 'example-python.html', # put .html to /www
                          width = '100%', height = '1000px',
                          frameborder = 0, scrolling = 'auto')
            )
          )
        )
      ),
      tabItem(
        tabName = "sauc_julia",
        fluidRow(
          column(
            width = 9,
            box(
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "info",
              background = NULL,
              # includeMarkdown("server.md")
              tags$iframe(src = 'example-julia.html', # put .html to /www
                          width = '100%', height = '1000px',
                          frameborder = 0, scrolling = 'auto')
            )
          )
        )
      ),
      tabItem(
        tabName = "about_me",
        fluidRow(
          column(
            width = 5,
            box(
              # title = "This is about me",
              width = 12,
              height = 15,
              # status = "primary",
              # background = "navy",
              # collapsible = TRUE,
              # solidHeader = TRUE,
              # p("This is about me.", style = "font-size:120%"),
              tags$a(href = "http://ouhsc.edu/bbmc/team/", img(src = "som.jpg", height = 750, width = 640), target ="_blank")
              , br()
              , br()
              )
              ),
          column(
            width = 7,
            box(
              title = "This is about me",
              width = 12,
              status = "primary",
              background = "navy",
              collapsible = TRUE,
              solidHeader = TRUE,
              # p("This is about me.", style = "font-size:120%"),
              # tags$a(href = "http://ouhsc.edu/bbmc/team/", img(src = "som.jpg", height = 200, width = 200), target ="_blank")
              br(),
              br(),
              span("I am a Research Biostatistician at ",
                   span(tags$a(href = "http://ouhsc.edu/bbmc/team/", "The Department of Pediatrics, The University of Oklahoma Health Sciences Center. ", target ="_blank"),
                        style = "color:blue"),
              "I received my MApStat and MS in Biostatistics from LSU and OUHSC, respectively. In addition to BBMC, I work as a statistician and data programmer
              in a number of pediatric research projects. I was trained in biostatistics and epidemiology, and has research experience in Fetal Alcohol Spectrum
              Disorders (FASD), HIV/AIDS clinical trials and child maltreatment prevention. I am interested in the applications of statistical computing and simulation,
              data analytics, dynamic reporting, and real-time data decision making. I use mainly R, python, and Julia programming languages.", style = "font-size:120%"),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              downloadLink("download_cv", p("Download my CV", style = "font-size:130%"))
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
              width = 20,
              height = 20,
              title = "Feedback Form",
              status = "info",
              background = NULL,
              collapsible = TRUE,
              solidHeader = TRUE,
              htmlOutput("googleForm")
              )
          ),
          column(
            width = 4,
            box(
              width = 20,
              height = 20,
              title = "Selected Comments",
              status = "info",
              background = NULL,
              collapsible = TRUE,
              solidHeader = TRUE,
              div(style = 'overflow-x: scroll', DT::dataTableOutput('googleFormData')),
              actionButton("refresh", "Refresh Sheet")
            )
          ),
          column(
            width = 4,
            box(
              width = 12,
              title = "Contact me",
              status = "info",
              background = NULL,
              # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
              collapsible = TRUE,
              solidHeader = TRUE,
              span("I can be contacted at",
                   a("energeticsom@gmail.com", href = "mailto:energeticsom@gmail.com"),
                   br(),
                   br(),
                   p("Please report any bugs about the sAUC (R package)", span(tags$a(href = "https://github.com/sbohora/sAUC", "here",
                                                                          target = "_blank"))),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   p("Please report any bugs about the saucpy (python package)", span(tags$a(href = "https://github.com/sbohora/saucpy/", "here",
                                                                                      target = "_blank"))),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   p("Please report any bugs about the SemiparametricAUC.jl (Julia package)", span(tags$a(href = "https://github.com/sbohora/SemiparametricAUC.jl/", "here",
                                                                                      target = "_blank"))),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   em(
                     span("Created by "),
                     a("Som B. Bohora", href = "mailto:energeticsom@gmail.com"),
                     span(", July 2017"),
                     br(),
                     br()
                   ), style = "font-size:140%; color:green")
            )
          )
        )
      ),
      tabItem(
        tabName = "disqus_here",
        strong(h3("This is a discussion board about sAUC", style = "color:blue", align="center")),
        div(
          id = "disqus_thread",
         HTML(
          "<script>
              (function() {
                  var d = document, s = d.createElement('script');

                  s.src = '//sauc.disqus.com/embed.js';

                  s.setAttribute('data-timestamp', +new Date());
                  (d.head || d.body).appendChild(s);
              })();
          </script>
          <noscript>Please enable JavaScript to view the <a href='https://disqus.com/?ref_noscript' rel='nofollow'>comments powered by Disqus.</a></noscript>"
         )
        )
      )
    ),
  div(HTML('<script id="dsq-count-scr" src="//sauc.disqus.com/count.js" async></script>'))
  ) #End the dashboardBody

) #End the dashboardPage,


