library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "sAUC Model",
  disable = FALSE,
  dropdownMenu(type = "messages",
               messageItem(from = "Sales Dept",
                           message = "Sales are steady this month."),
               messageItem(from = "New User",
                           message = "How do I register?",
                           icon = icon("question"),
                           time = "13:45"),
               messageItem(from = "Support",
                           message = "The new server is ready.",
                           icon = icon("life-ring"),
                           time = "2014-12-01")),
  dropdownMenu(type = "notifications",
               notificationItem(text = "5 new uers today",
                                icon = icon("users")),
               notificationItem(
                 text = "12 items delivered",
                 icon("truck"),
                 status = "success"),
               notificationItem(
                 text = "Server load at 86%",
                 icon = icon("exclamation-triangle"),
                 status = "warning")),
  dropdownMenu(type = "task",
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
    uiOutput("auc"),
    disable = FALSE,
    sidebarMenu(
      menuItem("AUC Regression",
               tabName = "auc_reg",
               icon = icon("bar-chart")),
      menuItem("AUC Simulation",
               tabName = "auc_simulate",
               icon = icon("line-chart"),
               badgeLabel = "Perform",
               badgeColor = "green"),
      sidebarMenuOutput("menu"),
      sidebarSearchForm(
        textId = "searchText",
        buttonId = "searchButton",
        label = "Search..."
      ),
      menuItem("Obtain Codes",
               tabName = "get_code",
               icon = icon("code")),
      menuItem("About Me",
               tabName = "about_me",
               icon = icon("picture-o")),
      menuItem("Feedback",
               tabName = "feedback",
               icon = icon("comment-o")),
      tags$hr(),
      menuItem(
        text = "",
        href = "https://mytinyshinys.shinyapps.io/dashboard",
        badgeLabel = "All Dashboards"
      ),
      tags$hr(),

      tags$body(
        a(
          class = "addpad",
          href = "https://twitter.com/SomBohora",
          target = "_blank",
          img(src = "./inst/shiny/www/twitter-logo.png")
        ),
        a(
          class = "addpad2",
          href = "mailto:energeticsom@gmail.com",
          img(src = "./inst/shiny/www/email-logo.png")
        ),
        a(
          class = "addpad2",
          href = "https://github.com/sbohora",
          target = "_blank",
          img(src = "./inst/shiny/www/github-logo.png")
        )
      )
    )

  ),
  dashboardBody(
    tabItems(
      tabItem(
        "auc_reg"
      )
    )

  ) #End the dashboardBody

) #End the dashboardPage

