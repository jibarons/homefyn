

options(shiny.error = browser)
options(shiny.error = recover)
# Define UI
ui <- secure_app(head_auth = tags$script(inactivity),
fluidPage(
  theme = shinytheme("united"),
  titlePanel(
    tags$h1(
      "My Home Finances", icon("coins"),
      style = "display: inline-block; background-color: #ea5421; width: 100%; 
      height: 50px;text-align: center; color:white; font-weight:bold"
    )
  ),
  fluidRow(
    column(4, tags$b(textOutput("res_auth"))),
    column(2, offset = 6, radioGroupButtons(
      inputId = "in_currency", #label = list(icon("coins"), strong("Currency")),
      choices = unique(db_fin$currency), selected = "php",
      direction = "horizontal", size = "xs"
    )),
  ),
  navbarPage("HomeFyn",

    tabPanel("Home",
      # Summary outputs
      mainPanel(width = 12,
        fluidRow(
          column(3,
            tags$h2(icon("hand-holding-dollar"), "Income", style = "font-weight:bold"),
            tags$h2(textOutput("income"), style = "color:#00cd00")
          ),
          column(3,
            tags$h2(icon("money-bill-transfer"), "Expense", style = "font-weight:bold"),
            tags$h2(textOutput("expense"), style = "color:#cd9b1d")
          ),
          column(3,
            tags$h2(icon("file-invoice-dollar"), "Loan", style = "font-weight:bold"),
            tags$h2(textOutput("loan"), style = "color:#cd0000")
          ),
          column(3,
            tags$h2(icon("scale-balanced"), "Life balance:", style = "font-weight:bold"),
            tags$h2(textOutput("balance"), style = "color:#4f94cd")
          ),
        ),
        fluidRow(
          mainPanel(width = 9,
            column(3, tableOutput("asigns")),
          ),
          mainPanel(width = 3,
            fluidRow(tags$h2(icon("money-bill-trend-up"), "Expires soon:",
                style = "font-weight:bold")),
            fluidRow(tags$h3(textOutput("expire3"), style = "color:#228b22")),
            fluidRow(tags$h3(textOutput("expire6"), style = "color:#228b22")),
          )
        ),
        fluidRow(
          column(7, plotlyOutput("waterp")),
          column(5, plotlyOutput("stackp"))
        )
      )
    ),
    tabPanel("Details",
      fluidRow(column(6, tableOutput("expire_detail"))),
      fluidRow(plotlyOutput("waterp_detail"))
    ),
    tabPanel("Finances Data",
      fluidRow(
        mainPanel(width = 5,
          fluidRow(
            column(12, selectInput(
              inputId = "in_category", label = strong("Finance category"),
              choices = unique(db_fin$category), selected = unique(db_fin$category),
              multiple = TRUE
            )),
          ),
          fluidRow(
            column(6, selectInput(
              inputId = "in_currency_db", label = strong("Currency"),
              choices = unique(db_fin$currency), selected = "php", 
              multiple = TRUE
            )),
            column(6, selectInput(
              inputId = "in_active", label = strong("Status"),
              choices = unique(db_fin$active), selected = "active", 
              multiple = TRUE
            ))
            #column(3, dateRangeInput(inputId = "daterange"))
          ),
        ),
        mainPanel(width = 7,
          fluidRow(
            column(6, multiInput(
              inputId = "in_group", label = strong("Finance group"),
              choices = sort(unique(db_fin$group)),
              selected = unique(db_fin$group),
              options = list(
                enable_search = FALSE, 
                non_selected_header = "Choose:",
                selected_header = "Selected:"
              )
            )),
            column(6, multiInput(
              inputId = "in_bank", label = strong("Income assigment"),
              choices = sort(unique(db_fin$bank_asign)),
              selected = unique(db_fin$bank_asign),
              options = list(
                enable_search = FALSE, 
                non_selected_header = "Choose:",
                selected_header = "Selected:"
              )
            ))
          )
        )
      ),
      fluidRow(column(12, DT::DTOutput("data_fin")))
    ),
  )
))