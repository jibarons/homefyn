

options(shiny.error = browser)
options(shiny.error = recover)
# Define UI
ui <- secure_app(head_auth = tags$script(inactivity),
fluidPage(
  theme = shinytheme("united"),
  # Define HTML tags
  tags$head(
    tags$style(HTML("
      hr {border-top: 1px solid #000000; color: gray}
      h1 {font-weight:bold}
      h2 {font-weight:bold}
    "))
  ),

  titlePanel(
    tags$h1(
      "My Home Finances Tracker", icon("coins"),
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
  navbarPage("MyHomeFin",

    tabPanel("Home",
      # Summary outputs
      mainPanel(width = 12,
        fluidRow(
          column(3,
            tags$h1(icon("hand-holding-dollar"), "Income"),
            tags$h2(textOutput("income"), style = "color:#00cd00")
          ),
          column(3,
            tags$h1(icon("money-bill-transfer"), "Expense"),
            tags$h2(textOutput("expense"), style = "color:#cd9b1d")
          ),
          column(3,
            tags$h1(icon("file-invoice-dollar"), "Loan"),
            tags$h2(textOutput("loan"), style = "color:#cd0000")
          ),
          column(3,
            tags$h1(icon("scale-balanced"), "Life balance:"),
            tags$h2(textOutput("balance"), style = "color:#4f94cd")
          ),
        ),
        fluidRow(column(width = 12, tags$h1(textOutput("empty_row")))),

        fluidRow(
          hr(),
          mainPanel(width = 6,
            fluidRow(
              # Expenses expiring
              column(4, tags$h2(icon("arrow-trend-up"), "Expiring expenses:")),
              column(4, tags$h3(htmlOutput("exp_expire3"),
                style = "color:#228b22; font-weight:normal")),
              column(4, tags$h3(htmlOutput("exp_expire6"),
                style = "color:#228b22; font-weight:normal")),
            ),
            fluidRow(
              # Income expiring 
              column(4, tags$h2(icon("arrow-trend-down"), "Expiring income:")),
              column(4, tags$h3(htmlOutput("inc_expire3"),
                style = "color:#8f0101; font-weight:normal")),
              column(4, tags$h3(htmlOutput("inc_expire6"),
                style = "color:#8f0101; font-weight:normal")),
            )
          ),
          mainPanel(width = 6,
            column(12, tableOutput("asigns"),  align = "center")
          )
        ),
        fluidRow(
          hr(), materialSwitch(
          inputId = "agg_expenses", label = "Aggregate expenses",
          value = TRUE
          )
        ),
        fluidRow(plotlyOutput("fin_time")),
        fluidRow(
          column(7, plotlyOutput("waterp")),
          column(5, plotlyOutput("stackp"))
        )
      )
    ),
    tabPanel("Details",
      fluidRow(
        column(6, tableOutput("expire_detail")),
        column(6, tableOutput("income_detail"))
      ),
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