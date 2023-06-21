# homefyn

A small Shiny app gain insight on your your home finances. How much I am speding on debts? How much in day to day expenses? I am earning enough? What will happen int he next 3 or 6 months? What happened 1 year ago?

# Data
The real app is connected to a google sheet where all expenses/income get recorded and app updates in real time. For this repo, we provide mockup data as example. \

If you want to connect the data from Google drive

```
# Connect to Google drive
## Create an OAuth key from google drive. You can store it in a folder in the app directory called ".secrets". 

options(gargle_oauth_cache = ".secrets") # set the dir to look for oauth key
# Connect
googlesheets4::gs4_auth(email = "some@gmail.com", cache = ".secrets")
gsheet_id <- "some_id_123" # file google id
db_fin <- googlesheets4::read_sheet(gsheet_id)
```

# Run
You cna run directly the app with
`shiny::runApp("app")`

App credentials:
* user = "guest"
* password = "123$5"

# Dependencies

```
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(rlang)
library(priceR)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(shinymanager)
```