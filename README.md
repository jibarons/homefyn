# homefyn

A small Shiny app gain insight on your your home finances. How much I am speding on debts? How much in day to day expenses? I am earning enough? What will happen int he next 3 or 6 months? and What happened 1 year ago?

# Data
The real app is connected to a google sheet where all expenses/income get recorded and app updates in real time. For this repo, we provide mockup data as example.

Hwoever, we left the commented code to connect to google drive for reference.

# Run
You cna run directly the app with
`shiny::runApp("app")`

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