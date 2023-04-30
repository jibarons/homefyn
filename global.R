library(dplyr)
library(tidyr)
library(openxlsx)
library(config)
#library(splitstackshape)
library(rlang)
library(priceR)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(shinymanager)

# Setup password reqs after inactivity
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"
# data.frame with credentials info
credentials <- data.frame(
  user = c("user1", "user2"),
  password = c("12345", "123"),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)

# load funs
fmat.num = function(x, d = 0) format(
  as.numeric(sprintf(paste0("%.",d,"f"), x)), big.mark = ","
)

# Get finance data ----
db_fin <- readxl::read_excel("data/finance_data.xlsx")

# Exchange rates ----
## Get exchnage rates ----
php_rate <- priceR::exchange_rate_latest("PHP")
php_usd <- php_rate[php_rate$currency == "USD", 2]
php_eur <- php_rate[php_rate$currency == "EUR", 2]
usd_rate <- priceR::exchange_rate_latest("USD")
usd_eur <- usd_rate[usd_rate$currency == "EUR", 2]
## Convert rates ----
db_fin <- db_fin |> dplyr::mutate(
    cost_php = ifelse(!is.na(cost_eur), cost_eur / php_eur, cost_php),
    cost_php = ifelse(!is.na(cost_usd), cost_usd / php_usd, cost_php),
    cost_eur = ifelse(is.na(cost_eur), cost_php * php_eur, cost_eur),
    cost_usd = ifelse(is.na(cost_usd), cost_php * php_usd, cost_usd),
)

# Calculate active cost ----
db_fin$end_date <- as.Date(db_fin$end_date) # format date
## Calculate months to expire
db_fin$expire_months <- round(
  as.numeric(difftime(db_fin$end_date, Sys.Date(), units = "days") / 30.42)
)
## Active if months to expire >= 0
db_fin$active <- ifelse(
  db_fin$expire_months >= 0 | is.na(db_fin$expire_months), "active", "expired"
)

# Pivot long
db_fin <- tidyr::pivot_longer(db_fin, dplyr::matches("^cost_"), names_to = "currency") |>
  dplyr:::mutate(currency = gsub("^cost_(.+)$", "\\1", currency))



#shiny::runApp()


