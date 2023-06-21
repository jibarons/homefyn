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

pass <- Sys.getenv("homefin_pass")
# data.frame with credentials info
credentials <- data.frame(
  user = c("guest"),
  password = c("123$5"),
  stringsAsFactors = FALSE
)

# Setup
# Custom funs
fmat.num = function(x, d = 0) {
  x <- sprintf(paste0("%.", d, "f"), x) |> as.numeric()
  x <- format(x, big.mark = ",")
  return(x)
}

# Get finance data ----
db_fin <- readxl::read_excel("../data/mockup_fin_data.xlsx")

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

# Format dates ----
db_fin$end_date <- as.Date(db_fin$end_date)
db_fin$start_date <- as.Date(db_fin$start_date)
## Calculate months to expire
db_fin$expire_months <- round(
  as.numeric(difftime(db_fin$end_date, Sys.Date(), units = "days") / 30.42)
)
## Active if months to expire >= 0
db_fin$active <- ifelse(
  db_fin$expire_months > 0 | is.na(db_fin$expire_months), "active", "expired"
)

# Pivot long keeo start-end sep # Why? Cannot be merged like below - possible refactor
db_fin <- tidyr::pivot_longer(db_fin, dplyr::matches("^cost_"), names_to = "currency") |>
  dplyr:::mutate(currency = gsub("^cost_(.+)$", "\\1", currency))


# Pivot long merging dates
db_fin_time <- db_fin |>
  tidyr::pivot_longer(
    c(start_date, end_date), names_to = "when", values_to = "date",
    values_drop_na = TRUE
  ) |>
  dplyr::arrange(date)
# Ended expenses as negative
db_fin_time <- dplyr::mutate(db_fin_time, value = dplyr::if_else(
  when == "end_date", -1 * value, value))



# shiny::runApp("app")
# rsconnect::setAccountInfo(
#   name='jibarons', 
#   token='??', 
#   secret='??'
# )
# rsconnect::deployApp()






  

