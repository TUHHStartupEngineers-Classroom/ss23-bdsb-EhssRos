library(jsonlite)
library(glue)
library(httr)
library(RSQLite)
con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "C:/Users/ehsan/Desktop/DataSciece/myds/ss23-bdsb-EhssRos/01_tidyverse/ds_data/02_chinook/Chinook_Sqlite.sqlite")
resp <- GET("https://swapi.dev/api/people/1/")
dbListTables(con)
tbl(con, "Album")
album_tbl <- tbl(con, "Album") %>% collect()
#blablabla
dbDisconnect(con)
con
# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")
resp
name <- "Fred"
glue('My name is {name}.')
resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()
data_list <- list(strings= c("string1", "string2"), 
                  numbers = c(1,2,3), 
                  TRUE, 
                  100.23, 
                  tibble(
                    A = c(1,2), 
                    B = c("x", "y")
                  )
)
# get the URL for the wikipedia page with all S&P 500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()


url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()
rank <-  html %>% 
  html_nodes(css = ".titleColumn") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()
title <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_text()

library(jsonlite)

get_weather_data <- function(location) {
  url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/", 
                location, "?unitGroup=metric&key=RA4VC64DQX39GT95QYTHY545B&contentType=json")
  json_data <- fromJSON(url)
  return(json_data)
}
weather_data <- get_weather_data("Hamburg")
# Load required libraries
library(jsonlite)
library(glue)
library(stringi)
library(httr)

get_weather_data <- function(location) {
  # Construct URL for API request
  url <- glue("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/{location}",
              "?unitGroup=metric&key=RA4VC64DQX39GT95QYTHY545B&contentType=json")
  # Make API request and parse JSON response
  json_data <- fromJSON(content(GET(url), "text"), flatten = TRUE)
  # Extract daily weather data from JSON response
  daily_data <- json_data$days

  return(daily_data)
}


get_weather_data <- function(location) {
  # Construct URL for API request
  url <- glue("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/{location}",
              "?unitGroup=metric&key=RA4VC64DQX39GT95QYTHY545B&contentType=json")
  # Make API request and parse JSON response
  json_data <- fromJSON(content(GET(url), "text"), flatten = TRUE)
  # Extract daily weather data from JSON response
  daily_data <- json_data$days
  # Extract only datetime, tempmax, tempmin, and temp columns
  extracted_data <- daily_data[, c("datetime", "tempmax", "tempmin", "temp")]
  # Rename columns
  colnames(extracted_data) <- c("Datum", "Max. Temperatur", "Min. Temperatur", "Temperatur")
  # Display data in a table with specified header
  cat(paste0("Wetter ", location, " die nächsten 15 Tage:\n\n"))
  print(as_tibble(extracted_data))
}

# Example usage
weather_data <- get_weather_data("Hamburg")


get_weather_data <- function(location) {
  # Construct URL for API request
  url <- glue("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/{location}",
              "?unitGroup=metric&key=RA4VC64DQX39GT95QYTHY545B&contentType=json")
  # Make API request and parse JSON response
  json_data <- fromJSON(content(GET(url), "text"), flatten = TRUE)
  # Extract daily weather data from JSON response
  daily_data <- json_data$days
  # Extract datetime column from each day's data frame
  #datetime_list <- lapply(daily_data, function(x) x$datetime)
  # Combine datetime values from all days into a single vector
  #datetime <- unlist(datetime_list)
  return(dailydata)
}
datetime <- get_weather_data("Hamburg")
print(datetime)
lapply(weather_data$days$datetime, print)