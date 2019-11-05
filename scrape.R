library(readr)
library(dplyr)
library(rvest)
library(tidyverse)
library(lubridate)
library(xml2)
library(tidycensus)
library(utils)
# A1.1&1.2
# Create folders for data and figures
dir.create("./hw4_Gao_Gu_Zhou/data")
dir.create("./hw4_Gao_Gu_Zhou/figures")

# A2.1
# Function to scrape cirme data on current page of neighborhood
scrape_data <- function(x) {
        response <- read_html(x)
        crime <- rvest::html_nodes(x = response, 
                                   xpath = '//td[contains(@class, "field-name")]')
        crime<- html_text(crime, trim = T)
        time <- rvest::html_nodes(x = response, 
                                  xpath = '//span[contains(@class, "date-display")]')
        time <- parse_date_time(html_text(time),'m/d/y HM p!')
        hour <- hour(time)
        crime_data <- data.frame(crime, hour)
        crime_data <- cbind(crime_data, neighborhood = rep(neighborhood, nrow(crime_data)))
        crime_data
}

# Function to get url for next page of crime data if there is more page after the current page for a specific neighborhood
get_next_url <- function(x) {
        response <- read_html(x)
        next_page <- rvest::html_nodes(x = response, 
                                       xpath = '//a[contains(@title, "next page")]')
        next_page <- html_attr(x = next_page, 'href')
        page_url <- ifelse(length(next_page) == 0, NA, paste0('https://www.universalhub.com', next_page))
        page_url
}

# Get html for the homepage of cirme
home_url <- "https://www.universalhub.com/crime/home.html"
resp <- read_html(home_url)

# Scrape nodes for all neighborhoods
neighbor <- rvest::html_nodes(x = resp, 
                              xpath = '//select[@id = "edit-jump"]//option[contains(@value, "crime/")]')

# Creat empty crime_data
crime_data <- data.frame()
# loop for each neighbor to collect crime data
for (i in 1:length(neighbor)) {
        neighborhood <- html_text(neighbor[[i]], trim = T)
        link_neighbor <- paste0('https://www.universalhub.com', xml_attrs(neighbor[[i]])[["value"]])
        crime_data <- rbind(crime_data, scrape_data(link_neighbor))
        next_url <- get_next_url(link_neighbor)
        while (!is.na(next_url)) {          # Check whether there is next page after current page and if true, loop until it becames false
                next_page <- scrape_data(next_url)
                crime_data <- rbind(crime_data, next_page)
                next_url <- get_next_url(next_url)
        }
        crime_data
}

# Save data
write_csv(crime_data, 'question_a2_1.csv')

# while (page < last_page) {
#         next_page <- scrape_data(next_url)
#         crime_data <- rbind(crime_data, next_page)
#         next_url <- get_next_url(next_url)
#         curr_page <- curr_page + 1
# }
# 
# 
# get_last_page <- function(x) {
#         response <- read_html(x)
#         pages <- rvest::html_nodes(x = response, 
#                                    xpath = '//a[contains(@title, "Go to page")]')
#         last_page <- ifelse(length(pages) == 0, 1, max(as.numeric(html_text(pages))))
#         last_page
# }
# 
# 
# get_curr_page <- function(x) {
#         response <- read_html(x)
#         curr_page <- rvest::html_nodes(x = response, 
#                                        xpath = '//li[contains(@class, "pager-current")]')
#         curr_page <- ifelse(length(curr_page) == 0, 1, as.numeric(html_text(curr_page)))
#         curr_page
# }

