################################
#Opgave 1 ola 4
library(httr)
library(rvest)
library(tidyverse)

# ---------- Opgave 1.1: Hente data fra Bilbasen ----------
# URL til scraping
vwlink <- "https://www.bilbasen.dk/brugt/bil/vw/id2314?fuel=3&includeengroscvr=true&includeleasing=false"

# Headers til HTTP-anmodning
headers <- add_headers(
  `accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
  `accept-encoding` = "gzip, deflate, br",
  `accept-language` = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
)

# Hent og parse første side
response <- GET(url = vwlink, headers)
if (response$status_code != 200) {
  stop("Fejl ved hentning af data: HTTP status ", response$status_code)
}
rawcontent <- content(response, as = "text", encoding = "UTF-8")
page <- read_html(rawcontent)

# Find antal sider
last_page <- page %>% 
  html_element('span[data-e2e="pagination-total"]') %>% 
  html_text(trim = TRUE) %>% 
  as.numeric()

if (is.na(last_page)) {
  last_page <- 1
}

# Data frame til at gemme biler
vwdf <- data.frame(
  Model = character(),
  Price = character(),
  Details = character(),
  Description = character(),
  Location = character(),
  Link = character(),
  stringsAsFactors = FALSE
)

# Loop gennem siderne og hent data
for (i in 1:last_page) {
  message(paste("Scraper side", i, "af", last_page))
  loopurl <- paste0(vwlink, "&page=", i)
  
  response <- GET(loopurl, headers)
  if (response$status_code != 200) {
    warning("Fejl ved hentning af side ", i, ": HTTP status ", response$status_code)
    next
  }
  
  rawcontent <- content(response, as = "text", encoding = "UTF-8")
  page <- read_html(rawcontent)
  
  carlist <- page %>% html_elements("article")
  for (car in carlist) {
    tryCatch({
      model <- car %>% html_element(".Listing_makeModel__7yqgs") %>% html_text(trim = TRUE)
      price <- car %>% html_element(".Listing_price__6B3kE") %>% html_text(trim = TRUE)
      details <- car %>% html_elements(".ListingDetails_listItem___omDg") %>% html_text(trim = TRUE) %>% paste(collapse = ", ")
      description <- car %>% html_element(".Listing_description__sCNNM") %>% html_text(trim = TRUE)
      location <- car %>% html_element(".Listing_location__nKGQz") %>% html_text(trim = TRUE)
      link <- car %>% html_element("a") %>% html_attr("href")
      
      vwdf <- rbind(vwdf, data.frame(
        Model = model,
        Price = price,
        Details = details,
        Description = description,
        Location = location,
        Link = paste0("https://www.bilbasen.dk", link),
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      warning("Fejl i dataekstraktion: ", e)
    })
  }
  
  Sys.sleep(runif(1, min = 2, max = 5))
}
