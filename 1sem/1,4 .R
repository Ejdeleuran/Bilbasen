library(httr)
library(rvest)
library(dplyr)

# Start med at definere grund-URL'en (uden pagineringsparameter)
base_url <- "https://www.autoscout24.com/lst/volkswagen/id.4/ft_electric?atype=C&cy=D&desc=0&powertype=kw&search_id=24cd417l46l&sort=standard&source=detailsearch&ustate=N%2CU"

# CSS-selectors til de relevante data
price_tag <- "p.Price_price__APlgs"
name_tag <- "h2"
mileage_tag <- 'span[data-testid="VehicleDetails-mileage_road"]'
calendar_tag <- 'span[data-testid="VehicleDetails-calendar"]'
seller_tag <- 'span[data-testid="sellerinfo-company-name"]'
link_tag <- "a[href]"  # Dette kan være CSS-selector for bilens link

# Opret en tom dataframe til at gemme data
de_vwdf <- data.frame(
  price_eur = character(0),
  car_model = character(0),
  mileage_km = character(0),
  year_first_registration = character(0),
  seller_name = character(0),
  car_link = character(0),      # Kolonne for bilens link
  scrapedate = character(0),    # Kolonne for scrapede dato
  stringsAsFactors = FALSE
)

# Definer antal sider, du vil hente (f.eks. de første 100 sider)
num_pages <- 100

# Få den aktuelle dato for scrapedate
scrapedate <- Sys.Date()

# Loop gennem alle siderne
for (page_num in 1:num_pages) {
  # Skab URL'en for den aktuelle side
  page_url <- paste0(base_url, "&page=", page_num)
  
  # Send GET-anmodning til websiden
  response <- GET(page_url)
  
  # Tjek statuskode for anmodningen
  if (status_code(response) != 200) {
    stop("Fejl ved hentning af data: ", status_code(response))
  }
  
  # Hent HTML-indholdet
  page <- read_html(content(response, "text", encoding = "UTF-8"))
  
  # Ekstraher biloplysninger
  cars <- page %>% html_elements("article")
  
  # Loop gennem alle biler på siden og ekstraher information
  for (car in cars) {
    tryCatch({
      # Hent bilens data
      de_price <- car %>% html_element(price_tag) %>% html_text(trim = TRUE)
      de_name <- car %>% html_element(name_tag) %>% html_text(trim = TRUE)
      de_mileage <- car %>% html_element(mileage_tag) %>% html_text(trim = TRUE)
      de_calendar <- car %>% html_element(calendar_tag) %>% html_text(trim = TRUE)
      de_seller <- car %>% html_element(seller_tag) %>% html_text(trim = TRUE)
      
      # Hent bilens link (relativt link til bilens detaljer)
      car_link <- car %>% html_element(link_tag) %>% html_attr("href")
      car_link <- paste0("https://www.autoscout24.com", car_link)  # Sæt base-URL sammen med linket
      
      # Tilføj til dataframe
      de_vwdf <- rbind(de_vwdf, data.frame(
        price_eur = de_price,
        car_model = de_name,
        mileage_km = de_mileage,
        year_first_registration = de_calendar,
        seller_name = de_seller,
        car_link = car_link,
        scrapedate = as.character(scrapedate),  # Indsæt den aktuelle scrapede dato
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      message("Fejl ved ekstraktion af data: ", e)
    })
  }
}

# Ændre kolonnenavnene
colnames(de_vwdf) <- c("price_eur", "car_model", "mileage_km", "year_first_registration", "seller_name", "car_link", "scrapedate")

# Data er nu gemt i 'de_vwdf' dataframe i R.
# Du kan nu bruge dataene i R til analyse eller visualisering
print(head(de_vwdf))  # Print de første rækker for at se dataene

