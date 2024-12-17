# Indlæs nødvendige pakker
library(dplyr)
library(stringr)

# Simuleret scrape date (én dag senere end oprindeligt)
scrape_date <- Sys.Date() + 1

# Oprette ny dataframe for de nye biler
new_rows <- data.frame(
  Model = c("VW ID.4 GTX", "VW ID.4 Pro"),
  Price = c("400.000 kr.", "350.000 kr."),
  Details = c("El, 2023, 400 km", "El, 2023, 500 km"),
  Description = c("Ny bil. Første ejer.", "Ny bil med masser af udstyr."),
  Location = c("København", "Aarhus"),
  Link = c("https://www.bilbasen.dk/link1", "https://www.bilbasen.dk/link2"),
  stringsAsFactors = FALSE
)

# Oprette simulated_danish_data som en kopi af danish_data og tilføje scrape_date
simuleret_danish_data <- danish_data %>%
  mutate(scrape_date = as.Date("2024-11-22")) %>%
  mutate(scrape_date = scrape_date)

# Fjernelse af solgte biler (5 tilfældige rækker)
set.seed(123)  # Sørger for, at de samme rækker vælges hver gang
if(nrow(simuleret_danish_data) >= 5) {  # Tjekker om der er nok biler til at fjerne
  solgte_biler <- sample(1:nrow(simuleret_danish_data), 5)  # Vælg 5 tilfældige biler
  fjernede_biler <- simuleret_danish_data[solgte_biler, ]  # Gem de solgte biler i en separat dataframe
  simuleret_danish_data <- simuleret_danish_data[-solgte_biler, ]  # Fjern de solgte biler fra simuleret_danish_data
} else {
  print("Ikke nok biler til at fjerne.")
}

# Tilføjelse af nye biler
simuleret_danish_data <- bind_rows(simuleret_danish_data, new_rows)

# Ændring af priser (3 tilfældige biler med opdateret pris)
set.seed(456)  # Sørger for, at de samme biler påvirkes hver gang
opdaterede_biler_indeks <- sample(1:nrow(simuleret_danish_data), 3)  # Vælg 3 tilfældige biler
opdaterede_biler <- simuleret_danish_data[opdaterede_biler_indeks, ]  # Gem de opdaterede biler
opdaterede_biler$original_price <- opdaterede_biler$Price  # Gem den oprindelige pris

# Opdater priserne med tilfældige ændringer (±10%)
simuleret_danish_data$Price[opdaterede_biler_indeks] <- as.character(
  round(as.numeric(gsub("[^0-9]", "", simuleret_danish_data$Price[opdaterede_biler_indeks])) * runif(3, 0.9, 1.1), 0)
)

# Gem de opdaterede biler i en separat dataframe
opdaterede_biler$updated_price <- simuleret_danish_data$Price[opdaterede_biler_indeks]

# Resultat
print("Simuleret Dataframe med nye data:")
print(simuleret_danish_data)

print("Fjernede biler:")
print(fjernede_biler)

print("Opdaterede biler:")
print(opdaterede_biler)

