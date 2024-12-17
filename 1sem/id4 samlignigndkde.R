# Indlæs nødvendige pakker
library(ggplot2)
library(scales)

# Indlæs de danske og tyske data
danish_data <- read.csv("VWid4_RENSET_DATA_BILBASEN2024-11-25.csv", stringsAsFactors = FALSE)
german_data <- read.csv("de_vwdf.csv", stringsAsFactors = FALSE)

# Valutakurs (EUR -> DKK)
exchange_rate <- 7.5

# Rens danske priser
danish_data$Pris <- as.numeric(gsub("[^0-9]", "", danish_data$Pris))

# Filtrer relevante danske rækker
danish_segment <- danish_data[!is.na(danish_data$Pris), ]

# Rens tyske priser (fjern kun ikke-numeriske tegn)
german_data$price_eur <- as.numeric(gsub("[^0-9]", "", german_data$price_eur))

# Omregn til DKK (hvis priser allerede er i EUR)
german_data$price_dkk <- german_data$price_eur * exchange_rate

# Filtrer priser > 0
german_segment <- german_data[!is.na(german_data$price_dkk) & german_data$price_dkk > 0, ]

# Hent top 2 priser i Danmark
top2_danish <- sort(danish_segment$Pris, decreasing = TRUE)[1:2]

# Hent top 2 priser i Tyskland
top2_german <- sort(german_segment$price_dkk, decreasing = TRUE)[1:2]

# Hvis der ikke er nok data i Tyskland, sættes manglende værdier til NA
if(length(top2_german) < 2) {
  top2_german <- c(top2_german, rep(NA, 2 - length(top2_german)))  # Tilføj NA hvis der er færre end 2 priser
}

# Opret et data frame til at sammenligne de to højeste priser
price_comparison <- data.frame(
  Country = c("Danmark - Top 1", "Danmark - Top 2", "Tyskland - Top 1", "Tyskland - Top 2"),
  Price = c(top2_danish[1], top2_danish[2], top2_german[1], top2_german[2])
)

# Plot de to højeste priser for Danmark og Tyskland
ggplot(price_comparison, aes(x = Country, y = Price, fill = Country)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Søjlediagram
  scale_fill_manual(values = c("Danmark - Top 1" = "blue", "Danmark - Top 2" = "lightblue",
                               "Tyskland - Top 1" = "#33a02c", "Tyskland - Top 2" = "#b2df8a")) +  # Farver
  scale_y_continuous(labels = scales::label_number()) +  # Formatter y-aksen uden komma
  geom_text(aes(label = ifelse(is.na(Price), "Ingen data", paste0(round(Price, 0), " DKK"))), 
            vjust = -0.5, size = 4, fontface = "bold") +  # Tekst over søjler
  theme_minimal(base_size = 14) +  # Minimalistisk tema
  labs(
    title = "Top 2 priser for VW ID.4 i Danmark og Tyskland",
    subtitle = "Priser omregnet til DKK (Valutakurs: 1 EUR = 7.5 DKK)",
    x = "Land og Top",
    y = "Pris (DKK)",
    caption = "Data hentet fra Bilbasen og Autoscout24"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10)),
    axis.text = element_text(face = "bold"),
    legend.position = "none"
  )

