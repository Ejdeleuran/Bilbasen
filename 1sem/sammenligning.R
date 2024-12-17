# Indlæs nødvendige pakker
library(ggplot2)

# Indlæs de danske og tyske data
danish_data <- read.csv("VWid4_RENSET_DATA_BILBASEN2024-11-25.csv", stringsAsFactors = FALSE)
german_data <- read.csv("de_vwdf.csv", stringsAsFactors = FALSE)

# Valutakurs (EUR -> DKK)
exchange_rate <- 7.5

# Tjek strukturen af datasættene
str(danish_data)
str(german_data)

# ---------------------------------------------------------
# RENSNING OG TRANSFORMATION AF DATA
# ---------------------------------------------------------

# 1. Rens danske priser (fjern "kr.", punktum og andre tegn)
danish_data$Pris <- as.numeric(gsub("[^0-9]", "", danish_data$Pris))

# 2. Filtrer relevante danske rækker
danish_segment <- danish_data[!is.na(danish_data$Pris), ]

# 3. Rens tyske priser (fjern €, punktum og andre tegn, og konverter til numerisk)
german_data$price_eur <- as.numeric(gsub("[^0-9]", "", german_data$price_eur)) / 100

# 4. Konverter de tyske priser til DKK
german_data$price_dkk <- german_data$price_eur * exchange_rate

# 5. Filtrer relevante tyske rækker
german_segment <- german_data[!is.na(german_data$price_dkk), ]

# ---------------------------------------------------------
# BEREGNING AF GENNEMSNITSPRISER
# ---------------------------------------------------------

# Beregn gennemsnitsprisen for begge datasæt
mean_danish_price <- mean(danish_segment$Pris, na.rm = TRUE)
mean_german_price <- mean(german_segment$price_dkk, na.rm = TRUE)

# Udskriv gennemsnitspriser
print(paste("Gennemsnitspris i Danmark: ", round(mean_danish_price, 2), "DKK"))
print(paste("Gennemsnitspris i Tyskland: ", round(mean_german_price, 2), "DKK"))

# ---------------------------------------------------------
# VISUALISERING AF DATA
# ---------------------------------------------------------

# Opret et data frame til sammenligning af priser
price_comparison <- data.frame(
  Country = c("Denmark", "Germany"),
  Price = c(mean_danish_price, mean_german_price)
)

# Plot sammenligningen
ggplot(price_comparison, aes(x = Country, y = Price, fill = Country)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Prisforskel mellem Danmark og Tyskland for bilsegmentet",
       y = "Gennemsnitspris (DKK)") +
  geom_text(aes(label = round(Price, 2)), vjust = -0.5)


### pænere visuelt 
# Opret et data frame til sammenligning af priser
price_comparison <- data.frame(
  Country = c("Danmark", "Tyskland"),
  Price = c(mean_danish_price, mean_german_price)
)

# Plot sammenligningen
ggplot(price_comparison, aes(x = Country, y = Price, fill = Country)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Smal og tydelig søjle
  scale_fill_manual(values = c("Danmark" = "#1f78b4", "Tyskland" = "#33a02c")) +  # Blå og grøn farve
  geom_text(aes(label = paste0(round(Price, 0), " DKK")), vjust = -0.5, size = 4, fontface = "bold") +  # Tekst over søjler
  theme_minimal(base_size = 14) +  # Minimalistisk tema med større skrift
  labs(
    title = "Gennemsnitlige priser for VW ID.4 i Danmark og Tyskland",
    subtitle = "Priser omregnet til DKK (Valutakurs: 1 EUR = 7.5 DKK)",
    x = "Land",
    y = "Gennemsnitspris (DKK)",
    caption = "Data hentet fra Bilbasen og Autoscout24"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centreret og fed titel
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10)),
    axis.text = element_text(face = "bold"),  # Fed skrift på aksetekster
    legend.position = "none"  # Fjern legenden, da farver er selvforklarende
  ) +
  annotate("text", x = 1.5, y = max(price_comparison$Price) * 1.05, 
           label = "Danske priser er højere sammenlignet med tyske priser", 
           size = 4, color = "grey30", fontface = "italic")


