# Læsning af nødvendige pakker
library(dplyr)
library(stringr)

# Funktion til at rense tekst
clean_text <- function(text) {
  text %>%
    # Erstatte emojis og specialtegn med tom streng ved at bruge Unicode
    str_replace_all("[\U0001F600-\U0001F64F|\U0001F300-\U0001F5FF|\U0001F680-\U0001F6FF|\U0001F700-\U0001F77F|\U0001F780-\U0001F7FF|\U0001F800-\U0001F8FF|\U0001F900-\U0001F9FF|\U0001FA00-\U0001FA6F|\U0001FA70-\U0001FAFF|\U00002702-\U000027B0|\U000024C2-\U0001F251]", "") %>%
    # Erstatte newline med punktum og mellemrum
    str_replace_all("[\r\n]", ". ") %>%
    # Erstatte flere mellemrum med ét mellemrum
    str_replace_all("\\s+", " ") %>%
    # Trim teksten for overskydende mellemrum i starten og slutningen
    str_trim()
}

# Anvend funktionen på 'Description' kolonnen i den danske dataframe
danish_data$Description <- sapply(danish_data$Description, clean_text)

# Hvis du vil kontrollere resultatet, kan du printe et eksempel
head(danish_data$Description)
