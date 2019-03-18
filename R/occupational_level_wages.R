library(readr)
library(ggplot2)
library(magrittr)


load_clean_soc_data <- function(socLevel, region = c("uk", "ni")) {
  ni.ashe.soc.data <- getwd() %>%
    paste0("/data/", region, "-ashe-soc-2018.csv") %>%
    read_csv() %>%
    subset(!(Median %in% c("x", NA))) %>% 
    subset(nchar(Code) == socLevel)
  
  ni.ashe.soc.data$Description %<>%
    gsub(pattern = "and", replacement = "&")
  
  ni.ashe.soc.data$Median %<>%
    gsub(pattern = ",", replacement = "") %>%
    as.numeric()
  
  return(ni.ashe.soc.data)
}


analyse_occupational_level_wages <- function(threshold = 30000, socLevel = 4, region = c("uk", "ni")) {
  ni.ashe.soc.data <- load_clean_soc_data(
    socLevel = socLevel,
    region = region)
  
  SOCLevelWages <- ni.ashe.soc.data %>%
    ggplot() + 
    geom_bar(
      mapping = aes(
        y = Median, 
        x = Description,
        fill = Median >= threshold),
      stat = "identity") +
    coord_flip() +
    geom_hline(yintercept = threshold) +
    ylab("Gross yearly wage (Median)") +
    xlab("") +
    theme_minimal() +
    theme(legend.position = "none")
  
  getwd() %>%
    paste0("/images/", region, "-soc-wage-level-", socLevel, ".png") %>%
    ggsave(
      plot = SOCLevelWages,
      device = "png")
  
  return(SOCLevelWages)
}


percentage_over_threshold <- function(threshold = 30000, socLevel = 4, region = c("uk", "ni")) {
  ni.ashe.soc.data <- load_clean_soc_data(
    socLevel = socLevel,
    region = region)
  
  percentOverThreshold <- ((ni.ashe.soc.data$Median >= 30000) %>% sum() / 
    (ni.ashe.soc.data %>% nrow())) *
    100
  
  return(percentOverThreshold)
}


analyse_occupational_level_wages(
  threshold = 30000,
  socLevel = 4,
  region = "uk")

percentage_over_threshold(
  region = "uk")
