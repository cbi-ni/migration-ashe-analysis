library(readr)
library(ggplot2)
library(magrittr)


REJECT_INDUSTRIES <- c(
  "other service activities",
  "water supply; sewerage, waste management & remediation activities",
  "agriculture, forestry & fishing",
  "activities of households as employers; undifferentiated goods-& services-producing activities of households for own use")


clean_up_ashe_data <- function(asheData) {
  asheData %<>%
    subset(Code %>% tolower() %in% letters) %>%
    subset(Median != "x")
  
  asheData$Description %<>%
    tolower() %>%
    gsub(pattern = "and", replacement = "&")
  
  asheData$Median %<>%
    gsub(pattern = ",", replacement = "") %>%
    as.numeric()
  
  asheData$Mean %<>%
    gsub(pattern = ",", replacement = "") %>%
    as.numeric()
  
  return(asheData)
}


industrial_level_wages <- function(threshold = 30000, region = c("ni", "uk")) {
  industry.wide.ashe.data <- getwd() %>%
    paste0("/data/", region, "-ashe-sic-2018.csv") %>%
    read_csv() %>%
    clean_up_ashe_data() %>%
    subset(Description != REJECT_INDUSTRIES[4])
    
  
  SICWageLevel <- industry.wide.ashe.data %>%
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
    paste0("/images/", region, "-sic-wage-level.png") %>%
    ggsave(
      plot = SICWageLevel,
      device = "png")
  
  return(SICWageLevel)
}


sic_wage_distribution <- function(region = c("ni", "uk")) {
  industry.wide.ashe.data <- getwd() %>%
    paste0("/data/", region, "-ashe-sic-2018.csv") %>%
    read_csv() %>%
    clean_up_ashe_data()
  
  wageDistribution <- c()
  for (i in 8:17) {
    wageDistribution %<>% append(
      purrr::flatten_chr(industry.wide.ashe.data[, i]))
  }
  
  industries <- rep(industry.wide.ashe.data$Description, 10)
  
  slim.df <- data.frame(
    industry = industries,
    wageDistribution = wageDistribution,
    stringsAsFactors = FALSE) %>%
    subset(wageDistribution != "x") %>%
    subset(!(industry %in% REJECT_INDUSTRIES))
  
  slim.df$wageDistribution %<>%
    gsub(
      pattern = ",", 
      replacement = "") %>%
    as.numeric()
  
  violinChart <- slim.df %>%
    ggplot() +
    geom_violin(
      mapping = aes(
        x = industry,
        y = wageDistribution,
        colour = industry,
        fill = industry)) +
    geom_hline(yintercept = 30000) +
    ylab("Gross yearly income") + 
    xlab("") +
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = "none")
  
  getwd() %>%
    paste0("/images/", region, "-sic-wage-distribution.png") %>%
  ggsave(
    plot = violinChart,
    device = "png")
  
  return(violinChart)
}


sic_wage_distribution(region = "ni")
industrial_level_wages(region = "ni")
