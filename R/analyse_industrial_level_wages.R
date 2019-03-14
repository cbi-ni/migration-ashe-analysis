library(readr)
library(ggplot2)
library(magrittr)


clean_up_ashe_data <- function(asheData) {
  asheData %<>%
    subset(Code %>% tolower() %in% letters) %>%
    subset(Median != "x")
  
  asheData$Description %<>%
    gsub(pattern = "and", replacement = "&")
  
  asheData$Median %<>%
    gsub(pattern = ",", replacement = "") %>%
    as.numeric()
  
  asheData$Mean %<>%
    gsub(pattern = ",", replacement = "") %>%
    as.numeric()
  
  return(asheData)
}

industrial_level_wages <- function(threshold = 30000) {
  industry.wide.ashe.data <- getwd() %>%
    paste0("/data/ni-ashe-sic-2018.csv") %>%
    read_csv() %>%
    clean_up_ashe_data()
  
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
    labs(title = "Industrial-level gross yearly wages with a £30,000 threshold") +
    theme_minimal() +
    theme(legend.position = "none")
  
  getwd() %>%
    paste0("/images/sic-wage-level.png") %>%
    ggsave(
      plot = SICWageLevel,
      device = "png")
  
  return(industrial_level_wage_chart)
}



sic_wage_distribution <- function() {
  industry.wide.ashe.data <- getwd() %>%
    paste0("/data/ni-ashe-sic-2018.csv") %>%
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
    subset(!(industry %in% c(
      "Other service activities",
      "Water supply; sewerage, waste management & remediation activities",
      "Agriculture, forestry & fishing")))
  
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
    paste0("/images/sic-wage-distribution.png") %>%
  ggsave(
    plot = violinChart,
    device = "png")
  
  return(violinChart)
}
