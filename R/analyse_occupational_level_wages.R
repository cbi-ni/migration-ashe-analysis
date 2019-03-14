library(readr)
library(ggplot2)
library(magrittr)


analyse_occupational_level_wages <- function(threshold = 30000, socLevel = 4) {
  ni.ashe.soc.data <- getwd() %>%
    paste0("/data/ni-ashe-soc-2018.csv") %>%
    read_csv() %>%
    subset(!(Median %in% c("x", NA))) %>% 
    subset(nchar(Code) == socLevel)
  
  ni.ashe.soc.data$Description %<>%
    gsub(pattern = "and", replacement = "&")
  
  ni.ashe.soc.data$Median %<>%
    gsub(pattern = ",", replacement = "") %>%
    as.numeric()
  
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
    paste0("/images/soc-wage-level-", socLevel, ".png") %>%
    ggsave(
      plot = SOCLevelWages,
      device = "png")
  
  return(SOCLevelWages)
}
