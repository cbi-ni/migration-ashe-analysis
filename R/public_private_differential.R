load_clean_public_private_data <- function() {
  public.private.data <- getwd() %>%
    paste0("/data/all-ashe-private-public-2018.csv") %>%
    read_csv() %>%
    subset(!(Median %in% c("x", NA))) %>%
    subset(!(Mean %in% c("x", NA)))
  
  public.private.data$Region %<>%
    gsub(pattern = " and ", replacement = " & ")
  
  public.private.data$Median %<>%
    gsub(pattern = ",", replacement = "") %>%
    as.numeric()
  
  public.private.data$Mean %<>%
    gsub(pattern = ",", replacement = "") %>%
    as.numeric()
  
  return(public.private.data)
}


regional_wage_levels <- function(sectors, threshold = 30000, averageType = c("Median", "Mean")) {
  if (sectors == "all") {
    sectors <- c(
      "All",
      "Public sector", 
      "Private sector")
  }
  public.private.data <- load_clean_public_private_data() %>%
    subset(Sector %in% sectors)
  
  privaetPublicWage <- public.private.data %>%
    ggplot() +
    geom_bar(
      mapping = aes(
        x = Region,
        y = Median,
        fill = Sector),
      position = "dodge",
      stat = "identity") +
    geom_hline(yintercept = threshold) +
    xlab("") +
    ylab("Gross annual wage (£)") + 
    scale_fill_discrete(name = "") +
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = "bottom")
  
  getwd() %>%
    paste0("/images/all-public-private-", averageType %>% tolower(), "-wages.png") %>%
    ggsave(
      plot = privaetPublicWage,
      device = "png")
  
  return(privaetPublicWage)
}


analyse_public_private_differential <- function(averageType = c("Median", "Mean")) {
  public.private.data <- load_clean_public_private_data() %>%
    subset(Sector %in% c(
      "Public sector", 
      "Private sector"))

  publicPrivateDifferential <- c()
  for (region in public.private.data$Region %>% unique()) {
    regional.data <- public.private.data %>% 
      subset(Region == region)
    publicPrivateDifferential %<>% 
      append((regional.data[2, averageType] / regional.data[1, averageType] - 1) * 100)
  }
  publicPrivateDifferential %<>% purrr::flatten_dbl()
  
  medianWageDifferential <- data.frame(
    region = public.private.data$Region %>% unique(),
    differential = publicPrivateDifferential,
    stringsAsFactors = FALSE) %>%
    ggplot() +
    geom_bar(
      mapping = aes(
        x = region,
        y = differential,
        fill = differential > 0),
      stat = "identity") +
    xlab("") +
    ylab("Private / public annual wage differential (%)") +
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = "none")
  
  getwd() %>%
    paste0("/images/all-public-private-", averageType %>% tolower(), "-wage-differential.png") %>%
    ggsave(
      plot = medianWageDifferential,
      device = "png")
  
  return(medianWageDifferential)
}


SECTORS <- c(
  "Private sector", 
  "Public sector")
  
  
analyse_public_private_differential(
  averageType = "Median")

regional_wage_levels(
  sectors = SECTORS, 
  averageType = "Median")
