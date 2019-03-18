library(ggplot2)
library(ggthemes)
library(magrittr)


# Set up list of mean yearly salary (2018)
REGIONAL_WAGE_LIST <- list(
  "0" = 24872,     # North East
  "1" = 26835,     # North West
  "2" = 25635,     # Yorkshire and The Humber
  "3" = 25898,     # East Midlands
  "4" = 33000,     # London
  "5" = 27530,     # West Midlands
  "6" = 28592,     # East of England
  "7" = 31680,     # South East
  "8" = 26587,     # South West
  "9" = 23519,     # Wales
  "10" = 28261,    # Scotland
  "11" = 22713     # Northern Ireland
)


generate_uk_aveage_wage_map <- function(wageList) {
  # Filename of UK shapefile
  mapDataLocation <- "NUTS_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom"
  
  # Load NI shapefile into R
  UK <- rgdal::readOGR(
    dsn = getwd() %>% paste0(
      "/data/map/regional-boundaries/", mapDataLocation, ".shp"),
    layer = mapDataLocation)
  
  # Create UK map data.frame
  UKMap <- ggplot2::fortify(
    model = UK)
  
  # Apply wage levels to regions
  UKMap$number <- rep(0, nrow(uk_map))
  for (id in UKMap$id %>% unique()) {
    UKMap$number[UKMap$id == id] <- wageList[id][[1]]
  }
  
  # Generate map plot
  UKRegionalMap <- ggplot(
    data = UKMap, 
    aes(
      x = long, 
      y = lat)) + 
    geom_polygon(
      colour = "white",
      aes(
        fill = number,
        group = group)) + 
    ggthemes::theme_map() +
    scale_fill_gradient(
      low = "#D32F2F", 
      high = "#0288D1") +
    theme(legend.position = "none")
  
  getwd() %>%
    paste0("/images/uk-average-wage-map.png") %>%
    ggsave(
      plot = UKRegionalMap,
      device = "png",
      width = 16.98,
      height = 21.96, 
      units = "cm")
  
  return(UKRegionalMap)
  
}


generate_uk_aveage_wage_map(
  wageList = REGIONAL_WAGE_LIST)
