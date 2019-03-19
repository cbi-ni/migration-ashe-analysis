#                        Agriculture, Forestry and Fishing    A
#                                     Mining and Quarrying    B
#                                            Manufacturing    C
#      Electricity, gas, steam and air conditioning supply    D
#             Water supply, sewerage, waste management and    E
#                                             Construction    F
# Wholesale and retail trade; repair of motor vehicles and    G
#                               Transportation and storage    H
#                Accommodation and food service activities    I
#                            Information and communication    J
#                       Financial and insurance activities    K
#                                   Real estate activities    L
#        Professional, scientific and technical activities    M
#            Administrative and support service activities    N
#     Public administration and defence; compulsory social    O
#                                                Education    P
#                  Human health and social work activities    Q
#                       Arts, entertainment and recreation    R
#                                 Other service activities    S
#  Activities of households as employers; undifferentiated    T
#         Activities of extraterritorial organisations and    U

# Industry sections most impacted: A, C, F, I


library(ggplot2)
library(ggthemes)
library(jsonlite)
library(magrittr)


generate_shapefile_map <- function() {
  spdf <- getwd() %>% 
    paste0("/data/map/northern-ireland/parliamentaries/NI-parliamentary-boundaries.shp") %>%
    maptools::readShapePoly()
  
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- ggplot2::fortify(
    spdf, region = "id")
  heatmap.data <- dplyr::inner_join(
    spdf.points, 
    spdf@data, 
    by = "id")
  
  heatmap.data$COUNTYNAME %<>% 
    as.vector()
  
  shapefileMap <- ggplot(
    data = heatmap.data) + 
    geom_polygon(
      colour = "white",
      fill = "grey",
      size = 0.5, 
      aes(
        x = long, 
        y = lat, 
        group = group)) +
    ggthemes::theme_map()
  
  return(shapefileMap)
}


group_firms_in_area <- function(firmData) {
  firmData$group <- "x"
  spdf <- getwd() %>% 
    paste0("/data/map/northern-ireland/parliamentaries/NI-parliamentary-boundaries.shp") %>%
    maptools::readShapePoly()
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- ggplot2::fortify(
    spdf, region = "id")
  heatmap.data <- dplyr::inner_join(
    spdf.points, 
    spdf@data, 
    by = "id")
  heatmap.data$COUNTYNAME %<>% 
    as.vector()
  maximinCoordinates <- list(
    groupIds = heatmap.data$id %>%
      unique(),
    latMax = c(), 
    latMin = c(), 
    longMax = c(), 
    longMin = c())
  for (groupId in maximinCoordinates$groupIds) {
    sub.heatmap.data <- subset(
      x = heatmap.data, 
      subset = id == groupId)
    maximinCoordinates$latMax %<>% 
      append(sub.heatmap.data$lat %>% max())
    maximinCoordinates$latMin %<>% 
      append(sub.heatmap.data$lat %>% min())
    maximinCoordinates$longMax %<>% 
      append(sub.heatmap.data$long %>% max())
    maximinCoordinates$longMin %<>% 
      append(sub.heatmap.data$long %>% min())
  }
  for (i in 1:(maximinCoordinates$latMax %>% length())) {
    groupFirms <- firmData %>% 
      subset(lat <= maximinCoordinates$latMax[i] & lat >= maximinCoordinates$latMin[i] &
               long <= maximinCoordinates$longMax[i] & long >= maximinCoordinates$longMin[i])
    m <- match(groupFirms$CompanyNumber, firmData$CompanyNumber)
    firmData$group[m] <- maximinCoordinates$groupIds[i]
  }
  return(firmData)
}


generate_company_map <- function(firmData, toYear, sections = c(), sectionOnly = FALSE, legend = TRUE) {
  shapefileMap <- generate_shapefile_map()
  
  sectorColours <- setNames(
    c("#9C27B0", "#E91E63", "#F44336", "#2196F3", "#3F51B5", "#673AB7", 
      "#009688", "#00BCD4", "#03A9F4", "#CDDC39", "#8BC34A", "#4CAF50", 
      "#FF9800", "#FFC107", "#FFEB3B", "#9E9E9E", "#795548", "#FF5722", 
      "#607D8B", "#880E4F", "#1565C0", "#388E3C"), 
    c(sort(unique(NIFirms$section)), NA))
  
  if (sections %>% length() > 0) {
    firmData %<>% 
      subset(section %in% sections)
  }
  
  NIFirmsYear <- firmData %>% 
    subset(year <= toYear) %>%
    subset(!is.na(section))
  
  if (sectionOnly) {
    `SIC Sector` <- NIFirmsYear$section
  } else {
    `SIC Sector` <- NIFirmsYear$`5DIGITSIC`
  }
  
  "Generating map for " %>%
    paste0(toYear) %>% 
    print()
  
  companiesEstablised <- shapefileMap +
    geom_point(
      data = NIFirmsYear, 
      aes(
        x = long, 
        y = lat,
        colour = section),
      alpha = 0.6) + 
    labs(
      color = "Industry sector code") +
    ylab("") + xlab("") +
    coord_cartesian(
      xlim = c(-8.25, -5),
      ylim = c(54, 55.5)) +
    theme_minimal() +
    scale_color_manual(values = sectorColours) +
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.position = ifelse(legend, "right", "none"))
  
  getwd() %>%
    paste0("/images/map-all-ni-firms-", toYear,".png") %>%
    ggsave(
      plot = companiesEstablised,
      device = "png", 
      width = 19.83,
      height = 14.74, 
      units = "cm")
  
  return(companiesEstablised)
}


generate_firm_sections <- function(firmData) {
  UKSIC2007Condensed <- getwd() %>% 
    paste0("/data/uk-sic-2007-condensed.csv") %>%
    readr::read_csv()
  
  section <- sicDescription <- sectionDescription <- c()
  for (i in 1:(firmData %>% nrow())) {
    if (firmData$`5DIGITSIC`[i] %>% is.na()) {
      section %<>% 
        append(NA)
      sicDescription %<>% 
        append(NA)
      sectionDescription %<>% 
        append(NA)
    } else {
      r <- firmData$`5DIGITSIC`[i] %>% 
        match(UKSIC2007Condensed$sic_code)
      section %<>% 
        append(UKSIC2007Condensed$section[r])
      sicDescription %<>% 
        append(UKSIC2007Condensed$sic_description[r])
      sectionDescription %<>% 
        append(UKSIC2007Condensed$section_description[r])
    }
  }
  
  firmData$section <- section
  firmData$sicDescription <- sicDescription
  firmData$sectionDescription <- sectionDescription
  
  return(firmData)
}


generate_company_areas <- function(firmData, toYear, sections = c()) {
  shapefileMap <- generate_shapefile_map()
  
  firmData %<>% 
    subset(year <= toYear) %>%
    subset(!is.na(section))
  
  if (sections %>% length() > 0) {
    firmData %<>% 
      subset(section %in% sections)
  }
  
  shapefileMap$data$number <- 0
  for (grp in firmData$group %>% unique()) {
    shapefileMap$data$number[shapefileMap$data$id == grp] <- subset(
      x = firmData, 
      subset = group == grp) %>% 
      nrow()
  }
  
  companyAreaMap <- ggplot(
    data = shapefileMap$data) + 
    geom_polygon(
      colour = "white",
      size = 0.5, 
      aes(
        x = long, 
        y = lat, 
        fill = number,
        group = group)) +
    scale_fill_gradient(
      low = "#CFD8DC", 
      high = "#C2185B") +
    ggthemes::theme_map() + 
    theme(legend.position = "none")
  
  sections %<>% 
    paste(collapse = "-") %>% 
    tolower()
  
  getwd() %>%
    paste0("/images/ni-impact-map-sic-", sections, ".png") %>%
  ggsave(
    plot = companyAreaMap,
    device = "png", 
    width = 19.83,
    height = 14.74, 
    units = "cm")
  
  return(companyAreaMap)
}


load(
  getwd() %>% 
    paste0("/data/RData/NIFirms.rda"))

NIFirms %<>% 
  subset(lat < 60) %>% 
  subset(long < -4)

NIFirms$year <- strsplit(
  x = NIFirms$`Incorporation Date (Formatted)`, 
  split = "-", 
  fixed = TRUE) %>% 
  purrr::flatten_chr() %>%
  subset(nchar(.) == 4) %>%
  as.integer()

NIFirms$`5DIGITSIC` %<>% 
  as.integer()

NIFirms %<>%
  generate_firm_sections()

generate_company_map(
  firmData = NIFirms, 
  toYear = 2018, 
  sections = c(),
  sectionOnly = TRUE)

NIFirms %<>% 
  group_firms_in_area()

generate_company_areas(
  firmData = NIFirms,
  toYear = 2018,
  sections = c("A", "C", "F", "I"))
