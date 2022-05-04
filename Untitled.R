# Required packages
library(tidyverse)
library(rgdal)
library(sp)


# load data
occ_NF <- read.csv("GBIF_occ_NF_03may2022.csv", header = T) %>% 
  #mutate(municipio = rep(LETTERS[1:10], 924)) %>% 
  select(scientificName, decimalLongitude, decimalLatitude)

norte_RJ <- read.delim("~/Downloads/0263225-210914110416597.csv") %>% 
  mutate(decimalLongitude = as.numeric(decimalLongitude), 
         decimalLatitude = as.numeric(decimalLatitude)) %>% 
  filter(!is.na(decimalLongitude))

occs <- norte_RJ %>% 
  dplyr::select(occurrenceID, decimalLongitude, decimalLatitude) 

# Shapefile import
NORFLU <- rgdal::readOGR("map.shp")
NORFLU_wsg84 <- spTransform(NORFLU, CRS("+proj=longlat +datum=WGS84"))

UCs <- rgdal::readOGR("ucstodas.shp")
proj4string(UCs) <- CRS("+proj=longlat +datum=WGS84")

# Filter UCs within cities
UCs_NF <- subset(UCs, ID_UC0 %in% c(over(NORFLU_wsg84, UCs) %>%  
                                      distinct(ID_UC0) %>% 
                                      filter(!is.na(.)) %>% 
                                      pull()))

# Spatial data
firstPoints <- SpatialPoints(coords = cbind(occs$decimalLongitude, occs$decimalLatitude), 
                             proj4string=CRS("+proj=longlat +datum=WGS84"))
# spdf <- SpatialPointsDataFrame(coords = cbind(occs$decimalLongitude, occs$decimalLatitude), data = norte_RJ,
#                                proj4string = CRS("+proj=longlat +datum=WGS84"))


# Occurrences within cities
cities <- sp::over(firstPoints, NORFLU_wsg84)
protc <- sp::over(firstPoints, UCs)

# merge
bind_cols(protc, cities, norte_RJ) %>% 
  filter(!is.na(NM_MUNICIP)) %>% 
  write.csv("GBIF_occ_NF_03may2022.csv", row.names = FALSE)

occs <- bind_cols(protc, cities, norte_RJ) %>% 
  filter(!is.na(NM_MUNICIP))

occs %>% 
  group_by(NM_MUNICIP, CATEGORI3) %>% 
  summarise(occ = n_distinct(occurrenceID)) %>% data.frame()

occs %>% 
  mutate(CATEGORI3 = plyr::mapvalues(CATEGORI3, from = c(NA, "\xc1rea de Prote\xe7\xe3o Ambiental",
                                                         "Esta\xe7\xe3o Ecol\xf3gica", "Ref\xfagio de Vida Silvestre",
                                                         "Reserva Biol\xf3gica", "Reserva Particular do Patrim\xf4nio Natural"), 
                                     to = c("Fora de UC", "Àrea de Proteção Ambiental",
                                     "Estação Ecológica", "Refúgio de Vida Silvestre",
                                     "Reserva Biológica", "Reserva Particular do Patrimônio Natural"))) %>% 
  mutate(CATEGORIA = ifelse(CATEGORI3 == "Fora de UC", "Fora de UC", "Dentro de UC")) %>% 
  ggplot(aes(y = NM_MUNICIP, fill = CATEGORI3)) +
    geom_bar(position = 'fill') +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(x = "", y = "")


