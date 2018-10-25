# Configurando diretório de trabalho
setwd("E:/Dev/Source/DataScienceAcademy-FormacaoCientistaDeDados/projeto_05-MapeandoOcorrenciaZikaVirus")
getwd()

# Sources
source("lib/Tools.R")
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggmap")
library(ggmap)
#install.packages("leaflet")
library(leaflet)

# Coleta de dados e Análise Exploratória
## Carregar os arquivos 
lstFiles = list.files(path = "data/", pattern = ".csv", full.names = TRUE)
lstFiles
files <- lapply(lstFiles, read.csv, stringsAsFactors = FALSE) 

## Resumo dos arquivos
str(files, 1)
lapply(files, names)[1]
lapply(files, head,2)[1:2]

## Organizando o shape dos dados
?do.call
?rbind
data <- do.call(rbind, files)
#View(data)
data <- data %>% mutate(report_date = as.Date(report_date))
str(data)
glimpse(data)

## Visualização
region <- data %>% filter(location_type == "region")
region %>% 
  slice(1:length(unique(region$location)))

region %>% 
  ggplot(aes(x = report_date, y = value, group = location, color = location)) + 
  geom_line() +  
  geom_point() +
  ggtitle("Casos de Zika por Região do Brasil")

region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location, levels = location,ordered = TRUE)) %>%
  ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") +
  ylab("Número de Casos Reportados") + xlab("Region") + 
  ggtitle("Casos de Zika Reportados no Brasil")

# Pré-processamento
data <- data %>% select(-(6:7)) 
data %>% slice (1:20)

region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location,levels=location,ordered=TRUE)) %>% 
  glimpse()

data_totals <- data %>% filter(location=="Brazil") 
data_totals

region_totals <- data %>% filter(location_type=="region") %>%
  group_by(report_date,location) %>%  
  summarize(tot = sum(value)) 

regvec <- vector()  
length(regvec) <- nrow(data)
for (ii in 1:nrow(data)) {
  if (data[ii,]$location_type != "region")  {
    regvec[ii] <- newlab
  } else {
    newlab <- data[ii,]$location
    regvec[ii] <- newlab
  }
}

statedf <- cbind(data,regvec)
statedf <- statedf %>% filter(location != "Brazil") 
statedf <- statedf %>% filter(location_type != "region") 

statedf %>% group_by(report_date,regvec) %>% summarize(tot=sum(value)) -> totals
head(statedf)

# Plot do Mapa
longlat <- geocode(unique(statedf$location)) %>%  mutate(loc = unique(statedf$location)) 
longlat

statedf %>% filter(as.character(report_date) == "2016-06-11") %>% 
  group_by(location) %>% summarize(cases = sum(value)) %>% 
  inner_join(longlat, by = c("location" = "loc")) %>% 
  mutate(LatLon = paste(lat, lon, sep = ":")) -> formapping
head(formapping)

num_of_times_to_repeat <- formapping$cases
long_formapping <- formapping[rep(seq_len(nrow(formapping)),
                                  num_of_times_to_repeat),]
head(long_formapping)

leaflet(long_formapping) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())