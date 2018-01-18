library(dplyr)
library(readr) 
library(highcharter)
library(leaflet)
library(stringr)
library(sp)

MM=read.csv("data/MissingMigrants-Global-2018-01-17T12-06-41.csv", encoding = "UTF-8")

############################################################################
# Evolution of number of deaths/missing
############################################################################
MM %>% 
  filter(Reported.Year<2018) %>% 
  mutate(Month=paste(Reported.Month, Reported.Year, sep= " ") %>% 
           parse_date("%b %Y",locale=locale("en"))) %>% 
  group_by(Region.of.Incident, Month) %>% 
  summarise(DeadMiss=sum(Total.Dead.and.Missing)) -> df1


hchart(df1, "spline", hcaes(x = Month, y = DeadMiss, group = Region.of.Incident)) %>% 
  hc_xAxis(title = list(text = "Month")) %>% 
  hc_yAxis(title = list(text = "Total")) %>% 
  hc_title(text = "Migrant deaths or missing by region") %>% 
  hc_subtitle(text = "January 2014 to December 2017") %>% 
  hc_credits(enabled = TRUE, # add credits
             text = "<b>Data source:</b> Missing Migrants Project",
             href = "https://missingmigrants.iom.int/")

############################################################################
# Evolution of number of deaths/missing (Mediterraean vs. rest of the world)
############################################################################
MM %>% 
  filter(Reported.Year<2018) %>% 
  mutate(Region.of.Incident=ifelse(Region.of.Incident=="Mediterranean","Mediterranean", "Rest of the world"),
         Month=paste(Reported.Month, Reported.Year, sep= " ") %>% 
           parse_date("%b %Y",locale=locale("en"))) %>% 
  group_by(Region.of.Incident, Month) %>% 
  summarise(DeadMiss=sum(Total.Dead.and.Missing)) -> df2

hchart(df2, "spline", hcaes(x = Month, y = DeadMiss, group = Region.of.Incident)) %>% 
  hc_xAxis(title = list(text = "Month")) %>% 
  hc_yAxis(title = list(text = "Total")) %>% 
  hc_title(text = "Migrant deaths or missing: Mediterranean vs. rest of the world") %>% 
  hc_subtitle(text = "January 2014 to December 2017") %>% 
  hc_credits(enabled = TRUE, # add credits
             text = "<b>Data source:</b> Missing Migrants Project",
             href = "https://missingmigrants.iom.int/")

############################################################################
# Shape of the time series
############################################################################
MM %>% 
  filter(Reported.Year<2018) %>% 
  filter(Region.of.Incident=="Mediterranean") %>% 
  mutate(Month=paste(Reported.Month, Reported.Year, sep= " ") %>% 
           parse_date("%b %Y",locale=locale("en"))) %>% 
  group_by(Month) %>% 
  summarise(DeadMiss=sum(Total.Dead.and.Missing)) -> df3

zoo::read.zoo(as.data.frame(df3)) %>% 
  ts(frequency=12) %>% 
  stl("per") %>% 
  hchart() %>% 
  hc_title(text = "Migrant deaths and missing in Mediterranean Area") %>% 
  hc_subtitle(text = "Time series decomposition. January 2014 to December 2017") %>% 
  hc_xAxis(title = list(text = "Month")) %>% 
  hc_credits(enabled = TRUE,
             text = "<b>Data source:</b> Missing Migrants Project",
             href = "https://missingmigrants.iom.int/")

############################################################################
# Location of incidents
############################################################################
MM %>% 
  filter(Reported.Year==2017) %>% 
  mutate(lat=str_split_fixed(Location, ",", n=2)[ , 1] %>% as.numeric,
         lon=str_split_fixed(Location, ",", n=2)[ , 2] %>% as.numeric,
         popup=paste0("<b>Region of incident:</b> ", Region.of.Incident,
                      "<br><b>Reported date:</b> ", Reported.Date,
                       "<br><b>Number dead:</b> ", Number.Dead,
                      "<br><b>Number missing:</b> ", Number.Missing,
                      "<br><b>Total dead and missing:</b> ", Total.Dead.and.Missing,
                      "<br><b>Number of survivors:</b> ", Number.of.survivors,   
                      "<br><b>Number of female:</b> ", Number.of.Female,
                      "<br><b>Number of male:</b> ", Number.of.Male,
                      "<br><b>Number of children:</b> ", Number.of.Children,        
                      "<br><b>Cause of death:</b> ", Cause.of.death,
                      "<br><b>Location description:</b> ", Location.Description,
                      paste0("<br><b>Information source: </b> ","<a href=", URL,">", Information.Source, "</a>"),
                      "<br><b>Location:</b> ", Location,
                      "<br><b>Information reliability:</b> ", Information.Reliability,
                      "<br><b>URL:</b> ", URL,
                      "<br><b>Latitude:</b> ", lat,
                      "<br><b>Longitude:</b> ", lon)) -> df4

coordinates(df4) = ~lon+lat

leaflet() %>% addTiles() %>%
  addMarkers(data=df4,
             popup=~popup,
             clusterOptions = markerClusterOptions())


