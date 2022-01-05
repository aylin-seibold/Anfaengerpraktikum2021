# Anfängerpraktikum WiSe 2021/22
# Covid-19 Infektionen und Hospitalisierung 
# Autoren: Xiao Fu, Vaishali Iyer, Richard Pernerstorfer, Aylin Seibold  

## Anfängerpraktikum WiSe 2021/22  
## B.Sc Statistik 
## LMU München 
## Institut für Statistik 

## Projektpartnerin: Frau Yeganeh Khazaei (Institut für Statistik: StaBLab) 
## Betreuer: Herr Patrick Kaiser 

## Datum: München, den 23.12.2021
## Instruktionen:
## 1. Gehen sie sicher, dass sie alle librarys installiert haben
## 2. Erstellen sie ein R-Projekt mit der Github URL "https://github.com/aylin-seibold/Anfaengerpraktikum2021"
## 3. Führen sie dieses File aus
### Librarys laden

library(dplyr)
library(ggplot2)
library(ggpubr)
library(readr)
library(vroom)
library(rio)
library(stringr)
library(tidyr)
library(tidyverse)
library(lubridate)
library (readxl)
library(pals)

### Datensätze einlesen 

### RKI_COVID19 Datensatz
RKI_COVID19 <- read.csv(unz("RKI_COVID19.zip", "RKI_COVID19.csv"), fileEncoding = "UTF-8")
RKI_COVID19[,9] <- as.Date(RKI_COVID19[,9]) 

### Subdatensatz RKI_Covid19
RKI_Infektionen <- aggregate(RKI_COVID19$AnzahlFall, RKI_COVID19[9], sum)
colnames(RKI_Infektionen) <- c("Datum" , "Infektionen")
RKI_Infektionen$Datum <- as.Date(RKI_Infektionen$Datum)


### DIVI_Intensivregister Datensatz
divi_17_11 <- read_csv("DIVI-Intensivregister_2021-11-17_12-15.csv")


### RKI Datensatz mit 7-Tage-Inzidenz für Covid-Infektionen für die Bundesländer Deutschlands
sieben_tage_inzidenz <- as.data.frame(read_excel("Bd_Inz.xlsx", na="NA", sheet="Sheet2"))
sieben_tage_inzidenz[,1] <-as.Date(sieben_tage_inzidenz[,1])


### Impfquoten Datensatz
vaccination_timeseries <-read_tsv("germany_vaccinations_timeseries_v2.tsv")
myformat2<-"%Y-%m-%d"
vaccination_timeseries$date<-as.Date(vaccination_timeseries$date,myformat2)


### Hospitalisierungs Datensatz
Aktuell_Deutschland_COVID_19_Hospitalisierungen <-read.csv("Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")