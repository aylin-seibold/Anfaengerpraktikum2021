## Anfängerpraktikum WiSe 2021/22
## Covid-19 Infektionen und Hospitalisierung 
## Autoren: Xiao Fu, Vaishali Iyer, Richard Pernerstorfer, Aylin Seibold  

## Anfängerpraktikum WiSe 2021/22  
## B.Sc Statistik 
## LMU München 
## Institut für Statistik 

## Projektpartnerin: Frau Yeganeh Khazaei (Institut für Statistik: StaBLab) 
## Betreuer: Herr Patrick Kaiser 

## Datum: München, den 11.01.2022



### Dieses Skript wird in jeden unserer folgenden R-Files als erstes durchgeführt 
### (durch den Befehl: source("Librarys und Datensaetze"))
### und ist somit die Grundlage für das Erstellen unserer Subdatensätze und Graphiken.


### Ausführen der Librarys 

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



### Einlesen der Datensätze  

# RKI_COVID19.csv
# Datensatz für Covid-Neuinfektionen 
RKI_COVID19 <- read.csv(unz("RKI_COVID19.zip", "RKI_COVID19.csv"), fileEncoding = "UTF-8")
RKI_COVID19[,9] <- as.Date(RKI_COVID19[,9]) 


# RKI_Covid19.csv: 
# Subdatensatz mit den Variablen AnzahlFall und Meldedatum
RKI_Infektionen <- aggregate(RKI_COVID19$AnzahlFall, RKI_COVID19[9], sum)
colnames(RKI_Infektionen) <- c("Datum" , "Infektionen")
RKI_Infektionen$Datum <- as.Date(RKI_Infektionen$Datum)


# DIVI-Intensivregister_2021-11-17_12-15.csv
# Datensatz für Belegung der Betten
divi_17_11 <- read_csv("DIVI-Intensivregister_2021-11-17_12-15.csv")


# Bd_Inz.xlsx  
# In Excel modifizierter Datensatz. Ursprünglicher Datensatz: Fallzahlen_Kum_Tab.xlsx
# Datensatz für 7-Tage-Inzidenz für Covid-Infektionen
sieben_tage_inzidenz <- as.data.frame(read_excel("Bd_Inz.xlsx", na="NA", sheet="Sheet2"))
sieben_tage_inzidenz[,1] <-as.Date(sieben_tage_inzidenz[,1])


# germany_vaccinations_timeseries_v2.tsv
# Datensatz für Impfquoten
vaccination_timeseries <-read_tsv("germany_vaccinations_timeseries_v2.tsv")
myformat2<-"%Y-%m-%d"
vaccination_timeseries$date<-as.Date(vaccination_timeseries$date,myformat2)


# Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv
# Datensatz für 7-Tage-Inzidenz für Covid-Hospitalisierungen
Aktuell_Deutschland_COVID_19_Hospitalisierungen <-read.csv("Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")


