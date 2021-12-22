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



### Grafiken
### Folie 8
axiscolors <- c("black","black","black","black","black","black","black",
                "black","black","black","black","red","black","black","black",
                "black","black","black","black","black","black","black","black", "red")


ggplot(data = subset(RKI_Infektionen, Datum >= "2020-03-01"), aes(x = Datum, y = (Infektionen / 83129285) *100000)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen pro 100.000 Einwohner", title = "Covid-Infektionen pro 100.000 Einwohner in Deutschland") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  theme(axis.text.x = element_text(color=axiscolors)) 

### Folie 9
axiscolors <- c("black","black","black","black","black","black","black",
                "black","black","black","black","red","black","black","black",
                "black","black","black","black","black","black","black","black", "red")

color_code1 = c("Bayern" = "blue", "Deutschland" = "black", "Bremen" = "darkgreen", "Sachsen" = "red", "Andere" = "grey80")

ggplot(sieben_tage_inzidenz, aes(x = sieben_tage_inzidenz[,1])) +
  geom_line(aes(y = sieben_tage_inzidenz[,2], col = "Andere"), alpha = 0.5,size = 1.5) +
  geom_line(aes(y = Hessen, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Berlin, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = sieben_tage_inzidenz[,15], col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Saarland, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Brandenburg, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Niedersachsen, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = sieben_tage_inzidenz[,9], col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = sieben_tage_inzidenz[,11], col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Hamburg, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = sieben_tage_inzidenz[,12], col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Thüringen, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = sieben_tage_inzidenz[,16], col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Sachsen, col = "Sachsen"), size = 1.5) +
  geom_line(aes(y = Bremen, col = "Bremen"), size = 1.5) +
  geom_line(aes(y = Bayern, col ="Bayern" ), size = 1.5) + 
  geom_line(aes(y = Gesamt, col = "Deutschland"), size = 1.5) +
  scale_colour_manual(values = color_code1, name ="", breaks = c("Deutschland", "Bayern", "Sachsen", "Bremen", "Andere")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d. %b %y") +
  expand_limits(x = as.Date(c("2020-03-01", "2021-12-01"))) +
  labs(x = "Datum", y = "7-Tage-Inzidenz für Covid-Infektionen",
       title = "7-Tage-Inzidenz für Covid-Infektionen in Deutschland und Bundesländern") +
  
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.x = element_text(color=axiscolors)) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +  
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white")) 

### Folie 10
# 7 -Tage Hospitalisierung pro 100000 einwohner in verschiedenen Bundesländer
# Einwohnerzahl aus: https://de.wikipedia.org/wiki/Liste_der_deutschen_Bundesl%C3%A4nder_nach_Bev%C3%B6lkerung Jahr 2020
# Deutschland Bevölkerung: https://en.wikipedia.org/wiki/Demographics_of_Germany


# Subdatensätze erstellen für jedes Bundesland und Deutschland 
hosp <- as.data.frame(Aktuell_Deutschland_COVID_19_Hospitalisierungen)

hosp_7tage_schleswig_holstein <- subset(hosp, Bundesland_Id == "1")
hosp_7tage_hamburg <- subset(hosp, Bundesland_Id == "2")
hosp_7tage_niedersachsen <- subset(hosp, Bundesland_Id == "3")
hosp_7tage_bremen <- subset(hosp, Bundesland_Id == "4")
hosp_7tage_nordrhein_westfalen <- subset(hosp, Bundesland_Id == "5")
hosp_7tage_hessen <- subset(hosp, Bundesland_Id == "6")
hosp_7tage_rheinland_pfalz <- subset(hosp, Bundesland_Id == "7")
hosp_7tage_baden_Wuerttemberg <- subset(hosp, Bundesland_Id == "8")
hosp_7tage_bayern <- subset(hosp, Bundesland_Id == "9")
hosp_7tage_saarland <- subset(hosp, Bundesland_Id == "10")
hosp_7tage_berlin<- subset(hosp, Bundesland_Id == "11")
hosp_7tage_brandenburg<- subset(hosp, Bundesland_Id == "12")
hosp_7tage_mecklenburg_Vorpommern<- subset(hosp, Bundesland_Id == "13")
hosp_7tage_sachsen<- subset(hosp, Bundesland_Id == "14")
hosp_7tage_sachsen_anhalt<- subset(hosp, Bundesland_Id == "15")
hosp_7tage_thueringen<- subset(hosp, Bundesland_Id == "16")
hosp_7tage_deutschland <- subset(hosp, Bundesland_Id == "0")

# Hospitalisieurngsfaelle Bremen, Sachsen , Bayern & Deustchland

bremen_hospit <- aggregate(hosp_7tage_bremen$`X7T_Hospitalisierung_Faelle`, hosp_7tage_bremen[1], sum)
bremen_hospit$Datum <- as.Date(bremen_hospit$Datum)

bayern_hospit <- aggregate(hosp_7tage_bayern$`X7T_Hospitalisierung_Faelle`, hosp_7tage_bayern[1], sum)
bayern_hospit$Datum <- as.Date(bayern_hospit$Datum)

sachsen_hospit <- aggregate(hosp_7tage_sachsen$`X7T_Hospitalisierung_Faelle`, hosp_7tage_sachsen[1], sum)
sachsen_hospit$Datum <- as.Date(sachsen_hospit$Datum)

deutschland_hospt <- aggregate(hosp_7tage_deutschland$`X7T_Hospitalisierung_Faelle`, hosp_7tage_deutschland[1], sum)
deutschland_hospt$Datum <- as.Date(deutschland_hospt$Datum)


# andere Bundesländer Hospitalisierungsfälle

schleswig_holstein_hospit <- aggregate(hosp_7tage_schleswig_holstein$`X7T_Hospitalisierung_Faelle`, hosp_7tage_schleswig_holstein[1], sum)
schleswig_holstein_hospit$Datum <- as.Date(schleswig_holstein_hospit$Datum)


hamburg_hospit <- aggregate(hosp_7tage_hamburg$`X7T_Hospitalisierung_Faelle`, hosp_7tage_hamburg[1], sum)
hamburg_hospit$Datum <- as.Date(hamburg_hospit$Datum)


niedersachsen_hospit <- aggregate(hosp_7tage_niedersachsen$`X7T_Hospitalisierung_Faelle`, hosp_7tage_niedersachsen[1], sum)
niedersachsen_hospit$Datum <- as.Date(niedersachsen_hospit$Datum)


nordrhein_westfalen_hospit <- aggregate(hosp_7tage_nordrhein_westfalen$`X7T_Hospitalisierung_Faelle`, hosp_7tage_nordrhein_westfalen[1], sum)
nordrhein_westfalen_hospit$Datum <- as.Date(nordrhein_westfalen_hospit$Datum)


hessen_hospit <- aggregate(hosp_7tage_hessen$`X7T_Hospitalisierung_Faelle`, hosp_7tage_hessen[1], sum)
hessen_hospit$Datum <- as.Date(hessen_hospit$Datum)


rheinland_pfalz_hospit <- aggregate(hosp_7tage_rheinland_pfalz$`X7T_Hospitalisierung_Faelle`, hosp_7tage_rheinland_pfalz[1], sum)
rheinland_pfalz_hospit$Datum <- as.Date(rheinland_pfalz_hospit$Datum)


baden_Wuerttemberg_hospit <- aggregate(hosp_7tage_baden_Wuerttemberg$`X7T_Hospitalisierung_Faelle`, hosp_7tage_baden_Wuerttemberg[1], sum)
baden_Wuerttemberg_hospit$Datum <- as.Date(baden_Wuerttemberg_hospit$Datum)


saarland_hospit <- aggregate(hosp_7tage_saarland$`X7T_Hospitalisierung_Faelle`, hosp_7tage_saarland[1], sum)
saarland_hospit$Datum <- as.Date(saarland_hospit$Datum)


berlin_hospit <- aggregate(hosp_7tage_berlin$`X7T_Hospitalisierung_Faelle`, hosp_7tage_berlin[1], sum)
berlin_hospit$Datum <- as.Date(berlin_hospit$Datum)


brandenburg_hospit <- aggregate(hosp_7tage_brandenburg$`X7T_Hospitalisierung_Faelle`, hosp_7tage_brandenburg[1], sum)
brandenburg_hospit$Datum <- as.Date(brandenburg_hospit$Datum)


mecklenburg_Vorpommern_hospit <- aggregate(hosp_7tage_mecklenburg_Vorpommern$`X7T_Hospitalisierung_Faelle`, hosp_7tage_mecklenburg_Vorpommern[1], sum)
mecklenburg_Vorpommern_hospit$Datum <- as.Date(mecklenburg_Vorpommern_hospit$Datum)


sachsen_anhalt_hospit <- aggregate(hosp_7tage_sachsen_anhalt$`X7T_Hospitalisierung_Faelle`, hosp_7tage_sachsen_anhalt[1], sum)
sachsen_anhalt_hospit$Datum <- as.Date(sachsen_anhalt_hospit$Datum)


thueringen_hospit <- aggregate(hosp_7tage_thueringen$`X7T_Hospitalisierung_Faelle`, hosp_7tage_thueringen[1], sum)
thueringen_hospit$Datum <- as.Date(thueringen_hospit$Datum)


## Jahreswechsel in rot markieren: x- Achse Farben

axiscolors <- c("black","black","black","black","black","black","black",
                "black","black","black","black","red","black","black","black",
                "black","black","black","black","black","black","black","black", "red")


## Plot 1: nur Sachsen, Bremen, Bayern und Deutschland farbig markiert

color_code2 <- c("Deutschland" = "black", "Bayern" = "blue",  "Bremen" = "darkgreen", "Sachsen" = "red", "Andere" = "grey80")

ggplot()+
  geom_line( size = 1, data = berlin_hospit, mapping = aes(x = Datum, y = ((x/ 3664088) *100000)/2 , col = "Andere")) +
  geom_line( size = 1, data = brandenburg_hospit, mapping = aes(x = Datum, y = ((x/2531071) *100000)/2 , col = "Andere")) +
  geom_line( size = 1, data = hamburg_hospit, mapping = aes(x = Datum, y = ((x/ 1852478) *100000)/2, col = "Andere"))+
  geom_line( size = 1, data = hessen_hospit, mapping = aes(x = Datum, y = ((x/6293154) *100000)/2, col = "Andere" )) +
  geom_line( size = 1, data = mecklenburg_Vorpommern_hospit, mapping = aes(x = Datum, y = ((x/1610774) *100000)/2 , col = "Andere")) +
  geom_line( size = 1, data = niedersachsen_hospit, mapping = aes(x = Datum, y = ((x/ 8003421) *100000)/2, col = "Andere")) +
  geom_line( size = 1, data = nordrhein_westfalen_hospit, mapping = aes(x = Datum, y = ((x/17925570) *100000)/2 , col = "Andere")) +
  geom_line( size = 1, data = rheinland_pfalz_hospit, mapping = aes(x = Datum, y = ((x/4098391) *100000)/2, col = "Andere" )) +
  geom_line( size = 1, data = saarland_hospit, mapping = aes(x = Datum, y = ((x/ 983991) *100000)/2, col = "Andere")) +
  geom_line( size = 1, data = sachsen_anhalt_hospit, mapping = aes(x = Datum, y = ((x/ 2180684) *100000)/2 , col = "Andere")) +
  geom_line( size = 1, data = schleswig_holstein_hospit, mapping = aes(x = Datum, y = ((x/2910875) *100000)/2 , col = "Andere")) +
  geom_line( size = 1, data = thueringen_hospit, mapping = aes(x = Datum, y = ((x/ 2120237) *100000)/2, col = "Andere"))+
  geom_line( size = 1, data = baden_Wuerttemberg_hospit, mapping = aes(x = Datum, y = ((x/11103043) *100000)/2, col = "Andere")) +
  geom_line( size = 2, data = sachsen_hospit, mapping = aes(x = Datum, y = ((x/4056941) *100000)/2 , col = "Sachsen")) +
  geom_line( size = 2, data = bremen_hospit, mapping = aes(x = Datum, y = ((x/ 680130) *100000)/2, col = "Bremen")) +
  geom_line( size = 2, data = bayern_hospit, mapping = aes(x = Datum, y = ((x/13140183) *100000)/2 , col = "Bayern")) +
  geom_line( size = 2, data = deutschland_hospt, mapping = aes(x = Datum, y = ((x/ 83129285) *100000)/2, col = "Deutschland")) +
  
  labs(x = "Datum", y = "7-Tage-Inzidenz für Covid-Hospitalisierungen", title = "7-Tage-Inzidenz für Covid-Hospitalisierungen in Deutschland und Bundesländern") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d. %b %y") +  
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  scale_color_manual(name = " ", values = color_code2) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  theme(axis.text.x = element_text(color=axiscolors)) 

### Folie 11

axiscolors <- c("black","black","black","black","black","black","black",
                "black","black","black","black","red","black","black","black",
                "black","black","black","black","black","black","black","black", "red")
color_code<-c("Impfquote erst"="lightgreen",
              "Impfquote voll"="darkgreen")
plot_impfquote<-ggplot()+ 
  geom_line(aes(x =date,y=impf_quote_erst,col="Impfquote erst"), linetype=1,size=2,data = vaccination_timeseries)+
  geom_line(aes(x =date,y=impf_quote_voll,col="Impfquote voll"), linetype=1,size=2,data = vaccination_timeseries)+
  labs(x = "Datum", y = "Anteil der geimpften Bevölkerung", title = "Impfquote in Deutschland")+
  scale_x_date(date_breaks = "1 month", date_labels = "%d. %b %y")+  
  scale_color_manual(name = " ", values = color_code)+
  ylim(0,1)+
  geom_vline(xintercept= as.Date(c("2021-10-01")), size = 2)+
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold",color =axiscolors )) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +  
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))+ 
  expand_limits(x = as.Date(c("2020-03-01", "2021-12-01")))
plot_impfquote
### Folie 12

sieben_tage_inzidenz_oktober <- subset(sieben_tage_inzidenz, sieben_tage_inzidenz[,1] >= "2021-10-01")

color_code2 = c("Bayern (Impfquote: 70,0%)" = "blue", "Deutschland (Impfquote: 72,4%)" = "black", "Bremen (Impfquote: 84,3%)" = "darkgreen", "Sachsen (Impfquote: 61,6%)" = "red", "Andere" = "grey80")

ggplot(sieben_tage_inzidenz_oktober, aes(x = sieben_tage_inzidenz_oktober[,1])) +
  geom_line(aes(y = sieben_tage_inzidenz_oktober[,2], col = "Andere"), alpha = 0.5,size = 1.5) +
  geom_line(aes(y = Hessen, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Berlin, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = sieben_tage_inzidenz_oktober[,15], col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Saarland, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Brandenburg, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Niedersachsen, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = sieben_tage_inzidenz_oktober[,9], col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = sieben_tage_inzidenz_oktober[,11], col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Hamburg, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = sieben_tage_inzidenz_oktober[,12], col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Thüringen, col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = sieben_tage_inzidenz_oktober[,16], col = "Andere"), alpha = 0.5, size = 1.5) +
  geom_line(aes(y = Sachsen, col = "Sachsen (Impfquote: 61,6%)"), size = 1.5) +
  geom_line(aes(y = Bremen, col = "Bremen (Impfquote: 84,3%)"), size = 1.5) +
  geom_line(aes(y = Bayern, col ="Bayern (Impfquote: 70,0%)" ), size = 1.5) + 
  geom_line(aes(y = Gesamt, col = "Deutschland (Impfquote: 72,4%)"), size = 1.5) +
  scale_colour_manual(values = color_code2, name ="", breaks = c("Deutschland (Impfquote: 72,4%)", "Sachsen (Impfquote: 61,6%)", "Bayern (Impfquote: 70,0%)","Bremen (Impfquote: 84,3%)", "Andere")) +
  
  expand_limits(x = as.Date(c("2021-10-01", "2021-12-01"))) +
  scale_x_date( date_labels = "%d. %b %y") +
  labs(x = "Datum", y = "7-Tage-Inzidenz für Covid-Infektionen",
       title = "7-Tage-Inzidenz für Covid-Infektionen in Deutschland und Bundesländern ab Oktober 2021") +
  
  
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white")) 

### Folie 13
# Subdatensätze für alle Bundesländer und Deutschland für Oktober

bremen_hospit_ab_11_Okt <-  subset(bremen_hospit, Datum >= "2021-10-11"  )
bayern_hospit_ab_11_Okt <-  subset(bayern_hospit, Datum >= "2021-10-11"  )
sachsen_hospit_ab_11_Okt <-  subset(sachsen_hospit, Datum >= "2021-10-11" )
berlin_hospit_ab_11_Okt <-  subset(berlin_hospit, Datum >= "2021-10-11" )
brandenburg_hospit_ab_11_Okt <-  subset(brandenburg_hospit, Datum >= "2021-10-11" )
hamburg_hospit_ab_11_Okt <-  subset(hamburg_hospit, Datum >= "2021-10-11" )
hessen_hospit_ab_11_Okt <-  subset(hessen_hospit, Datum >= "2021-10-11" )
mecklenburg_Vorpommern_hospit_ab_11_Okt <-  subset( mecklenburg_Vorpommern_hospit, Datum >= "2021-10-11" )
niedersachsen_hospit_ab_11_Okt <-  subset(niedersachsen_hospit, Datum >= "2021-10-11" )
nordrhein_westfalen_hospit_ab_11_Okt <-  subset(nordrhein_westfalen_hospit, Datum >= "2021-10-11" )
rheinland_pfalz_hospit_ab_11_Okt <-  subset(rheinland_pfalz_hospit, Datum >= "2021-10-11" )
saarland_hospit_ab_11_Okt <-  subset(saarland_hospit, Datum >= "2021-10-11" )
sachsen_anhalt_hospit_ab_11_Okt <-  subset(sachsen_anhalt_hospit, Datum >= "2021-10-11" )
thueringen_hospit_ab_11_Okt <-  subset(thueringen_hospit, Datum >= "2021-10-11" )
baden_Wuerttemberg_hospit_ab_11_Okt <-  subset(baden_Wuerttemberg_hospit, Datum >= "2021-10-11" )
deutschland_hospt_ab_11_Okt <-  subset(deutschland_hospt, Datum >= "2021-10-11" )
schleswig_holstein_hospit_ab_11_Okt <-  subset(schleswig_holstein_hospit, Datum >= "2021-10-11" )

# Farben erstellen für die Bundesländer

color_code3 <- c("Deutschland (Impfquote: 72,4%)" = "black", "Sachsen (Impfquote: 61,6%)" = "red", "Bayern (Impfquote: 70,0%)" = "blue",
                 "Bremen (Impfquote: 84,3%)" = "darkgreen", 
                 "Andere"="grey80")

ggplot()+
  geom_line( size = 1,data = berlin_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/ 3664088) *100000)/2 , col = "Andere")) +
  geom_line( size = 1,data = brandenburg_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/2531071) *100000)/2 , col = "Andere")) +
  geom_line( size = 1, data = hamburg_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/ 1852478) *100000)/2, col = "Andere")) +
  geom_line( size = 1,data = hessen_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/6293154) *100000)/2, col = "Andere" )) +
  geom_line( size = 1,data = mecklenburg_Vorpommern_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/1610774) *100000)/2 , col = "Andere")) +
  geom_line( size = 1, data = niedersachsen_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/ 8003421) *100000)/2, col = "Andere")) +
  geom_line( size = 1,data = nordrhein_westfalen_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/17925570) *100000)/2 , col = "Andere")) +
  geom_line( size = 1,data = rheinland_pfalz_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/4098391) *100000)/2, col = "Andere" )) +
  geom_line( size = 1, data = saarland_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/ 983991) *100000)/2, col = "Andere"))+
  geom_line( size = 1,data = sachsen_anhalt_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/ 2180684) *100000)/2 , col = "Andere")) +
  geom_line( size = 1,data = schleswig_holstein_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/2910875) *100000)/2 , col = "Andere")) +
  geom_line( size = 1, data = thueringen_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/ 2120237) *100000)/2, col = "Andere")) +
  geom_line( size = 1,data = baden_Wuerttemberg_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/11103043) *100000)/2, col = "Andere")) +
  geom_line( size = 2,data = sachsen_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/4056941) *100000)/2 , col = "Sachsen (Impfquote: 61,6%)")) +
  geom_line( size = 2,data = bayern_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/13140183) *100000)/2 , col = "Bayern (Impfquote: 70,0%)")) +
  geom_line( size = 2, data = bremen_hospit_ab_11_Okt, mapping = aes(x = Datum, y = ((x/ 680130) *100000)/2, col = "Bremen (Impfquote: 84,3%)")) +
  geom_line( size = 2, data = deutschland_hospt_ab_11_Okt, mapping = aes(x = Datum, y = ((x/ 83129285) *100000)/2, col = "Deutschland (Impfquote: 72,4%)")) +
  
  labs(x = "Datum", y = "7-Tage-Inzidenz für Covid-Hospitalisierungen", title = "7-Tage-Inzidenz für Covid-Hospitalisierungen\nin Deutschland und Bundesländern ab Oktober 2021") +
  scale_x_date( date_labels = "%d. %b %y") +  
  expand_limits(x = as.Date(c("2021-10-01", "2021-12-01"))) +
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  scale_color_manual(name = " ", values = color_code3)+
  theme( panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                         colour = "grey"), 
         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                         colour = "white")) 

### Event1 
# Subdatensätze erstellen
data_BLM_germany <- subset(RKI_COVID19, Meldedatum >= "2020-05-06" & Meldedatum <= "2020-08-06" )
data_BLM_bayern <- subset(data_BLM_germany, IdBundesland == "9" )
data_BLM_muenchen <- subset(data_BLM_bayern, Landkreis == "SK MÃ¼nchen")
data_BLM_bayern_ohne_muenchen  <- subset(data_BLM_bayern, Landkreis != "SK MÃ¼nchen")
data_BLM_germany <- aggregate(data_BLM_germany$AnzahlFall, data_BLM_germany[9], sum)
data_BLM_bayern <- aggregate(data_BLM_bayern$AnzahlFall, data_BLM_bayern[9], sum)
data_BLM_muenchen <- aggregate(data_BLM_muenchen$AnzahlFall, data_BLM_muenchen[9], sum)
data_BLM_muenchen <- rbind(data_BLM_muenchen, c("2020-06-01", 0))
data_BLM_muenchen <- data_BLM_muenchen[order(data_BLM_muenchen[,1]),]
data_BLM_bayern_ohne_muenchen <- aggregate(data_BLM_bayern_ohne_muenchen$AnzahlFall, data_BLM_bayern_ohne_muenchen[9], sum)
data_BLM_muenchen$x <- as.numeric(data_BLM_muenchen$x)
data_BLM_muenchen$anteil <- data_BLM_muenchen$x / data_BLM_bayern$x

divi_BLM <- subset(divi_17_11,date >= "2020-05-06" & date <= "2020-08-06" & bundesland == "09" )
divi_muenchen <- subset(divi_BLM, gemeindeschluessel == "09162")
divi_muenchen <- aggregate(divi_muenchen$faelle_covid_aktuell_invasiv_beatmet, divi_muenchen[1], sum)
divi_BLM <- aggregate(divi_BLM$faelle_covid_aktuell_invasiv_beatmet, divi_BLM[1], sum)
divi_BLM$invasiv_anteil <- divi_muenchen$x / divi_BLM$x

# Plot Covid-Infektionen pro 100.000 Einwohner München während BLM
BLM_muenchen_plot <- ggplot(data = data_BLM_muenchen, aes(x = Meldedatum, y = x / 14.72)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die BLM-Demo in München") +
  geom_vline(xintercept= as.Date(c("2020-06-06")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-06-13")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )

# Plot Covid-Infektionen pro 100.000 Einwohner Bayern ohne München während BLM
BLM_bayern_ohne_muenchen_plot <- ggplot(data = data_BLM_bayern_ohne_muenchen, aes(x = Meldedatum, y = x / 130.8)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die BLM-Demo in Rest-Bayern") +
  geom_vline(xintercept= as.Date(c("2020-06-06")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-06-13")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23)) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )

#Zusammenhängende Grafik
ggarrange(BLM_muenchen_plot, BLM_bayern_ohne_muenchen_plot, nrow = 2)

# Fälle Anteil München an Bayern während BLM
ggplot(data = data_BLM_muenchen, aes(y = anteil, x = Meldedatum)) + 
  geom_line(size = 2, color = "black") +
  labs(x = "Datum", y = "Anteil der Covid-Infektionen", title = "Anteil der münchner Covid-Infektionen an Bayern um die BLM-Demo") +
  geom_vline(xintercept= as.Date(c("2020-06-06")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-06-13")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)

# Belegte Intensivbetten Anteil München an Bayern während BLM
ggplot(data = divi_BLM, aes(x = date, y = invasiv_anteil)) +
  geom_line(stat="identity", size = 2) + 
  labs(x = "Datum", y = "Anteil der invasiv-beatmeten\nCovid-Patienten", title = "Anteil der invasiv-beatmeten Covid-Patienten Münchens an Bayern um die BLM-Demo") +
  geom_vline(xintercept= as.Date(c("2020-06-06")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-06-20")), color = "violet", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") +  
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)

# datensätze vernichten
divi_BLM <- NULL
divi_muenchen <- NULL
data_BLM_germany <- NULL
data_BLM_bayern <- NULL
data_BLM_muenchen <- NULL
data_BLM_bayern_ohne_muenchen <- NULL

### Events2
data_CovidDemo_germany <- subset(RKI_COVID19, Meldedatum >= "2020-08-12" & Meldedatum <= "2020-11-12" )
data_CovidDemo_bayern <- subset(data_CovidDemo_germany, IdBundesland == "9" )
data_CovidDemo_muenchen <- subset(data_CovidDemo_bayern, Landkreis == "SK MÃ¼nchen")
data_CovidDemo_bayern_ohne_muenchen  <- subset(data_CovidDemo_bayern, Landkreis != "SK MÃ¼nchen")
data_CovidDemo_germany <- aggregate(data_CovidDemo_germany$AnzahlFall, data_CovidDemo_germany[9], sum)
data_CovidDemo_bayern <- aggregate(data_CovidDemo_bayern$AnzahlFall, data_CovidDemo_bayern[9], sum)
data_CovidDemo_muenchen <- aggregate(data_CovidDemo_muenchen$AnzahlFall, data_CovidDemo_muenchen[9], sum)
data_CovidDemo_bayern_ohne_muenchen <- aggregate(data_CovidDemo_bayern_ohne_muenchen$AnzahlFall, data_CovidDemo_bayern_ohne_muenchen[9], sum)
data_CovidDemo_muenchen$anteil <- data_CovidDemo_muenchen$x / data_CovidDemo_bayern$x
divi_CovidDemo <- subset(divi_17_11,date >= "2020-08-12" & date <= "2020-11-12" & bundesland == "09" )
divi_muenchen <- subset(divi_CovidDemo, gemeindeschluessel == "09162")
divi_muenchen <- aggregate(divi_muenchen$faelle_covid_aktuell_invasiv_beatmet, divi_muenchen[1], sum)
divi_CovidDemo <- aggregate(divi_CovidDemo$faelle_covid_aktuell_invasiv_beatmet, divi_CovidDemo[1], sum)
divi_CovidDemo$invasiv_anteil <- divi_muenchen$x / divi_CovidDemo$x

# Fälle CovidDemo München
CovidDemo_muenchen_plot <- ggplot(data = data_CovidDemo_muenchen, aes(x = Meldedatum, y = x / 14.72)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die Corona-Demo in München") +
  geom_vline(xintercept= as.Date(c("2020-09-12")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-09-19")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )


# Fälle CovidDemo Bayern ohne München 
CovidDemo_bayern_ohne_muenchen_plot <- ggplot(data = data_CovidDemo_bayern_ohne_muenchen, aes(x = Meldedatum, y = x / 130.8)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die Corona-Demo in Rest-Bayern") +
  geom_vline(xintercept= as.Date(c("2020-09-12")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-09-19")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))


#Zusammenhängende Grafik
ggarrange(CovidDemo_muenchen_plot, CovidDemo_bayern_ohne_muenchen_plot, nrow = 2)

# Fälle CovidDemo Anteil München an Bayern
ggplot(data = data_CovidDemo_muenchen, aes(y = anteil, x = Meldedatum)) + 
  geom_line(size = 2, color = "black") +
  labs(x = "Datum", y = "Anteil der Covid-Infektionen", title = "Anteil der münchner Covid-Infektionen an Bayern um die Corona-Demo") +
  geom_vline(xintercept= as.Date(c("2020-09-12")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-09-19")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )+
  ylim(0,1)


# belegte Intensivbetten Prozent
ggplot(data = divi_CovidDemo, aes(x = date, y = invasiv_anteil)) +
  geom_line(stat="identity", size = 2) + 
  labs(x = "Datum", y = "Anteil der invasiv-beatmeten\nCovid-Patienten", title = "Anteil der invasiv-beatmeten Covid-Patienten Münchens an Bayern um die Corona-Demo") +
  geom_vline(xintercept= as.Date(c("2020-09-12")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-09-26")), color = "violet", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") +  
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )+
  ylim(0,1)

# datensätze vernichten
divi_CovidDemo <- NULL
divi_muenchen <- NULL
data_CovidDemo_germany <- NULL
data_CovidDemo_bayern <- NULL
data_CovidDemo_muenchen <- NULL
data_CovidDemo_bayern_ohne_muenchen <- NULL


### Event3
# Datensätze erstellen
data_EM_germany <- subset(RKI_COVID19, Meldedatum >= "2021-05-11" & Meldedatum <= "2021-09-11" )
data_EM_bayern <- subset(data_EM_germany, IdBundesland == "9" )
data_EM_muenchen <- subset(data_EM_bayern, Landkreis == "SK MÃ¼nchen")
data_EM_bayern_ohne_muenchen  <- subset(data_EM_bayern, Landkreis != "SK MÃ¼nchen")
data_EM_germany <- aggregate(data_EM_germany$AnzahlFall, data_EM_germany[9], sum)
data_EM_bayern <- aggregate(data_EM_bayern$AnzahlFall, data_EM_bayern[9], sum)
data_EM_muenchen <- aggregate(data_EM_muenchen$AnzahlFall, data_EM_muenchen[9], sum)
data_EM_bayern_ohne_muenchen <- aggregate(data_EM_bayern_ohne_muenchen$AnzahlFall, data_EM_bayern_ohne_muenchen[9], sum)
data_EM_muenchen$anteil <- data_EM_muenchen$x / data_EM_bayern$x
divi_EM <- subset(divi_17_11,date >= "2021-05-11" & date <= "2021-09-11" & bundesland == "09" )
divi_muenchen <- subset(divi_EM, gemeindeschluessel == "09162")
divi_muenchen <- aggregate(divi_muenchen$faelle_covid_aktuell_invasiv_beatmet, divi_muenchen[1], sum)
divi_EM <- aggregate(divi_EM$faelle_covid_aktuell_invasiv_beatmet, divi_EM[1], sum)
divi_EM$invasiv_anteil <- divi_muenchen$x / divi_EM$x


# Fälle EM München
EM_muenchen_plot <- ggplot(data = data_EM_muenchen, aes(x = Meldedatum, y = x / 14.72)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die EM in München") +
  geom_vline(xintercept = as.Date(c("2021-06-15", "2021-06-19", "2021-06-23", "2021-07-02")), color = "red", size = 2) +
  annotate("rect", fill = "skyblue1", alpha = 0.4, 
           xmin = as.Date(c("2021-06-22")) -0.5, xmax = as.Date(c("2021-07-16")) +0.5,
           ymin = 0, ymax = Inf)  +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )

# Fälle EM Bayern ohne München 
EM_bayern_ohne_muenchen_plot <- ggplot(data = data_EM_bayern_ohne_muenchen, aes(x = Meldedatum, y = x / 130.8)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die EM in Rest-Bayern") +
  geom_vline(xintercept = as.Date(c("2021-06-15", "2021-06-19", "2021-06-23", "2021-07-02")), color = "red", size = 2) +
  annotate("rect", fill = "skyblue1", alpha = 0.4, 
           xmin = as.Date(c("2021-06-22")) -0.5, xmax = as.Date(c("2021-07-16")) +0.5,
           ymin = 0, ymax = Inf)  +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )

#Zusammenhängende Grafik
ggarrange(EM_muenchen_plot, EM_bayern_ohne_muenchen_plot, nrow = 2)

# Fälle EM Anteil München an Bayern
ggplot(data = data_EM_muenchen, aes(y = anteil, x = Meldedatum)) + 
  geom_line(size = 2, color = "black") +
  labs(x = "Datum", y = "Anteil der Covid-Infektionen", title = "Anteil der münchner Covid-Infektionen an Bayern um die EM") +
  geom_vline(xintercept = as.Date(c("2021-06-15", "2021-06-19", "2021-06-23", "2021-07-02")), color = "red", size = 2) +
  annotate("rect", fill = "skyblue1", alpha = 0.4, 
           xmin = as.Date(c("2021-06-22")) -0.5, xmax = as.Date(c("2021-07-16")) +0.5,
           ymin = 0, ymax = Inf)  +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)


# belegte Intensivbetten Prozent
ggplot(data = divi_EM, aes(x = date, y = invasiv_anteil)) +
  geom_line(stat="identity", size = 2) + 
  labs(x = "Datum", y = "Anteil der invasiv-beatmeten\nCovid-Patienten", title = "Anteil der invasiv-beatmeten Covid-Patienten Münchens an Bayern um die EM") +
  geom_vline(xintercept = as.Date(c("2021-06-15", "2021-06-19", "2021-06-23", "2021-07-02")), color = "red", size = 2) +
  annotate("rect", fill = "plum4", alpha = 0.4, 
           xmin = as.Date(c("2021-06-29")) - 0.5, xmax = as.Date(c("2021-07-23")) + 0.5,
           ymin = 0, ymax = Inf)  +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") +  
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)

# datensätze vernichten
divi_EM <- NULL
divi_muenchen <- NULL
data_EM_germany <- NULL
data_EM_bayern <- NULL
data_EM_muenchen <- NULL
data_EM_bayern_ohne_muenchen <- NULL

### Event4
# Subdatensätze erstellen
data_IAA_germany <- subset(RKI_COVID19, Meldedatum >= "2021-08-07" & Meldedatum <= "2021-11-12" )
data_IAA_bayern <- subset(data_IAA_germany, IdBundesland == "9" )
data_IAA_muenchen <- subset(data_IAA_bayern, Landkreis == "SK MÃ¼nchen")
data_IAA_bayern_ohne_muenchen  <- subset(data_IAA_bayern, Landkreis != "SK MÃ¼nchen")
data_IAA_germany <- aggregate(data_IAA_germany$AnzahlFall, data_IAA_germany[9], sum)
data_IAA_bayern <- aggregate(data_IAA_bayern$AnzahlFall, data_IAA_bayern[9], sum)
data_IAA_muenchen <- aggregate(data_IAA_muenchen$AnzahlFall, data_IAA_muenchen[9], sum)
data_IAA_bayern_ohne_muenchen <- aggregate(data_IAA_bayern_ohne_muenchen$AnzahlFall, data_IAA_bayern_ohne_muenchen[9], sum)
data_IAA_muenchen$anteil <- data_IAA_muenchen$x / data_IAA_bayern$x

divi_IAA <- subset(divi_17_11,date >= "2021-08-07" & date <= "2021-11-12" & bundesland == "09" )
divi_muenchen <- subset(divi_IAA, gemeindeschluessel == "09162")
divi_muenchen <- aggregate(divi_muenchen$faelle_covid_aktuell_invasiv_beatmet, divi_muenchen[1], sum)
divi_IAA <- aggregate(divi_IAA$faelle_covid_aktuell_invasiv_beatmet, divi_IAA[1], sum)
divi_IAA$invasiv_anteil <- divi_muenchen$x / divi_IAA$x



# Covid-Infektionen pro 100.000 Einwohner München während IAA
IAA_muenchen_plot <- ggplot(data = data_IAA_muenchen, aes(x = Meldedatum, y = x / 14.72)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die IAA in München") +
  annotate("rect", fill = "pink", alpha = 0.4, 
           xmin = as.Date(c("2021-09-07")) - 0.5 , xmax = as.Date(c("2021-09-12")) + 0.5,
           ymin = 0, ymax = Inf)  +
  annotate("rect", fill = "skyblue1", alpha = 0.4, 
           xmin = as.Date(c("2021-09-14")) -0.5, xmax = as.Date(c("2021-09-26")) +0.5,
           ymin = 0, ymax = Inf)  +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )


# Covid-Infektionen pro 100.000 Einwohner Bayern ohne München während IAA 
IAA_bayern_ohne_muenchen_plot <- ggplot(data = data_IAA_bayern_ohne_muenchen, aes(x = Meldedatum, y = x / 130.8)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die IAA in Rest-Bayern") +
  annotate("rect", fill = "pink", alpha = 0.4, 
           xmin = as.Date(c("2021-09-07")) - 0.5 , xmax = as.Date(c("2021-09-12")) + 0.5,
           ymin = 0, ymax = Inf)  +
  annotate("rect", fill = "skyblue1", alpha = 0.4, 
           xmin = as.Date(c("2021-09-14")) -0.5, xmax = as.Date(c("2021-09-26")) +0.5,
           ymin = 0, ymax = Inf)  +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )

#Zusammenhängende Grafik
ggarrange(IAA_muenchen_plot, IAA_bayern_ohne_muenchen_plot, nrow = 2)

# Fälle Anteil München an Bayern während IAA
ggplot(data = data_IAA_muenchen, aes(y = anteil, x = Meldedatum)) + 
  geom_line(size = 2, color = "black") +
  labs(x = "Datum", y = "Anteil der Covid-Infektionen", title = "Anteil der münchner Covid-Infektionen an Bayern um die IAA") +
  annotate("rect", fill = "pink", alpha = 0.4, 
           xmin = as.Date(c("2021-09-07")) - 0.5 , xmax = as.Date(c("2021-09-12")) + 0.5,
           ymin = 0, ymax = Inf)  +
  annotate("rect", fill = "skyblue1", alpha = 0.4, 
           xmin = as.Date(c("2021-09-14")) -0.5, xmax = as.Date(c("2021-09-26")) +0.5,
           ymin = 0, ymax = Inf)  +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)

# Belegte Intensivbetten Prozent
ggplot(data = divi_IAA, aes(x = date, y = invasiv_anteil)) +
  geom_line(stat="identity", size = 2) + 
  labs(x = "Datum", y = "Anteil der invasiv-beatmeten\nCovid-Patienten", title = "Anteil der invasiv-beatmeten Covid-Patienten Münchens an Bayern um die IAA") +
  annotate("rect", fill = "pink", alpha = 0.4, 
           xmin = as.Date(c("2021-09-07")) - 0.5, xmax = as.Date(c("2021-09-12")) + 0.5,
           ymin = 0, ymax = Inf)  +
  annotate("rect", fill = "plum4", alpha = 0.4, 
           xmin = as.Date(c("2021-09-21")) - 0.5, xmax = as.Date(c("2021-10-03")) + 0.5,
           ymin = 0, ymax = Inf)  +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") +  
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)

# Datensätze vernichten
divi_IAA <- NULL
divi_muenchen <- NULL
data_IAA_germany <- NULL
data_IAA_bayern <- NULL
data_IAA_muenchen <- NULL
data_IAA_bayern_ohne_muenchen <- NULL

### Ende
