source("Librarys und Datensaetze.R")

### Graphiken:
### Nummer 1: 7-Tage-Inzidenz für Covid-Hospitalisierungen in Deutschkand und Bundesländern
### (Folie 10)
### Nummer 2: 7-Tage-Inzidenz für Covid-Hospitalisierungen in Deutschkand und Bundesländern ab Oktober 2021
### (Folie 13)


# Vektor für Makierung der Jahresumbrüche
axiscolors <- c("black","black","black","black","black","black","black",
                "black","black","black","black","red","black","black",
                "black", "black","black","black","black","black","black",
                "black","black", "red")


# Subdatensätze 7-Tage-Inzidenz für Covid-Hospitalisierungen für jedes Bundesland und Deutschland 
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
hosp_7tage_berlin <- subset(hosp, Bundesland_Id == "11")
hosp_7tage_brandenburg <- subset(hosp, Bundesland_Id == "12")
hosp_7tage_mecklenburg_Vorpommern <- subset(hosp, Bundesland_Id == "13")
hosp_7tage_sachsen <- subset(hosp, Bundesland_Id == "14")
hosp_7tage_sachsen_anhalt <- subset(hosp, Bundesland_Id == "15")
hosp_7tage_thueringen <- subset(hosp, Bundesland_Id == "16")
hosp_7tage_deutschland <- subset(hosp, Bundesland_Id == "0")


# Hospitalisieurngsfaelle Bremen, Sachsen , Bayern und Deustchland
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


# Nummer 1
color_code1 <- c("Deutschland" = "black", "Bayern" = "blue",  
                 "Bremen" = "darkgreen", "Sachsen" = "red", 
                 "Andere" = "grey80")

ggplot()+
  geom_line(size = 1, data = berlin_hospit, mapping = aes(x = Datum, y = ((x/ 3664088) *100000)/2 , col = "Andere")) +
  geom_line(size = 1, data = brandenburg_hospit, mapping = aes(x = Datum, y = ((x/2531071) *100000)/2 , col = "Andere")) +
  geom_line(size = 1, data = hamburg_hospit, mapping = aes(x = Datum, y = ((x/ 1852478) *100000)/2, col = "Andere"))+
  geom_line(size = 1, data = hessen_hospit, mapping = aes(x = Datum, y = ((x/6293154) *100000)/2, col = "Andere" )) +
  geom_line(size = 1, data = mecklenburg_Vorpommern_hospit, mapping = aes(x = Datum, y = ((x/1610774) *100000)/2 , col = "Andere")) +
  geom_line(size = 1, data = niedersachsen_hospit, mapping = aes(x = Datum, y = ((x/ 8003421) *100000)/2, col = "Andere")) +
  geom_line(size = 1, data = nordrhein_westfalen_hospit, mapping = aes(x = Datum, y = ((x/17925570) *100000)/2 , col = "Andere")) +
  geom_line(size = 1, data = rheinland_pfalz_hospit, mapping = aes(x = Datum, y = ((x/4098391) *100000)/2, col = "Andere" )) +
  geom_line(size = 1, data = saarland_hospit, mapping = aes(x = Datum, y = ((x/ 983991) *100000)/2, col = "Andere")) +
  geom_line(size = 1, data = sachsen_anhalt_hospit, mapping = aes(x = Datum, y = ((x/ 2180684) *100000)/2 , col = "Andere")) +
  geom_line(size = 1, data = schleswig_holstein_hospit, mapping = aes(x = Datum, y = ((x/2910875) *100000)/2 , col = "Andere")) +
  geom_line(size = 1, data = thueringen_hospit, mapping = aes(x = Datum, y = ((x/ 2120237) *100000)/2, col = "Andere"))+
  geom_line(size = 1, data = baden_Wuerttemberg_hospit, mapping = aes(x = Datum, y = ((x/11103043) *100000)/2, col = "Andere")) +
  geom_line(size = 2, data = sachsen_hospit, mapping = aes(x = Datum, y = ((x/4056941) *100000)/2 , col = "Sachsen")) +
  geom_line(size = 2, data = bremen_hospit, mapping = aes(x = Datum, y = ((x/ 680130) *100000)/2, col = "Bremen")) +
  geom_line(size = 2, data = bayern_hospit, mapping = aes(x = Datum, y = ((x/13140183) *100000)/2 , col = "Bayern")) +
  geom_line(size = 2, data = deutschland_hospt, mapping = aes(x = Datum, y = ((x/ 83129285) *100000)/2, col = "Deutschland")) +
  labs(x = "Datum", y = "7-Tage-Inzidenz für Covid-Hospitalisierungen", title = "7-Tage-Inzidenz für Covid-Hospitalisierungen in Deutschland und Bundesländern") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d. %b %y") +  
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  scale_color_manual(name = " ", values = color_code1) +
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  theme(axis.text.x = element_text(color=axiscolors)) 


# Subdatensätze 7-Tage-Inzidenz für Covid-Hospitalisierungen ab Oktober 2021
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

# Nummer 2
color_code2 <- c("Deutschland (Impfquote: 72,4%)" = "black", "Sachsen (Impfquote: 61,6%)" = "red", 
                 "Bayern (Impfquote: 70,0%)" = "blue", "Bremen (Impfquote: 84,3%)" = "darkgreen", 
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
  scale_color_manual(name = " ", values = color_code2)+
  theme( panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                         colour = "grey"), 
         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                         colour = "white")) 