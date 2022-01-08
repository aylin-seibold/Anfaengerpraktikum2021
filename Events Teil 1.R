source("Librarys und Datensaetze.R")

### Graphiken:
### Zeitraum um die Black Lives Matter Demonstration
### Nummer 1.1 Tägliche Covid-Infektionen pro 100.000 Einwohner in München und Rest-Bayern
### Nummer 1.2 Anteil der münchner Covid-Infektionen an Bayern
### Nummer 1.3 Anteil invasiv-beatmeten Covid-Patienten Münchens an Bayern
### 
### Zeitraum um die Corona Demonstration
### Nummer 2.1 Tägliche Covid-Infektionen pro 100.000 Einwohner in München und Rest-Bayern
### Nummer 2.2 Anteil der münchner Covid-Infektionen an Bayern
### Nummer 2.3 Anteil invasiv-beatmeten Covid-Patienten Münchens an Bayern


# Zeitraum um die Black Lives Matter Demonstration

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


# Zu Nummer 1.1 
BLM_muenchen_plot <- ggplot(data = data_BLM_muenchen, aes(x = Meldedatum, y = x / 14.72)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die BLM-Demo in München") +
  geom_vline(xintercept= as.Date(c("2020-06-06")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-06-13")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23))+
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )


# Zu Nummer 1.1
BLM_bayern_ohne_muenchen_plot <- ggplot(data = data_BLM_bayern_ohne_muenchen, 
                                        aes(x = Meldedatum, y = x / 130.8)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die BLM-Demo in Rest-Bayern") +
  geom_vline(xintercept= as.Date(c("2020-06-06")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-06-13")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23)) +
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )


# Nummer 1.1
ggarrange(BLM_muenchen_plot, BLM_bayern_ohne_muenchen_plot, nrow = 2)


# Nummer 1.2
ggplot(data = data_BLM_muenchen, aes(y = anteil, x = Meldedatum)) + 
  geom_line(size = 2, color = "black") +
  labs(x = "Datum", y = "Anteil der Covid-Infektionen", 
       title = "Anteil der münchner Covid-Infektionen an Bayern um die BLM-Demo") +
  geom_vline(xintercept= as.Date(c("2020-06-06")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-06-13")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)


# Nummer 1.3
ggplot(data = divi_BLM, aes(x = date, y = invasiv_anteil)) +
  geom_line(stat="identity", size = 2) + 
  labs(x = "Datum", y = "Anteil der invasiv-beatmeten\nCovid-Patienten", 
       title = "Anteil der invasiv-beatmeten Covid-Patienten Münchens an Bayern um die BLM-Demo") +
  geom_vline(xintercept= as.Date(c("2020-06-06")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-06-20")), color = "violet", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") +  
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)


# Zeitraum um die Corona Demonstration

# Subdatensätze erstellen
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


# Zu Nummer 2.1 
CovidDemo_muenchen_plot <- ggplot(data = data_CovidDemo_muenchen, aes(x = Meldedatum, y = x / 14.72)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die Corona-Demo in München") +
  geom_vline(xintercept= as.Date(c("2020-09-12")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-09-19")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23))+
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )


# Zu Nummer 2.1 
CovidDemo_bayern_ohne_muenchen_plot <- ggplot(data = data_CovidDemo_bayern_ohne_muenchen, aes(x = Meldedatum, y = x / 130.8)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die Corona-Demo in Rest-Bayern") +
  geom_vline(xintercept= as.Date(c("2020-09-12")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-09-19")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23))+
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))


# Nummer 2.1
ggarrange(CovidDemo_muenchen_plot, CovidDemo_bayern_ohne_muenchen_plot, nrow = 2)


# Nummer 2.2
ggplot(data = data_CovidDemo_muenchen, aes(y = anteil, x = Meldedatum)) + 
  geom_line(size = 2, color = "black") +
  labs(x = "Datum", y = "Anteil der Covid-Infektionen", 
       title = "Anteil der münchner Covid-Infektionen an Bayern um die Corona-Demo") +
  geom_vline(xintercept= as.Date(c("2020-09-12")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-09-19")), color = "blue", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)


# Nummer 2.3
ggplot(data = divi_CovidDemo, aes(x = date, y = invasiv_anteil)) +
  geom_line(stat="identity", size = 2) + 
  labs(x = "Datum", y = "Anteil der invasiv-beatmeten\nCovid-Patienten", 
       title = "Anteil der invasiv-beatmeten Covid-Patienten Münchens an Bayern um die Corona-Demo") +
  geom_vline(xintercept= as.Date(c("2020-09-12")), color = "red", size = 2) +
  geom_vline(xintercept= as.Date(c("2020-09-26")), color = "violet", size = 2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") +  
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27)) +
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)
