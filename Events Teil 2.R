source("Librarys und Datensaetze.R")

### Graphiken:
### Zeitraum um die Fußball-Europameisterschaft
### Nummer 1.1 Tägliche Covid-Infektionen pro 100.000 Einwohner in München und Rest-Bayern
### Nummer 1.2 Anteil der münchner Covid-Infektionen an Bayern
### Nummer 1.3 Anteil invasiv-beatmeten Covid-Patienten Münchens an Bayern
### 
### Zeitraum um die IAA
### Nummer 2.1 Tägliche Covid-Infektionen pro 100.000 Einwohner in München und Rest-Bayern
### Nummer 2.2 Anteil der münchner Covid-Infektionen an Bayern
### Nummer 2.3 Anteil invasiv-beatmeten Covid-Patienten Münchens an Bayern


# Zeitraum um die Fußball-Europameisterschaft

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


# Zu Nummer 1.1
EM_muenchen_plot <- ggplot(data = data_EM_muenchen, aes(x = Meldedatum, y = x / 14.72)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die EM in München") +
  geom_vline(xintercept = as.Date(c("2021-06-15", "2021-06-19", "2021-06-23", "2021-07-02")), color = "red", size = 2) +
  annotate("rect", fill = "skyblue1", alpha = 0.4, 
           xmin = as.Date(c("2021-06-22")) -0.5, xmax = as.Date(c("2021-07-16")) +0.5,
           ymin = 0, ymax = Inf)  +
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
EM_bayern_ohne_muenchen_plot <- ggplot(data = data_EM_bayern_ohne_muenchen, aes(x = Meldedatum, y = x / 130.8)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die EM in Rest-Bayern") +
  geom_vline(xintercept = as.Date(c("2021-06-15", "2021-06-19", "2021-06-23", "2021-07-02")), color = "red", size = 2) +
  annotate("rect", fill = "skyblue1", alpha = 0.4, 
           xmin = as.Date(c("2021-06-22")) -0.5, xmax = as.Date(c("2021-07-16")) +0.5,
           ymin = 0, ymax = Inf)  +
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


# Nummer 1.1
ggarrange(EM_muenchen_plot, EM_bayern_ohne_muenchen_plot, nrow = 2)


# Nummer 1.2
ggplot(data = data_EM_muenchen, aes(y = anteil, x = Meldedatum)) + 
  geom_line(size = 2, color = "black") +
  labs(x = "Datum", y = "Anteil der Covid-Infektionen", 
       title = "Anteil der münchner Covid-Infektionen an Bayern um die EM") +
  geom_vline(xintercept = as.Date(c("2021-06-15", "2021-06-19", "2021-06-23", "2021-07-02")), color = "red", size = 2) +
  annotate("rect", fill = "skyblue1", alpha = 0.4, 
           xmin = as.Date(c("2021-06-22")) -0.5, xmax = as.Date(c("2021-07-16")) +0.5,
           ymin = 0, ymax = Inf)  +
  scale_x_date(date_breaks = "1 week", date_labels = "%d. %b %y") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 18, face = "bold")) +
  theme(text = element_text(size = 27))+
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)


# Nummer 1.3
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




# Zeitraum um die IAA

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



# Zu Nummer 2.1
IAA_muenchen_plot <- ggplot(data = data_IAA_muenchen, aes(x = Meldedatum, y = x / 14.72)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die IAA in München") +
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


# Zu Nummer 2.1 
IAA_bayern_ohne_muenchen_plot <- ggplot(data = data_IAA_bayern_ohne_muenchen, aes(x = Meldedatum, y = x / 130.8)) +
  geom_bar(stat="identity", width = 1) + 
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner um die IAA in Rest-Bayern") +
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
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") )


# Nummer 2.1
ggarrange(IAA_muenchen_plot, IAA_bayern_ohne_muenchen_plot, nrow = 2)


# Nummer 2.2
ggplot(data = data_IAA_muenchen, aes(y = anteil, x = Meldedatum)) + 
  geom_line(size = 2, color = "black") +
  labs(x = "Datum", y = "Anteil der Covid-Infektionen", 
       title = "Anteil der münchner Covid-Infektionen an Bayern um die IAA") +
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
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)


# Nummer 2.3
ggplot(data = divi_IAA, aes(x = date, y = invasiv_anteil)) +
  geom_line(stat="identity", size = 2) + 
  labs(x = "Datum", y = "Anteil der invasiv-beatmeten\nCovid-Patienten", 
       title = "Anteil der invasiv-beatmeten Covid-Patienten Münchens an Bayern um die IAA") +
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
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white") ) +
  ylim(0,1)

