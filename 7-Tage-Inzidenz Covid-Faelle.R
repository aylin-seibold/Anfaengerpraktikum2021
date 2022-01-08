source("Librarys und Datensaetze.R")

### Graphiken:
### Nummer 1: 7-Tage-Inzidenz für Covid-Infektionen in Deutschland und Bundesländer (Folie 9)
### Nummer 2: 7-Tage-Inzidenz für Covid-Infektionen in Deutschland und Bundesländer ab Oktober 2021 (Folie 12)


# Vektor für Makierung der Jahresumbrüche
axiscolors <- c("black","black","black","black","black","black","black",
                "black","black","black","black","red","black","black",
                "black", "black","black","black","black","black","black",
                "black","black", "red")


# Nummer 1
color_code1 = c("Bayern" = "blue", "Deutschland" = "black", 
                "Bremen" = "darkgreen", "Sachsen" = "red", "Andere" = "grey80")

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
  scale_colour_manual(values = color_code1, name ="", 
                      breaks = c("Deutschland", "Bayern", "Sachsen", "Bremen", "Andere")) +
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


# Subdatensatz 7-Tage-Inzidenz für Covid-Fälle ab Oktober 2021
sieben_tage_inzidenz_oktober <- subset(sieben_tage_inzidenz, sieben_tage_inzidenz[,1] >= "2021-10-01")


# Nummer 2
color_code2 = c("Bayern (Impfquote: 70,0%)" = "blue", "Deutschland (Impfquote: 72,4%)" = "black", 
                "Bremen (Impfquote: 84,3%)" = "darkgreen", "Sachsen (Impfquote: 61,6%)" = "red", 
                "Andere" = "grey80")

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
  scale_colour_manual(values = color_code2, name ="", 
                      breaks = c("Deutschland (Impfquote: 72,4%)", "Sachsen (Impfquote: 61,6%)", 
                                 "Bayern (Impfquote: 70,0%)","Bremen (Impfquote: 84,3%)", "Andere")) +
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
