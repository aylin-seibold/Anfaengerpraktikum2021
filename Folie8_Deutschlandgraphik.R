### Folie 8: Covid-Infektionen pro 100.000 Einwohner

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