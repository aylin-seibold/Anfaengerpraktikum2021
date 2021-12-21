### Datensatz lesen
vaccination_timeseries <-read_tsv("germany_vaccinations_timeseries_v2.tsv")
View(vaccination_timeseries)

## Formatierung Date
myformat2<-"%Y-%m-%d"
vaccination_timeseries$date<-as.Date(vaccination_timeseries$date,myformat2)
View(vaccination_timeseries)


##Plot Impfquote
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

