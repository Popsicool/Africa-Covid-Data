library(tidyverse)
library(gridExtra)
cases<-read.csv("C:/Users/hp/Downloads/time_series_covid19_confirmed_global.csv")
recovery <-read.csv("C:/Users/hp/Downloads/time_series_covid19_recovered_global.csv")
death <-read.csv("C:/Users/hp/Downloads/time_series_covid19_deaths_global.csv")
Africa<- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso","Burundi","Cameroon",
           "Cabo Verde", "Central African Republic", "Chad",
          "Comoros", "Congo (Brazzaville)", "Congo (Kinshasa)", "Cote d'Ivoire" , "Djibouti", "Egypt",
           "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia",
           "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia",
           "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius",
           "Morocco","Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda",
           "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone",
           "Somalia", "South Africa", "Sudan", "Eswatini", "Tanzania", "Togo",
           "Tunisia", "Uganda", "Zambia","Zimbabwe")
africa_cases<- cases %>% filter(Country.Region %in% Africa) %>%
  select(c(Country.Region,X8.4.21)) %>% rename(Country= Country.Region, Cases= X8.4.21)
africa_recovery <- recovery %>% filter(Country.Region %in% Africa) %>%
  select(c(Country.Region,X8.4.21)) %>% rename(Country= Country.Region, Recovery= X8.4.21)
africa_death<- death %>% filter(Country.Region %in% Africa) %>%
  select(c(Country.Region,X8.4.21)) %>% rename(Country= Country.Region, Death= X8.4.21)
africa_covid<- full_join(africa_cases, africa_recovery)
africa_covid<- full_join(africa_covid, africa_death)
africa_covid<- africa_covid %>% mutate(per_death= round(Death/Cases * 100),
                        per_recovery= round(Recovery/ Cases * 100),
                        per_active= round((Cases-(Death+Recovery))/Cases *100),
                        Active_Cases= round(Cases-(Death+Recovery)))
top20_death<- arrange(africa_covid, desc(Death)) %>% head(20)
top_20_recovery<- arrange(africa_covid, desc(Recovery)) %>% head(20)
top20_cases<- arrange(africa_covid, desc(Cases)) %>% head(20)
top20_per_death<- arrange(africa_covid, desc(per_death)) %>% head(20)
top_20_per_recovery<- arrange(africa_covid, desc(per_recovery)) %>% head(20)
top_20_per_active<- arrange(africa_covid, desc(per_active)) %>% head(20)
top_20_active_cases<- arrange(africa_covid, desc(Active_Cases)) %>% head(20)

plot7<-ggplot(data = top20_per_death)+
  geom_point(mapping = aes(x= per_death, y=reorder(Country, per_death)),color="black")+
  labs(x="Percentage Death", y="", title= "Top 20 Africa countries with \n highest death percentage", size= 0.1)+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color= "red"))+
  scale_x_continuous(breaks =c(2, 3,4,5,6,7),
                     labels = c("2%","3%","4%", "5%","6%", "7%"))

plot5<- ggplot(data = top_20_per_recovery)+
    geom_point(mapping = aes(x= per_recovery, y=reorder(Country, per_recovery)),color="blue")+
  labs(x="Percentage recovery", y="", title= "Top 20 Africa countries with \n high recovery percentage", color= "blue")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color= "red"))+
  scale_x_continuous(breaks =c(94, 95,96,98,99),
                     labels = c("94%","95%","96%", "98%","99%"))

  scale_x_continuous(breaks =c(100000,500000, 750000,2500000),
                     labels = c("100k","500k","750k","2.5m"))
plot3<- ggplot(data= top_20_per_active)+
  geom_point(mapping = aes(x= per_active, y=reorder(Country, per_active)), color="green")+
  labs(x="Percentage of Active Cases", y="", title= "Top 20 Africa countries with \n highest percantage of active cases", color="green")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color= "red"))+
  scale_x_continuous(breaks =c(25, 50,75,90),
                     labels = c("25%","50%","75%", "90%"))
plot1<- ggplot(data =top20_cases)+
  geom_point(mapping = aes(x=Cases, y= reorder(Country,Cases)), color="red")+
  labs(x= "Number of Confirmed Cases", y= "", title= "Top 20 Africa countries with \n highest number of confirmed cases", color= "red")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color= "red"))+
  scale_x_continuous(breaks =c(500000,100000,1000000, 2500000),
                     labels = c("500k","100k","1m", "2.5m"))
plot6<- ggplot(data=top20_death)+
  geom_point(mapping = aes(x= Death, y= reorder(Country, Death)))+
  labs(x="Number of Deaths", y="", title= "Top 20 Africa countries with \n highest number of Deaths")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color= "red"))+
  scale_x_continuous(breaks =c(10000,20000,30000,70000),
                     labels = c("10k", "20k","30k", "70k"))
plot4<- ggplot(data = top_20_recovery)+
  geom_point(mapping = aes(x= Recovery, y= reorder(Country, Recovery)), color= "blue")+
  labs(x="Number of Recoveries", y="", title= "Top 20 Africa countries with \n highest number of recoveries", color="blue")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color= "red"))+
  scale_x_continuous(breaks =c(100000,500000, 750000,2500000),
                     labels = c("100k","500k","750k","2.5m"))
plot2<-ggplot(data = top_20_active_cases)+
  geom_point(mapping = aes(x=Active_Cases, y= reorder(Country, Active_Cases)), color="green")+
  labs(x="Number of Active Cases", y="", title= "Top 20 Africa countries with \n highest active cases", color= "green")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color= "red"))+
  scale_x_continuous(breaks =c(20000, 40000,80000,1000000, 150000),
                     labels = c("20k","40k","80k", "100k", "150k"))
grid.arrange(plot1,plot2, plot3, plot4, plot5, plot6,plot7, top = textGrob("Visualization of Africa Covid data as at  4th of August 2021",
                                                                           gp= gpar(fontsize= 15)),
             bottom= textGrob("Data source: data.humdata.org \n @Samson_Akinola1"))
