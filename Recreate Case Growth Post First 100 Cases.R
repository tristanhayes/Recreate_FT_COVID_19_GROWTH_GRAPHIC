library(tidyverse)
library(ggplot2)
library(readr)
library(directlabels)
library(scales)

#Read in Population Data
datapop <- read.csv("~/World_pops_2019_country_region.csv")
datapop<-datapop[,c("Location","PopTotal","PopDensity")]
names(datapop)[names(datapop) == 'Location'] <- 'CountryRegion'

datapop$CountryRegion<-as.character(datapop$CountryRegion)
#Correct Czech Republic, Iran, Korea South Hong Kong US
datapop$CountryRegion[datapop$CountryRegion=="China, Hong Kong SAR"]<-"Hong Kong"
datapop$CountryRegion[datapop$CountryRegion=="Czechia"]<-"Czech Republic"
datapop$CountryRegion[datapop$CountryRegion=="Republic of Korea"]<-"Korea, South"
datapop$CountryRegion[datapop$CountryRegion=="Iran (Islamic Republic of)"]<-"Iran"
datapop$CountryRegion[datapop$CountryRegion=="United States of America"]<-"US"



#Read in case data
jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_covid19_confirmed_global.csv", sep = "")

datacov <- read_csv(jhu_url)

#Transpose wide to long
data_long <- gather(datacov, date, confirmed_cases, `1/22/20`:colnames(datacov)[ncol(datacov)], factor_key=FALSE)

#fixing date
data_long$date<-as.Date(data_long$date,"%m/%d/%y")

#Setting Hong Kong as a separate Country/Region
data_long$`Country/Region`[data_long$`Province/State`=="Hong Kong"]<-"Hong Kong"

#Rename Czech Rpublic
data_long$`Country/Region`[data_long$`Country/Region`=="Czechia"]<-"Czech Republic"

#Get Average Lat Long (Will be used once we need population data)

#Sum by Country
data_collapsed<-setNames(aggregate(data_long$confirmed_cases, by=list(data_long$`Country/Region`,data_long$date), FUN=sum),c("CountryRegion","Date","Confirmed_Cases"))

#Reducing to >= 100 case
data_collapsed<-data_collapsed[which(data_collapsed$Confirmed_Cases>=100),]

#Sorting
data_collapsed <- data_collapsed[order(data_collapsed$CountryRegion,data_collapsed$Date),]

#Counting Days Since First 100 Cases
data_collapsed$days_since_100<- sequence(rle(data_collapsed$CountryRegion)$lengths)

#Resetting the first day to be 0
data_collapsed$days_since_100<- data_collapsed$days_since_100-1

#Limiting to Countries from FT Graphic
FT_Countries<-c("US","China","Italy","Spain","Germany","France","Iran","Korea, South","Japan","Singapore","Switzerland",
                "United Kingdom", "Netherlands", "Austria","Belgium","Norway","Sweden","Portugal","Canada","Australia",
                "Malaysia","Denmark","Brazil","Israel","Czech Republic","Turkey","India","Hong Kong")

FT_Dataset<-subset(data_collapsed, CountryRegion %in% FT_Countries)

#Limiting the data to the data in the graphic and the limiting the maximum number of days shown
FT_Dataset<-FT_Dataset[which(FT_Dataset$Date<=as.Date("2020-03-24") & FT_Dataset$days_since_100<=35),]

#Source for labels at last point on Line
#https://stackoverflow.com/questions/21781596/refer-to-the-last-column-in-r


ggplot(data=FT_Dataset, aes(x=days_since_100, y=Confirmed_Cases, group=CountryRegion)) +
  geom_line(aes(color=CountryRegion))+
  geom_point(aes(color=CountryRegion))+
   theme(legend.position="none")+
  geom_dl(aes(label = CountryRegion), method = list(dl.combine("last.points"), cex = 0.8))+
  scale_y_log10(expand = c(0, 0),breaks=c(100,200,500,1000,2000,5000,10000,20000,50000,100000),limits=c(100,100000),labels = scales::comma)+
  scale_x_continuous(expand = c(0, 0),breaks=c(5,10,15,20,25),limits=c(0,40),name="Number of days since 100th case ->")+
  labs(title="Country by country: how coronavirus case trajectories compare",subtitle="Cumulative number of confirmed cases by number of days since 100th case") +
  theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),plot.title = element_text(hjust = -0.18, vjust=2.12),plot.subtitle = element_text(hjust = -0.18, vjust=2.12))+
  ylab(NULL)


#####Graph by Population

#Merging in Population
FT_Dataset2<-merge(x = FT_Dataset, y = datapop, by = "CountryRegion", all.x = TRUE)

FT_Dataset2$Proportion<-FT_Dataset2$Confirmed_Cases/FT_Dataset2$PopTotal

#Limiting the maximum number of days shown
FT_Dataset2<-FT_Dataset2[which(FT_Dataset$days_since_100<=35),]


ggplot(data=FT_Dataset2, aes(x=days_since_100, y=Proportion, group=CountryRegion)) +
  geom_line(aes(color=CountryRegion))+
  geom_point(aes(color=CountryRegion))+
  theme(legend.position="none")+
  geom_dl(aes(label = CountryRegion), method = list(dl.combine("last.points"), cex = 0.8))+
  scale_y_continuous(expand = c(0, .025))+
  scale_x_continuous(expand = c(0, 0),breaks=c(5,10,15,20,25,30,35),limits=c(0,40),name="Number of days since 100th case ->")+
  labs(title="Country by country: how coronavirus case trajectories compare",subtitle="Cases per 1,000 population by number of days since 100th case") +
  theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),plot.title = element_text(hjust = -0.05, vjust=2.12),plot.subtitle = element_text(hjust = -0.05, vjust=2.12))+
  ylab(NULL)

#Rescaled
ggplot(data=FT_Dataset2, aes(x=days_since_100, y=Proportion, group=CountryRegion)) +
  geom_line(aes(color=CountryRegion))+
  geom_point(aes(color=CountryRegion))+
  theme(legend.position="none")+
  geom_dl(aes(label = CountryRegion), method = list(dl.combine("last.points"), cex = 0.8))+
  scale_y_log10(expand = c(0, .03),label = label_number(scale = 1000))+
  scale_x_continuous(expand = c(0, 0),breaks=c(5,10,15,20,25,30,35),limits=c(0,40),name="Number of days since 100th case ->")+
  labs(title="Country by country: how coronavirus case trajectories compare",subtitle="Cases per 100,000 population by number of days since 100th case") +
  theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),plot.title = element_text(hjust = -0.05, vjust=2.12),plot.subtitle = element_text(hjust = -0.05, vjust=2.12))+
  ylab(NULL)