install.packages("ggplot2")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("DT")
installed.packages("scales")
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
colors=c("red","blue","green","yellow","pink","black","brown")
apr_data<-read.csv("uber-raw-data-apr14.csv")
may_data<-read.csv("uber-raw-data-may14.csv")
jun_data<-read.csv("uber-raw-data-jun14.csv")
jul_data<-read.csv("uber-raw-data-jul14.csv")
aug_data<-read.csv("uber-raw-data-aug14.csv")
sep_data<-read.csv("uber-raw-data-sep14.csv")
data_2014<-rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)
data_2014
data_2014$Date.Time<-as.POSIXct(data_2014$Date.Time,format="%m/%d/%Y %H:%M:%S")
data_2014$Time<-format(as.POSIXct(data_2014$Date.Time,format="%m/ %d/ %Y %H:%M:%S"),format="%H:%M:%S")
data_2014$Date.Time<-ymd_hms(data_2014$Date.Time)
data_2014$day<-factor(day(data_2014$Date.Time))
data_2014$month<-factor(month(data_2014$Date.Time,label=TRUE))
data_2014$year<-factor(year(data_2014$Date.Time))
data_2014$dayofweek<-factor(wday(data_2014$Date.Time,label=TRUE))
data_2014$hour<-factor(hour(hms(data_2014$Time)))
data_2014$minute<-factor(minute(hms(data_2014$Time)))
data_2014$second<-factor(second(hms(data_2014$Time)))
hour_data<-data_2014 %>%
  group_by(hour) %>%
  dplyr::summarise(Total=n())
datatable(hour_data)
ggplot(hour_data,aes(hour,Total))+
  geom_bar(stat="identity",fill="steelblue",color="red")+
  ggtitle("Trips Every Hour")+
  theme(legend.position="none")+
  scale_y_continuous(labels=comma)
month_hour<-data_2014 %>%
  group_by(month,hour) %>%
  dplyr::summarise(Total=n())
ggplot(month_hour,aes(hour,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by Hour and Month")+
  scale_y_continuous(labels=comma)
day_group<-data_2014 %>%
  group_by(day) %>%
  dplyr::summarise(Total=n())
datatable(day_group)
ggplot(day_group,aes(day,Total)) +
  geom_bar(stat="identity",fill="steelblue")+
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels=comma)
day_month_group<-data_2014 %>%
  group_by(month,day) %>%
  dplyr::summarise(Total=n())
ggplot(day_month_group,aes(day,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by Day and Month")+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=colors)
month_group<-data_2014 %>%
  group_by(month) %>%
  dplyr::summarise(Total=n())
datatable(month_group)
ggplot(month_group,aes(month,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by Month")+
  theme(legend.position = "none")+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=colors)
month_weekday<-data_2014 %>%
  group_by(month,dayofweek) %>%
  dplyr::summarise(Total=n())
ggplot(month_weekday,aes(month,Total,fill=dayofweek))+
  geom_bar(stat="identity")+
  ggtitle("Trips by Day and Month")+
  theme(legend.position="none")+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=colors)
ggplot(data_2014,aes(Base))+
  geom_bar(fill="darkblue")+
  scale_y_continuous(labels=comma)+
  ggtitle("Trips by Bases")
ggplot(data_2014,aes(Base,fill=month))+
  geom_bar(position="dodge")+
  ggtitle("Trips by Bases and Month")+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=colors)
ggplot(data_2014,aes(Base,fill=dayofweek))+
  geom_bar(position="dodge")+
  ggtitle("Trips by Bases and Day of Week")+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=colors)
day_and_hour<-data_2014 %>%
  group_by(day,hour) %>%
  dplyr::summarise(Total=n())
datatable(day_and_hour)
ggplot(day_and_hour,aes(day,hour))+
  geom_tile(aes(fill=Total),color="white",na.rm=TRUE)+
  scale_fill_gradient(low="lightblue",high = "darkblue")+
  guides(fill=guide_legend(title="Heat Map by Hour and Day"))+
  theme_bw()+theme_minimal()+
  labs(title="Heat Map by Hour and Day",x="day",y="hour")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
ggplot(day_month_group,aes(day,month))+
  geom_tile(aes(fill=Total),color="white",na.rm=TRUE)+
  scale_fill_gradient(low="blue",high="navyblue")+
  guides(fill=guide_legend(title="Total"))+
  theme_bw()+theme_minimal()+
  labs(title="Heat Map by Month and Day",x="day",y="month")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
ggplot(month_weekday,aes(dayofweek,month))+
  geom_tile(aes(fill=Total),color="white",na.rm=TRUE)+
  scale_fill_gradient(low="blue",high="navyblue")+
  guides(fill=guide_legend(title="Total"))+
  theme_bw()+theme_minimal()+
  labs(title="Heat Map by Month and Day of Week",x="dayfweek",y="month")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
month_base<-data_2014 %>%
  group_by(Base,month) %>%
  dplyr::summarise(Total=n())
dayofweek_bases<-data_2014 %>%
  group_by(Base,dayofweek) %>%
  dplyr::summarise(Total=n())
ggplot(month_base,aes(Base,month))+
  geom_tile(aes(fill=Total),color="white",na.rm=TRUE)+
  scale_fill_gradient(low="blue",high="navyblue")+
  guides(fill=guide_legend(title="Total"))+
  theme_bw()+theme_minimal()+
  labs(title="Heat Map by Month and Bases",x="Base",y="month")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
ggplot(dayofweek_bases,aes(Base,dayofweek))+
  geom_tile(aes(fill=Total),color="white",na.rm=TRUE)+
  scale_fill_gradient(low="blue",high="navyblue")+
  guides(fill=guide_legend(title="Total"))+
  theme_bw()+theme_minimal()+
  labs(title="Heat Map by Bases and Day of Week",x="Base",y="dayofweek")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
min_lat<-40.5774
max_lat<-40.9176
min_long<--74.15
max_long<--73.7004
ggplot(data_2014,aes(x=Lon,y=Lat))+
  geom_point(size=1,color="blue")+
  scale_x_continuous(limits=c(min_long,max_long))+
  scale_y_continuous(limits=c(min_lat,max_lat))+
  theme_map()+
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014(APR-SEP)")
ggplot(data_2014,aes(x=Lon,y=Lat,color=Base))+
  geom_point(size=1)+
  scale_x_continuous(limits=c(min_long,max_long))+
  scale_y_continuous(limits=c(min_lat,max_lat))+
  theme_map()+
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014(APR-SEP) BY BASE")
  