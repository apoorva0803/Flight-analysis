library(plyr)
library(tidyr)
library(ggplot2)


paths=dir("dataset",pattern="_2015.csv",full.names=TRUE)
names(paths)=basename(paths)
df=ldply(paths,read.csv)
df$X=df$.id=NULL
names(df)=c("month","day_of_month","day_of_week","carrier",
            "origin","dest","dept_time","dep_delay",
            "arr_time","arr_delay")
save(df,file="FirstQuarter2015.Rdata")
load(file="FirstQuarter2015.Rdata")

str(df)
df = df %>%
  mutate(
    dept_time =round(dept_time/100,0),
    preDep=dep_delay,
    onTime=dep_delay,
    dep_delay=ifelse(dep_delay<0,0,dep_delay),
    arr_delay=ifelse(arr_delay<0,0,arr_delay),
    preDep=ifelse(preDep<0,preDep,0),
    onTime=ifelse(onTime!=0,NA,0))

###Departure delays as fun of departure time
plot_data = df %>%
gather(delay_type,newdelay,dep_delay,preDep,onTime) %>%
#mutate(delay_type = ifelse(delay_type=="dep_delay","Departure Delay","Pre Departure"))%>%
group_by(dept_time,delay_type) %>%
dplyr::summarise(mu=mean(newdelay,na.rm=TRUE),
                 se=sqrt(var(newdelay,na.rm=TRUE)/length(na.omit(newdelay))),
                 obs=length(na.omit(newdelay)))

#Plot
p=ggplot(plot_data,aes(x=dept_time,y=mu,min=mu-se,max=mu+se,group=delay_type,color=delay_type)) +
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = seq(0,24)) + 
  scale_y_continuous(breaks = seq.int(-6,30,2))+
  labs(x="Hour of Day",y="Mean Delay (Minutes)",title="Delays in Departure Time") +
  theme(legend.position="right") +
  scale_color_discrete(name="Schedule")
p
#ggsave(plot=p,file="Flight_Delays_By_Hour_DelayType.png",width=6,height=4)

plot_data_popular = df %>%
  filter(origin %in% c(
    "ATL",
    "LAX",
    "ORD",
    "DFW",
    "JFK",
    "DEN",
    "SFO",
    "CLT",
    "LAS",
    "PHX"
  ))  %>%
  group_by(dept_time,origin) %>%
  dplyr::summarise(mu=mean(dep_delay,na.rm=TRUE),
                   se=sqrt(var(dep_delay,na.rm=TRUE)/length(na.omit(dep_delay))),
                   obs=length(na.omit(dep_delay))) %>%
  mutate(mu=ifelse((mu-0<.001),NA,mu),
         origin=factor(origin,levels=c(
           "ATL",
           "LAX",
           "ORD",
           "DFW",
           "JFK",
           "DEN",
           "SFO",
           "CLT",
           "LAS",
           "PHX")))

ggplot(subset(plot_data_popular,as.numeric(origin) <=5), aes(x=dept_time, y=mu, min=mu-se,max=mu+se,fill=origin,shape=origin))+ 
   geom_bar(stat="identity", position=position_dodge())+
   labs(x="Hour of Day",y="Mean Departure Delay (Minutes)",title="Delay trend of five Most Popular Airports")
#ggsave(plot=p,file="Delay trend of five Most Popular Airports.png",width=6,height=4)
