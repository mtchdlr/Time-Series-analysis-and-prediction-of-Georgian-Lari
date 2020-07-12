###########################################
library(readxl)
library(aTSA)
library(dplyr)
library(fpp2)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)
library(zoo)
library(gridExtra)
###########################################
###########################################

data=read_xlsx('book1.xlsx')
data=data[-1,]
data$Date=as.Date(data$Date,"%Y-%m-%d",tz="GMT")

data_ts=ts(data$`P(t)_P(t-1)`,frequency=365.25, start=c(2001,1))

?ts
data_ts_dollar=ts(data$`USD-GEL`,frequency = 365.25,start = c(2001,1))

#da=xts(data, order.by = data$Date)
#class(da)
#class(da)
#
#da_core=coredata(da)
#da_core
#View(da_core)
#class(da_core)
#
#
#da_index=index(da)
#class(da_index)
#
#plot.xts(as.numeric(da$`P(t)_P(t-1)`))
#class(da)
#plot(data$`USD-GEL`,type='h')
#period=c("2009-01/2009-04")

par(mfrow=c(2,1), mex=0.5,cex=0.75)#tesli naxazi
plot(data$Date,data$`USD-GEL`,type='h',col="coral",main="USD-GEL Exchange Rate",xlab = "Date",ylab = "Exchange Rate")
plot(data$Date,data$`P(t)_P(t-1)`, type="l",col="coral",main="Change in Exchange Rate",xlab="Date",ylab = "Difference")

hist(data$`P(t)_P(t-1)`,probability=T,breaks = 10)#maybe

max(data$`P(t)_P(t-1)`)
data[data$`P(t)_P(t-1)`=="0.16",]
mean(data$`P(t)_P(t-1)`)
which(data$`P(t)_P(t-1)`=="0.16", arr.ind=T)
#################################


which(data$`P(t)_P(t-1)`=="0.16", arr.ind=T)


##########################################################################
one=ggplot()+ geom_rect(aes(
  xmin = as.Date('2008-07-15'),
  xmax = as.Date('2009-01-27'),
  ymin = 1.25,
  ymax = 3
),
fill="coral",
color = "coral",
size =0.1,
alpha=0.2,
)+geom_line(aes(x=data$Date, y=data$`USD-GEL`),lwd=1,col="pink3")+
  labs(title='USD-GEL Exchange Rate',subtitle = "Daily data from 2001 to Feb 29, 2020")+
  xlab('Date')+
  ylab('USD-GEL')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),
        plot.title = element_text(color = "grey50",face = "bold"),plot.subtitle = element_text(color = "grey30"),
        axis.text.y = element_text(face="bold", color="coral", size=14))
one
##########################################################################
data$color=ifelse(data$`P(t)_P(t-1)` < 0, "Negative","Positive")
point=data[2871,]

two=ggplot(data,as.numeric = FALSE)+
  geom_point(aes(x=point$Date,y=point$`P(t)_P(t-1)`))+
  geom_line(aes(x=Date,y=`P(t)_P(t-1)`,col=color))+
  annotate(geom = "point", x = point$Date, y = point$`P(t)_P(t-1)`, colour = "orange", size = 4) + 
  annotate(geom = "text", x = point$Date, y = point$`P(t)_P(t-1)`, label = "2008-11-10",hjust=-0.25)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),legend.position = "none",
        plot.title = element_text(color = "grey30",face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        axis.text.y = element_text(face="bold", color="coral", size=14))+
  labs(title = 'Change in Exchange Rate', subtitle = "USD-GEL")+
  xlab("Date")+
  ylab("Difference")

two

# Wertilebiani tu mominda
#ggplot(data,aes(x = Date, y = `P(t)_P(t-1)`)) +
#geom_point(color = "darkorchid4")


##################
fi=gridExtra::grid.arrange(one,two,nrow=2)
fi
ggsave("fi.png",fi, dpi = 300, height = 20, width = 35, units = "cm")

##################
colors()
####################################################
### SEASONAL SHIT  #####
####################################################

with_every_year=ggseasonplot(data_ts, year.labels=F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),legend.position = "none",
        plot.title = element_text(color = "grey30",face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", color="coral", size=14))+
  labs(title='Exchange Rate Change in each Year',subtitle = 'USD-GEL')+
  xlab('')+
  facet_wrap(~year)

with_every_year
ggsave("with_every_year.png",with_every_year, dpi = 300, height = 20, width = 35, units = "cm")


######

with_every_year_monthly=ggseasonplot(data_ts_dollar, year.labels=TRUE, year.labels.left=TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(color = "grey30",face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(face="bold", color="coral", size=12))+
  labs(title='Exchange Rate Moving Pattern',subtitle = 'USD-GEL')+
  xlab('')

with_every_year_monthly
ggsave("with_every_year_monthly.png",with_every_year_monthly, dpi = 300, height = 20, width = 35, units = "cm")


#######################################################
#######################################################
# data omited 2008 disaster

which(data$`P(t)_P(t-1)`=="0.16")#the highest point...i checked and another high value close to this is row 2868
#with value 0.005
data_omited=data[-c(2871,2868),]

data_omited_ts=ts(data_omited$`P(t)_P(t-1)`,frequency=365.25, start=c(2001,1))
##########
omited_with_every_year=ggseasonplot(data_omited_ts, year.labels=F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),legend.position = "none",
        plot.title = element_text(color = "grey30",face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", color="coral", size=12))+
  labs(title='Exchange Rate Change in each Year',subtitle = 'USD-GEL')+
  xlab('')+
  facet_wrap(~year)

omited_with_every_year
ggsave("omited_with_every_year.png",omited_with_every_year, dpi = 300, height = 20, width = 35, units = "cm")



which(data$Date=="2008-01-01")
which(data$Date=="2008-12-31")
data_2008=data[2557:2922,]

which(data_omited$Date=="2008-01-01")
which(data_omited$Date=="2008-12-31")
data_omited_2008=data_2008[-c(312,315),]

one_2008=ggplot(data_2008)+
  geom_line(aes(x=Date, y=`USD-GEL`),lwd=2,col="seagreen2")+
  labs(title='USD-GEL Exchange Rate 2008',subtitle = "Daily data")+
  xlab('Date')+
  ylab('USD-GEL')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),
        plot.title = element_text(color = "grey30",face = "bold"),plot.subtitle = element_text(color = "grey50"),
        axis.text.y = element_text(face="bold", color="coral", size=14))

one_2008
ggsave("one_2008.png",one_2008, dpi = 300, height = 20, width = 35, units = "cm")

########################
which((sd(data_2008$`P(t)_P(t-1)`))<data_2008$`P(t)_P(t-1)`)
########################

before_2008=ggplot(data_2008)+
  geom_line(aes(x=Date, y=`P(t)_P(t-1)`),lwd=1,col="lightcoral")+
  labs(title='Change in Exchange Rate 2008',subtitle = "Daily data")+
  xlab('Date')+
  ylab('Difference')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),
        plot.title = element_text(color = "grey30",face = "bold"),plot.subtitle = element_text(color = "grey50"),
        axis.text.y = element_text(face="bold", color="coral", size=14))

before_2008
ggsave("before_2008.png",before_2008, dpi = 300, height = 20, width = 35, units = "cm")



after_2008=ggplot(data_omited_2008)+
  geom_line(aes(x=Date, y=`P(t)_P(t-1)`),lwd=1,col="palegreen2")+
  labs(title='Omitted Outliers - Change in Exchange Rate 2008',subtitle = "Daily data")+
  xlab('Date')+
  ylab('Difference')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),
        plot.title = element_text(color = "grey30",face = "bold"),plot.subtitle = element_text(color = "grey50"),
        axis.text.y = element_text(face="bold", color="coral", size=14))

after_2008
ggsave("after_2008.png",after_2008, dpi = 300, height = 20, width = 35, units = "cm")


lak=gridExtra::grid.arrange(before_2008,after_2008,nrow=2)
ggsave("before-after-2008.png",lak, dpi = 300, height = 20, width = 35, units = "cm")

#######################################################
#######################################################
####
####     OMITTING OUTLIERS
####
#######################################################
#######################################################

sd(data$`P(t)_P(t-1)`)

wasashlelebi=which(abs(data$`P(t)_P(t-1)`)>(sd(data$`P(t)_P(t-1)`))*2)

data_clean=data[-wasashlelebi,]


data_clean_ts=ts(data_clean$`P(t)_P(t-1)`,frequency=365.25, start=c(2001,1))

############
data_clean$color=ifelse(data_clean$`P(t)_P(t-1)` < 0, "Negative","Positive")

############
clean_with_every_year=ggseasonplot(data_clean_ts, year.labels=F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),legend.position = "none",
        plot.title = element_text(color = "grey30",face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", color="coral", size=12))+
  labs(title='USD-GEL Exchange Rate Change in each Year',subtitle = 'Omitted Outliers')+
  xlab('')+
  facet_wrap(~year)

clean_with_every_year
ggsave("clean_with_every_year.png",clean_with_every_year, dpi = 300, height = 20, width = 35, units = "cm")



two_clean=ggplot(data_clean,as.numeric = FALSE)+
  geom_line(aes(x=Date,y=`P(t)_P(t-1)`,col=color))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),legend.position = "none",
        plot.title = element_text(color = "grey30",face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        axis.text.y = element_text(face="bold", color="coral", size=12))+
  labs(title = 'Omitted Outliers-Change in Exchange Rate', subtitle = "USD-GEL")+
  xlab("Date")+
  ylab("Difference")

two_clean
ggsave("two_clean.png",two_clean, dpi = 300, height = 20, width = 35, units = "cm")



two_one=two=ggplot(data,as.numeric = FALSE)+
  geom_point(aes(x=point$Date,y=point$`P(t)_P(t-1)`))+
  geom_line(aes(x=Date,y=`P(t)_P(t-1)`,col=color))+
  geom_line(aes(x=Date, y=0.0161))+
  geom_line(aes(x=Date, y=-0.0161))+
  annotate(geom = "point", x = point$Date, y = point$`P(t)_P(t-1)`, colour = "orange", size = 4) + 
  annotate(geom = "text", x = point$Date, y = point$`P(t)_P(t-1)`, label = "2008-11-10",hjust=-0.25)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),legend.position = "none",
        plot.title = element_text(color = "grey30",face = "bold"),
        plot.subtitle = element_text(color = "grey50"),
        axis.text.y = element_text(face="bold", color="coral", size=12))+
  labs(title = 'The Original Data - Change in Exchange Rate', subtitle = "USD-GEL")+
  xlab("Date")+
  ylab("Difference")

two_one


two_clean


#before and after change plot
b=grid.arrange(two, two_clean,nrow=2)
ggsave("THE_PLOT.png",b, dpi = 300, height = 20, width = 35, units = "cm")


write.csv(data_clean,"data_clean.csv")

