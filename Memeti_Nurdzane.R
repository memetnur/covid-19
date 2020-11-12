'''
project D1
memetnur
date:06.11.2020
covid-19 outbreak
'''
#start
install.packages(" ggplot2 ")
install.packages(" mice ")
install.packages(" missForest ")
install.packages ("tidyverse")
install.package("reshape2")
install.package(" ggthemeis ")
install.package(" devtools ")
install.package(" ggExtra ")
install.package(" psych ")

library("tidyverse")
library("mice")
library("ggplot2")
library("reshape2")
library("ggthemes")
library("devtools")
library("ggExtra")
library("psych ")
library("missForest")

set.seed(3221)# this makes the example exactly reproducible

##2. Inspect the Data Set
data = read.delim("Memeti_Nurdzane.csv", sep=";", dec=".",
                header=TRUE, na.strings=c("","NA"))

#check the data for an overview:
str(data)

#T2.1
tib=as_tibble(data)
print(tib)
view(tib)
data2=as.data.frame(tib)
print(data2)

#T2.2 (2p) What are the variables? And T2.3 (2p) What is the type of each variable? 
#select the specific data variables, which I like.
mydata = subset(data, select=c(1:30))

#list the variables in data
names(mydata)
sapply(mydata, class)

#T2.4 (2p) How many observations are in the dataset? 
str(mydata)

#T2.5 Missing Data
a= md.pattern(mydata, plot = TRUE, rotate.names = TRUE)


##4. Clean the Data Set

#4.1data frames of type tibble
tib=as_tibble(mydata)
print(tib)
#view(mydata)
#view(tib)

#4.2 tidy data: rename first column of data, because it was like this "i..Data"
ncol(mydata)
# Check class
class(mydata)
class(mydata[,1])
colnames(mydata)[1]="Date"
#check again the variables
names(mydata)

#4.3. date structure convert class of "character" to "Date" of 1. column
mydata$Date=as.Date(mydata$Date)
#check class of first column
class(mydata$Date)

#4.3 missing data
#change values "NA" to zero
mydata[is.na(mydata)]=0
#check
print(data)
#delete missing data, if existing
#mydata2=na.omit(mydata)
#is.na(mydata2)
#sum(is.na(mydata2))
#md.pattern(mydata2,plot=TRUE)


##5. Apply Descriptive Measures

#T5.1 (2p) Compute the mean, median, variance, minimum, maximum, and quartiles. 
print(mydata$Hospitalized_ZH)
mean(mydata$Hospitalized_ZH)
median((mydata$Hospitalized_ZH))
var(mydata$Hospitalized_ZH)
min(mydata$Hospitalized_ZH)
max(mydata$Hospitalized_ZH)
quantile(mydata$Hospitalized_ZH)

box= ggplot(mydata, aes(x = factor (0),Hospitalized_ZH))
box + geom_boxplot(outlier.size = 1.5) + ylab ("COVID-19 hospitalized cases in ZH") 

##6. Apply Data Visualisation Towards your Goal
#T6.1 (2p) Construct a basic plot, i.e. a plot with one layer only, and without faceting.

mydata2=mydata
basic= ggplot(mydata2, aes(x=mydata2$Date, y=mydata2$Hospitalized_ZH))+geom_line()
basic

#6.2 Extend the basic plot

#show relation between hospitalized in canton zh and in switzerland
g= ggplot(mydata2, aes(x=mydata2$Date))+
  geom_line(aes(y=mydata2$Hospitalized_ZH, color="Hospitalized in ZH"))+
  geom_line(aes(y=mydata2$Hospitalized_CH, color="Hospitalized in CH"))
#6.3 Aestehtics 
#6.4 Titles
#6.5Themes
g2=g+labs(x="Date", y="Hospitalized",title="Hospitalized in Switzerland during Covid-19")+
  theme(panel.background=element_rect(fill="white", colour="grey50"))+
  theme(axis.title.y = element_text(size = rel(0.9), angle = 90))+
  theme(axis.title.x = element_text(size = rel(0.9)))+
  theme(
    legend.position = 'bottom'
  )
g2

b= ggplot(mydata2, aes(x=mydata2$Date))+
  geom_line(aes(y=mydata2$ZH, color="Covid-19 Cases in ZH"))+
  geom_line(aes(y=mydata2$CH, color="Covid-19 Cases in CH"))

b2=b+labs(x="Date", y="Covid-19 Cases",title="Covid-19 Cases in Switzerland")+
  theme(panel.background=element_rect(fill="white", colour="grey50"))+
  theme(axis.title.y = element_text(size = rel(0.9), angle = 90))+
  theme(axis.title.x = element_text(size = rel(0.9)))+
  theme(
    legend.position = 'bottom',
  )
b2

#6.6 Faceting, multiple plots, saving plots

library("gridExtra")
d=grid.arrange(g2,b2,nrow=2)
ggsave("Memeti_Nurdzane_plot.pdf", d)




