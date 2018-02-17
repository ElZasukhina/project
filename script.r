#----------- ETL and data exloring ------------------------------------

#data uploading 
#it is stored in 2 parts, so I'll merge it then

getwd()

winedata1 <- read.csv('data/winemag-data_first150k.csv') #it is quite heavy, I'll try fast read then
head(winedata1) 
str(winedata1)
summary(winedata1)
summary(winedata1$X) 

table(winedata1$country) #countries and codes that assigned to couuntry (maybe) but some of them in thousends - mb not counrty code

library(data.table)
winedata2 <- fread('data/winemag-data-130k-v2.csv', sep='auto')
str(winedata2)
summary(winedata2$V1) #V1 is ID - the same as X in winedata1
colnames(winedata2)[1] <- "ID"
colnames(winedata1)[1] <- "ID"

#in the second dataset there are 3 more columns: taster_name, taster_twitter_handle, title - we dont really need them, so drop
winedata2 <- winedata2[, -c(10:12)]

??wine

#merge 1 and 2 datasets
winedata <- rbind(winedata1, winedata2) #seems everything is good
head(winedata)

#data looks tidy - every row contains observations, every column is variable 

###IDEA: I have a description column with sommeliers' reviews. Would be great to see which wine characteristics are used more frequently
#in most expensive wine varieties - words like "aromatic, dense, toasty..."

#then would be great to merge soil data to it - ONCE I WILL FIND IT
#At least I know the best soils in the world for growing wine grapes - can create dummy for those regions
#Burgundy, France
#Mendoza, Argentina
#Sicily, Italy
#Tuscany, Italy
## according to Erik Neilso
#Marlborough, New Zeland
#Central Valley, USA
## according to me

length(which(winedata$province =='Burgundy'))
#6446

sum(winedata$province == 'Marlborough')
#2380

summary(winedata$price) #- lots of NA, is is a problem
winedata <- winedata[!is.na(winedata$price),]

library(dplyr)
winedata %>% if (province == 'Burgundy') {
 print(mean(price))
}
#error, return NA. after dropping NAs doesnt work too - bad approach

print(mean(winedata$price[winedata$province =='Burgundy']))
#70.82212, when median is 25$
###IDEA - TO DO CLUSTERING BY PRICE AND CHECH HOW IT DEPENDS ON PROVINCE

library(ggplot2)
ggplot(aes(points, price), data=winedata) +
  geom_point()
#that looks strange - and there's outliers

print(winedata[winedata$price > 3000])
winedata <- winedata[winedata$price < 1500]
summary(winedata$price)  #better, but still max is far away from 3rd quartile  

#creating dummy for best soil (by province)
winedata$bestsoil <- 0

#a little bit awkward and takes time (FOREVER), but don't know how to generate it in a nicer way
for (i in winedata$province) {
  if (i == 'Burgundy' | i == 'Mendoza' | i == 'Sicily' | i == 'Tuscany') {
    winedata$bestsoil = 1
  }
  else {
    winedata$bestsoil = 0
  }
}

summary(winedata$bestsoil)

