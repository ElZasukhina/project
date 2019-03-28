#----------- ETL and data exloring --------------------------------------------------------------
#Wine Enthusiast dataset with wine reviews, obtained from Kaggle
#data uploading 
#it is stored in 2 parts, so I'll merge it then

getwd()

library(data.table)
winedata1 <- fread('data/winemag-data_first150k.csv') 
head(winedata1) 
str(winedata1)
summary(winedata1)
summary(winedata1$X) 

table(winedata1$country)  

winedata2 <- fread('data/winemag-data-130k-v2.csv', sep='auto')
str(winedata2)
summary(winedata2$V1) #V1 is ID - the same as X in winedata1
colnames(winedata2)[1] <- "ID"
colnames(winedata1)[1] <- "ID"

#in the second dataset there are 3 more columns: taster_name, taster_twitter_handle, 
#title - we dont really need them, so drop
winedata2 <- winedata2[, -c(10:12)]

#merge 1 and 2 datasets
winedata <- rbind(winedata1, winedata2) #seems everything is good
head(winedata)
rm(winedata1, winedata2)

#data looks tidy - every row contains an observations, every column is a variable 

###IDEA: I have a description column with sommeliers' reviews. Would be great to see which wine 
#characteristics are used more frequently
#in most expensive wine varieties - words like "aromatic, dense, toasty...". 
#So do sentiment analysis for words or sequence of words frequency 

#then would be great to merge soil or weather data to it - ONCE I WILL FIND IT
#At least I know the best soils in the world for growing wine grapes - can create dummy for those regions
#Burgundy, France
#Mendoza, Argentina
#Sicily, Italy
#Tuscany, Italy
## according to Erik Neilso
#Marlborough, New Zeland
#Central Valley, USA
#Loire Valley, France
#Mosel, Germany 
#Bordeaux, France - the gravel in the clay soil increases drainage - check drainage in the description (or tannins)
#Napa Valley, USA
#Alsace, France
#Priorat, Spain
#Coonawarra, Australia
#according to Wine Folly

length(which(winedata$province =='Burgundy'))
sum(winedata$province == 'Loire Valley')

summary(winedata$price) #- lots of NA, it is a problem
price_na <- sum(is.na(winedata$price))
(price_na/length(winedata$ID))*100 #8,8% NAs out of all observations, since they are in my target variable - drop them
winedata <- winedata[!is.na(winedata$price),]

library(dplyr)
library(magrittr)
library(data.table)

print(mean(winedata$price[winedata$province =='Burgundy']))
#70.82212, when median is 25$
###IDEA - TO DO CLUSTERING BY PRICE AND CHECk HOW IT DEPENDS ON PROVINCE

library(ggplot2)
ggplot(aes(points, price), data=winedata) +
  geom_jitter()
#that looks strange, and there're outliers

print(winedata[winedata$price > 3000])
#Château les Ormes Sorbet from Bordeaux (sure) costs like very good laptop or shitty old skoda
winedata <- winedata[winedata$price < 1500]
summary(winedata$price)  #better, but still max is far away from the3rd quartile  
plot(density(winedata$price))

ggplot(winedata, aes(x = cluster, y = points)) +
  geom_boxplot()

#creating dummy for the best soil (by province)
winedata$bestsoil <- 0

bestprovince <- c('Burgundy','Mendoza', 'Sicily', 'Tuscany', 'Marlborough', 'Loire Valley', 
                  'Mosel', 'Bordeaux', 'Napa Valley', 'Alsace', 'Priorat', 'Coonawarra')
winedata$bestsoil <- ifelse(winedata$province %in% bestprovince,1,0)

summary(winedata$bestsoil)
plot(density(winedata$bestsoil))
#looks okay

ggplot(aes(points, price, colour=factor(bestsoil)), data=winedata) +
  geom_jitter()
#moslty represented in the second half (90+ points) - so, it's reasonable

# Distribution in bestsoil
ggplot(winedata, aes(
  x=bestsoil, y=points, fill = as.factor(bestsoil), group = as.factor(bestsoil))
) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

ggplot(winedata, aes(
  x=bestsoil, y=price, fill = as.factor(bestsoil), group = as.factor(bestsoil))
) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) #lots of outliers still


#mb also try to merge with weather in the harvest period (sep, oct) - but my data is not panel, 
#it's invariate in time, mb weather will not help

#------------ Clustering  -------------------------------------------------------------------

#winedata$points <- as.numeric(winedata$points)
#head(scaled)

winedata$price_log <- log(winedata$price)
winedata$points_log <- log(winedata$points)

scaled <- scale(winedata[,c('price_log', 'points_log')])
colnames(scaled) <- c('price_scaled', 'points_scaled')

scols <- c('country','province', 'region_1', 'region_2', 'variety')
winedata <- winedata[ ,(scols) := lapply(.SD, as.factor), .SDcols = scols]

winedata <- cbind(winedata, scaled)

#-------------- Hierarchical clustering on France -----------------------------------------------------
str(winedata)
sum(winedata$country == "France")
winedata$country <- as.character(winedata$country)
france <- winedata[winedata$country == "France",]
france <- france[,c('price', 'points', 'price_log', 'points_log', 'variety', 'region_1')]
summary(france)
sum(is.na(france))

france_hier <- france[, c('price_log', 'points_log')]

#price_points_dist <- dist(france_hier, method='euclidean')
#save(price_points_dist, file='points_variety_dist.Rdata')

#hier_clust <- hclust(price_points_dist, method='complete') #or single or average
#save(hier_clust, file='hier_clust.Rdata')
load('hier_clust.Rdata')
rm(price_points_dist)

#install.packages('dendextend')
library(dendextend)
dist_dend <- as.dendrogram(hier_clust)
plot(dist_dend) #4 clusters
dend_4 <- color_branches(dist_dend, k=5)
plot(dend_4)

groups <- cutree(hier_clust, h = 2) #3 groups
#groups[32552:32561] <- sample(seq(from = 1, to = 5), size = 10, replace = TRUE) 
# for some reason the group vector was 10 observations less
france_hier <- mutate(france_hier, cluster=groups) 

france <- cbind(france, france_hier)
write.csv(as.data.frame(france), file='hier_clust_france.csv')

france <- fread('hier_clust_france.csv')

groups %>% table #cluster 5 seems to be the "the best quality wines" cluster

ggplot(france_hier, aes(x = price_log, y = points_log, color = as.factor(groups))) +
  geom_jitter()

library(cluster)
clusplot(france_hier, groups , color = TRUE, 
         shade = T, labels = 1, lines = 0)

#2 word clouds: variety (price = size), variety (points=size)
library(wordcloud)
words_france <- france[,.(freq = .N, points = sum(points)), by = variety]
wordcloud(words_france$variety, freq = words_france$freq, min.freq = 20)

words_france_byprice <- france[,.(freq = .N, points = sum(price)), by = variety]
wordcloud(words_france_byprice$variety, freq = words_france_byprice$freq, min.freq = 20)

#playing with word clouds and clusters
clust5 <- france[france$cluster==5,]
words_france_clust5 <- clust5[,.(freq = .N, points = sum(points)), by = variety]
wordcloud(words_france_clust5$variety, freq = words_france_clust5$freq, min.freq = 5)
#intresting))

clust1 <- france[france$cluster==1]
words_france_clust1 <- clust1[,.(freq = .N, points = sum(points)), by = variety]
wordcloud(words_france_clust1$variety, freq = words_france_clust1$freq, min.freq = 100)
#intresting))

#---------------- K-means by price and points ---------------------------------------------------

###Elbow method to choose optimal # of clusters
#substructed mean, devided by st deviation. [in the same process as stundartised by normal distr]
inter <- c()
for (i in 1:10) {
  inter <- c(inter, kmeans(winedata[,c('price_scaled', 'points_scaled')],i)$tot.withinss)
  
}
plot(1:i, inter, type = "b")
lines(c(2,10), inter[c(2,10)], col = "green")
lines(c(3,10), inter[c(3,10)], col = "red")
lines(c(4,10), inter[c(4,10)], col = "blue")

(inter / inter[1]) %>% diff(1) * 100

#I would go for 4 or 2? (2 is not enough and it explains nothing)
set.seed(1)
kmeans <- kmeans(winedata[,c('price_scaled', 'points_scaled')], 4)
winedata$cluster <- kmeans$cluster 

#saving the data for the topic modelling and further decision tree
winedata_clust1 <- as.data.frame(winedata[winedata$cluster==1,])
winedata_clust4 <- as.data.frame(winedata[winedata$cluster==4,])
#write.csv(winedata_clust1, file='winedata_clust1.csv')
#write.csv(winedata_clust4, file='winedata_clust4.csv')
rm(winedata_clust1,winedata_clust4)

head(winedata$cluster)
plot(density(winedata$price))
hist(winedata$points) #almost normal, mb that's why kMeans does not want to do clustering by points
#or due to data specification? discrete 

centers <- kmeans$centers
summary(centers)
centers
#centers look okay

table(winedata$cluster)  

ggplot(winedata, aes(points_scaled, price_scaled, col=factor(cluster))) +
  geom_jitter() +
  geom_point(data= as.data.frame(kmeans$centers), 
             aes(points_scaled, price_scaled), inherit.aes = F)

#looks good

ggplot(winedata, aes(x = cluster, y = price, group = cluster,
                     fill = cluster)) +
  geom_boxplot()

head(winedata[winedata$cluster == 1])
table(winedata$country[winedata$cluster == 1]) #mostly from France, US, Italy - ok
table(winedata$bestsoil[winedata$cluster == 1]) #1967/1122 :( - no, it's actually good
table(winedata$variety[winedata$cluster == 1]) #TOP: Bordeaux-style Red Blend, Pinot Noir, 
#Cabernet Sauvignon, Chardonnay, Champagne Blend, Riesling,  - ok

library(cluster)
clusplot(winedata[,c('price_scaled', 'points_scaled')], kmeans$cluster, color = TRUE, shade = T, labels = 1)
# Mess? and price + points explain whole data. Then I will try to exlude price or points and add everything else

summary(winedata[winedata$cluster==4]) #median price is 200, median best soil is 0.36
summary(winedata[winedata$cluster==1]) #median price is 53, medial bestsoil is 0.16, 
#but median points do not vary much

#better way:
winedata %>% 
  group_by(cluster) %>% 
  summarise_all(funs(mean(.))) -> clusters_tab

print(clusters_tab[,c("points", "price", "bestsoil")])

# Distribution of price in high-price cluster by country
ggplot(winedata[winedata$cluster==1], aes(
  x=country, y=price, fill = as.factor(country), group = as.factor(country))
) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Distribution of price in high-price cluster by varietry
ggplot(winedata[winedata$cluster==1], aes(
  x=country, y=price, fill = as.factor(variety), group = as.factor(variety))
) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))


#---------------- k-means on full data ------------------------------------------

### Try second clustering on the whole data
#store all objects as a factor, *factors do not work*, than transform to dummy table
#install.packages('cclust')
library(cclust)

### trying to do kmeans with gower distance (for non-numeric variables)
winedata_clustering1 <- winedata[,c('ID', 'points', 'price', 'bestsoil','country','region_2', 'variety')]
kmeans_fulldata1 <- kmeans(winedata_clustering1, 3, method="manhattan")

kmeans_fulldata1 <- cclust(winedata_clustering1, 3, dist="manhattan",method= "kmeans")

### AAAAAAAAAAAA DOES NOT WORK!!!!!!!!!!!!!

#------------- Principal component analysis --------------------------------------
scaled1 <- scale(winedata_clustering1[,c('price', 'points')])
colnames(scaled1) <- c('price_scaled', 'points_scaled')

winedata_pca <- cbind(winedata_clustering1, scaled1)

winedata_pca <- winedata_pca[,c('bestsoil', 'price_scaled', 'points_scaled')]

pca <- prcomp(winedata_pca, scale = FALSE)
plot(pca, type='l')

# only 1 variable explains data varience 
#which one is important?
pca$rotation 
#price and points describe data most

#----------------- k-means on full data with dummy.data.table -----------------------

winedata_clustering <- winedata[,c('ID', 'points', 'price', 'bestsoil')]

winedata_clustering <- na.omit(winedata_clustering)
summary(winedata_clustering)

library(dummies)
winedata_asdummies <- dummy.data.frame(winedata[,c('country','region_2', 'variety')])

winedata_clustering <- cbind(winedata_clustering, winedata_asdummies)
str(winedata_clustering)

inter_full <- c()
for (i in 1:10) {
  inter_full <- c(inter_full, kmeans(winedata_clustering,i)$tot.withinss)
}
plot(1:i, inter_full, type = "b")
#elbow is definantely exists on 2, but 2 clusters explains a little, go for 3

set.seed(1)
kmeans_fulldata <- kmeans(winedata_clustering, 3) 
winedata$cluster_full <- kmeans_fulldata$cluster 
str(winedata)

#save(winedata, file='winedata_afterclust.csv')

winedata %>% 
  group_by(cluster_full) %>% 
  summarise_all(funs(mean(.)))

centers_full <- kmeans_fulldata$centers
centers_full

winedata$price_log <- log(winedata$price)
winedata$points_log <- log(winedata$points)

ggplot(winedata, aes(points_log, price_log, col=factor(cluster_full))) +
  geom_jitter() 

#seems, that's all not great - centers are very close to each other, 
#and clusters do not differ from each other

# ---------------------- Decision Tree ---------------------------------------------------------
#install.packages('rpart') 
library(rpart)

#install.packages("cairoDevice") #to install Rattle
library(cairoDevice)
#install.packages("rattle", dependencies = T)
#library(rattle)
#install.packages('rattle', repos='http://rattle.togaware.com', type='source')
#library(rattle)
#install.packages('randomForest')
library(randomForest)
#install.packages('forestFloor')
library(forestFloor)

# Train and test

index <- sample(nrow(winedata_clustering), size =  0.8 * nrow(winedata_clustering))
train <- winedata_clustering[index,] %>% as.data.frame()
test <- winedata_clustering[-index,] %>% as.data.frame()

# Regression Tree

tree <- rpart(price ~ ., winedata_clustering[,-c(5,6,7,8,9,11,12)]) #exluded region_1, winery - large ones
rpart.plot::rpart.plot(tree) #looks absolutely terrible, need to group varieties - dummies
#first select by points, then by varieties 
tree$cptable
#dont know how to interpret
tree$variable.importance
summary(tree)
tree
#can see 2 groups of 'best wines':
#Argentina,Austria,Chile,England,New Zealand,US 
#Australia,France,Germany,Hungary,Italy,Portugal,South Africa,Spain 
#absolutely makes sence

#accurancy 
pred <- as.data.frame(predict(tree))
ifelse(pred > 100, "Expensive", "Cheap") == train$pred #smth wrong
sum(ifelse(pred > 100, "Expensive", "Cheap") == train$pred) / nrow(train)
#confusion matrix
table(true = test$points, pred = test$pred) 

#clasify with varieties, create dummy for varieties - cannot find how to refer to the brunch as a vector of varieties
tree_withvariety <- rpart(price ~ ., winedata[,c('country', 'points', 'price', 'variety', 
                                                 'bestsoil')])
tree_withvariety
rpart.plot::rpart.plot(tree_withvariety)

#creating dummy for 1 bunch of varieties
tree_withvariety$frame
path.rpart(tree_withvariety, 9) -> a9
a9$`9`[length(a9$`9`)] %>% 
  strsplit("=") %>% 
  unlist() %>% 
  strsplit(",") %>% 
  unlist() -> leaf9

winedata$leaf9 <- ifelse(winedata$variety %in% leaf9[-1], 1,0)
leaf9[1]

#creating dummy for 2 bunch of varieties
path.rpart(tree_withvariety, 10) -> a10
a10$`10`[length(a10$`10`)] %>% 
  strsplit("=") %>% 
  unlist() %>% 
  strsplit(",") %>% 
  unlist() -> leaf10

winedata$leaf10 <- ifelse(winedata$variety %in% leaf10[-1], 1,0)

#creating dummy for 3 bunch of varieties
path.rpart(tree_withvariety, 12) -> a12
a12$`12`[length(a12$`12`)] %>% 
  strsplit("=") %>% 
  unlist() %>% 
  strsplit(",") %>% 
  unlist() -> leaf12

winedata$leaf12 <- ifelse(winedata$variety %in% leaf12[-1], 1,0)

#creating dummy for 4 bunch of varieties
path.rpart(tree_withvariety, 30) -> a30
a30$`30`[length(a30$`30`)] %>% 
  strsplit("=") %>% 
  unlist() %>% 
  strsplit(",") %>% 
  unlist() -> leaf30

winedata$leaf30 <- ifelse(winedata$variety %in% leaf30[-1], 1,0)

# Regression tree on the new data (with grouped varieties)
tree_dummie_var <- rpart(price ~ ., winedata[,c('country', 'points', 'price', 'bestsoil',
                                                'leaf9', 'leaf10', 'leaf12', 'leaf30')])
tree_dummie_var
rpart.plot::rpart.plot(tree_dummie_var)

winedata[country=='US'][,'variety'] -> states
table(states)
#pinot noir is the most popular variety in States 


### withing the clusters ###
# 1 clust
tree_clust1 <- rpart(price ~ ., winedata_clust1[,c('country', 
                                                   "points", "price", "variety", "bestsoil")])
rpart.plot::rpart.plot(tree_clust1)
tree_clust1
pred_clust1 <- as.data.frame(predict(tree_clust1))


# 4 clust
tree_clust4 <- rpart(price ~ ., winedata_clust4[,c('country', 
                                                   "points", "price", "bestsoil")])
rpart.plot::rpart.plot(tree_clust4)
tree_clust4
pred_clust4 <- as.data.frame(predict(tree_clust4))
