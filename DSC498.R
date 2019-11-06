library(ade4)
library(vegan)
library(gclus)
library(cluster)
library(RColorBrewer)
library(labdsv)
library(corrplot)
install.packages("factoextra")
library(factoextra)

data<-read.csv("yeardata 2.csv")

df<-scale(data)
fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

distance <- get_dist(data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

set.seed(123)
km_result <- kmeans(df, 4, nstart = 24)
print(km_result)
dd <- cbind(data, cluster = km_result$cluster)
head(dd)
table(dd$cluster)
fviz_cluster(km_result, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid",
             star.plot = TRUE, 
             repel = TRUE,
             ggtheme = theme_minimal()
)

d<-dist(as.matrix(data))
hc<-hclust(d)
plot(hc,labels=data$City)
plot(data)

cor=cor(data)
corrplot(cor)
?corrp
  
