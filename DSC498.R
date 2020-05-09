library(cluster)
library(RColorBrewer)
library(labdsv)
library(corrplot)
library(ggplot2)
library(factoextra)
library(dplyr)
library(FactoMineR)
library(randomForest)
library(mice)
library(pROC)
library(e1071)
library(tidyverse)
library(glmnet)
library(caret)
library(ROCR)
library(MLmetrics)
install.packages("MLmetrics")


data<-read.csv("yeardata.csv", header=TRUE, sep=",")

data2<-read.csv("yearlydata2.csv", header=TRUE, sep=",")
group1<-read.csv("aqigroup1.csv",header=TRUE, sep=",")
group2=read.csv("aqigroup2.csv",header=TRUE, sep=",")
renjun<-read.csv("renjun.csv",header=TRUE, sep=",")
HB<-read.csv("HB.csv", header=TRUE, sep=",")
HB3<-read.csv("aqigroup3.csv", header=TRUE, sep=",")
HB4<-read.csv("HB4.csv", header=TRUE, sep=",")


population<- read.csv("population data.csv",header=TRUE, sep=",")
coal<-read.csv("coal.csv", header=TRUE, sep=",")
coke<-read.csv("coke.csv", header=TRUE, sep=",")
gas<-read.csv("Natural gas.csv", header=TRUE, sep=",")
diesel<-read.csv("diesel.csv", header=TRUE, sep=",")
electricity<-read.csv("electricity.csv", header=TRUE, sep=",")
fueloil<-read.csv("fuel oil.csv", header=TRUE, sep=",")
gasoline<-read.csv("gasoline.csv", header=TRUE, sep=",")
kerosene<-read.csv("kerosene.csv", header=TRUE, sep=",")


population[1,1:17]
hist(population[1,1:17])

# find best number of clusters
row.names(data2) <- data[,1]
head(data2)
df <- scale(data2) 
fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

# distance matrix
distance <- get_dist(data2)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07",show_labels = TRUE))

# K-means for 3 cluster
set.seed(123)
km_result <- kmeans(df, 4,nstart=25)
print(km_result)
print(km_result$cluster)
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
fviz_cluster(km_result,data=df)
aqi<-data %>%
  mutate(Cluster = km_result$cluster)%>%
  group_by(Cluster) %>%
  summarise_all("mean")


coal2<-coal %>%
  mutate(Cluster = km_result$cluster)%>%
  group_by(Cluster) %>%
  summarise_all("mean")


# hierarchical analysis for 3 cluster
result <- dist(df, method = "euclidean")
result_hc <- hclust(d = result, method = "ward.D2")
fviz_dend(result_hc, cex = 0.6)
fviz_dend(result_hc, k = 4, 
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, 
          rect = TRUE          
)

Hebei<-read.csv("Hebei.csv",header=TRUE)
Hebei2<-read.csv("Hebei2.csv",header=TRUE)
library(psych)
normalization<-function(x)
{
  return((x-min(x))/(max(x)-min(x)))}
Hebei<-normalization(Hebei)
Hebei2<-normalization(Hebei2)
pairs(Hebei[c("coal","coke","Netural.gas","diesel","electricity","fuel.oil","gasoline","kerosene","car","population","aqi")])
library(psych)
pairs.panels(Hebei[c("coal","coke","Netural.gas","diesel","electricity","fuel.oil","gasoline","kerosene","car","population","aqi")])


decathlon2.active <- Hebei2
res.pca <- PCA(decathlon2.active, graph = FALSE)

var <- get_pca_var(res.pca)
var
head(var$coord, 4)
fviz_pca_var(res.pca, col.var = "black")
head(var$cos2)
corrplot(var$cos2, is.corr=FALSE)


fviz_cos2(res.pca, choice = "var", axes = 1:2)

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
head(var$contrib, 4)
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
# 各变量对第二主成分的贡献
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
ind <- get_pca_ind(res.pca)
ind
fviz_pca_ind(res.pca)
lm<-lm(Hebei)
summary(lm)

# feature selection
library(Boruta)

str(group2)
names(group2)<- gsub("_", "", names(group2))
group2=na.omit(group2)
set.seed(123)
boruta.train <- Boruta(aqi~., data = group2, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
# random forest
#classify
HB3=na.omit(HB3)
index <- sample(2,nrow(HB3),replace = TRUE,prob=c(0.7,0.3))
traindata <- HB3[index==1,]
testdata <- HB3[index==2,]


traindata <- as.data.frame(traindata)
set.seed(1234)
hb_tree<- randomForest(as.factor(traindata[,"aqil"])~., data=traindata, mtry=3,important=TRUE)
print(hb_tree)
hb_tree$importance

varImpPlot(hb_tree, main = "variable importance")


hb_pred<-predict(hb_tree, newdata=testdata)
pred_out_1<-predict(object=hb_tree,newdata=testdata,type="prob")
table <- table(hb_pred,testdata$aqil)  
sum(diag(table))/sum(table)
plot(margin(hb_pred,testdata$aqil),main=观测值被判断正确的概率图)
table(hb_pred,testdata$aqil)
obs_p_ran = data.frame(preb=hb_pred,obs=testdata$aqil)
ran_roc <- roc(testdata$aqil,as.numeric(hb_pred))
roc<-multiclass.roc (as.ordered(testdata$aqil),as.ordered(hb_pred))
roc #0.966
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='Random forest ROC curve,mtry=3,ntree=500')



hb_tree2<- randomForest(aqi~., data=traindata1, mtry=3,important=TRUE,na.action=na.omit)
hb_tree2
hb_tree2$importance
varImpPlot(hb_tree2, main = "variable importance")

1# SVM
train_sub = sample(nrow(HB3),7/10*nrow(HB3))
train_data = HB3[train_sub,]
test_data = HB3[-train_sub,]

traindata$aqiL = as.factor(traindata$aqil)
testdata$aqiL = as.factor(testdata$aqil)
HB_svm<- svm(aqiL~.,data=traindata,type = 'C',kernel = 'radial')
print(HB_svm)
pre_svm <- predict(HB_svm,newdata = testdata)
obs_p_svm = data.frame(pred=pre_svm,obs=testdata$aqil)

table(testdata$aqil,pre_svm,dnn=c("true value","predict value"))

roc<-multiclass.roc (as.ordered(test_data$aqil),as.ordered(pre_svm))
roc   #0.8333

# regression
lmgroup<-lm(aqi~.,data = group1)
summary(lmgroup)

lmrenjun<-lm(aqi~.-Coke.consuption -Coal.consuption -Crude.oil.consumption -Netural.gas.consumption,data=renjun)
summary(lmrenjun)

group2=na.omit(group2)
lmgroup2<-lm(aqi~.-Coke.consuption -Coal.consuption -Crude.oil.consumption -Netural.gas.consumption,data = group2)
summary(lmgroup2)

index2 <- sample(2,nrow(group2),replace = TRUE,prob=c(0.7,0.3))
traindata1 <- group2[index2==1,]
testdata1 <- group2[index2==2,]

group2_pred<-predict(lmgroup2, newdata=testdata1)
table(group2_pred,testdata1$aqi)
obs_p_reg = data.frame(pred=group2_pred,obs=testdata1$aqi)
mse_reg = (obs_p_reg$obs-obs_p_reg$prob)**2/length(obs_p_reg$prob)

plot(mse_reg,type="o")
p = ggplot() + geom_line(data = obs_p_reg$pred, color="blue") + geom_line(data=obs_p_reg$obs, color="red")
print(p)
#ridge reg
library(MASS)
library(ridge)
#ridgegroup2<-lm.ridge(aqi~.,lambda=seq(0,150,length=151),data=group2,model=TRUE)
ridgegroup2<-linearRidge(aqi~.,data=group2)
summary(ridgegroup2)

ridge_pred<-predict(ridgegroup2, newdata=testdata1)
table(ridge_pred,testdata1$aqi)
obs_p_ridge = data.frame(prob=ridge_pred,obs=testdata1$aqi)
mse_ridge = (obs_p_ridge$obs-obs_p_ridge$prob)**2/length(obs_p_ridge$prob)

plot(mse_ridge,type="o")

#corrplot
rquery.cormat(group2,type="full")











rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL, ...)
{
  library(corrplot)
  # Helper functions
  #+++++++++++++++++
  # Compute the matrix of correlation p-values
  cor.pmat <- function(x, ...) {
    mat <- as.matrix(x)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # Get lower triangle of the matrix
  getLower.tri<-function(mat){
    upper<-mat
    upper[upper.tri(mat)]<-""
    mat<-as.data.frame(upper)
    mat
  }
  # Get upper triangle of the matrix
  getUpper.tri<-function(mat){
    lt<-mat
    lt[lower.tri(mat)]<-""
    mat<-as.data.frame(lt)
    mat
  }
  # Get flatten matrix
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  # Define color
  if (is.null(col)) {
    col <- colorRampPalette(
      c("#67001F", "#B2182B", "#D6604D", "#F4A582",
        "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
        "#4393C3", "#2166AC", "#053061"))(200)
    col<-rev(col)
  }
  
  # Correlation matrix
  cormat<-signif(cor(x, use = "complete.obs", ...),2)
  pmat<-signif(cor.pmat(x, ...),2)
  # Reorder correlation matrix
  ord<-corrMatOrder(cormat, order="hclust")
  cormat<-cormat[ord, ord]
  pmat<-pmat[ord, ord]
  # Replace correlation coeff by symbols
  sym<-symnum(cormat, abbr.colnames=FALSE)
  # Correlogram
  if(graph & graphType[1]=="correlogram"){
    corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
             tl.col="black", tl.srt=45,col=col,...)
  }
  else if(graphType[1]=="heatmap")
    heatmap(cormat, col=col, symm=TRUE)
  # Get lower/upper triangle
  if(type[1]=="lower"){
    cormat<-getLower.tri(cormat)
    pmat<-getLower.tri(pmat)
  }
  else if(type[1]=="upper"){
    cormat<-getUpper.tri(cormat)
    pmat<-getUpper.tri(pmat)
    sym=t(sym)
  }
  else if(type[1]=="flatten"){
    cormat<-flattenCorrMatrix(cormat, pmat)
    pmat=NULL
    sym=NULL
  }
  list(r=cormat, p=pmat, sym=sym)
}

a=sd(0.93)
a
