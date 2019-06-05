setwd("C:\\MyData\\Upgrad&IIITB\\PredictiveAnalytics\\Module4_Clustering\\Practice_TelecomChurn")
library(ggplot2)
churnData=read.csv("churn_data_complete.csv")
nrow(churnData)


##Remove NAs
sum(is.na(churnData))
churnData = na.omit(churnData)

##Keep only reqd variables
colnames(churnData)
reqdCols=c("change_mou","drop_vce_Mean","custcare_Mean","avgmou")
churn=churnData[,which(colnames(churnData) %in% reqdCols)]

##Treat outliers
box <- boxplot.stats(churn$change_mou)
out <- box$out
churn <- churn[ !churn$change_mou %in% out, ]

box <- boxplot.stats(churn$drop_vce_Mean)
out <- box$out
churn <- churn[ !churn$drop_vce_Mean %in% out, ]

box <- boxplot.stats(churn$custcare_Mean)
out <- box$out
churn <- churn[ !churn$custcare_Mean %in% out, ]

box <- boxplot.stats(churn$avgmou)
out <- box$out
churn <- churn[ !churn$avgmou %in% out, ]

churn$change_mou=scale(churn$change_mou)
churn$drop_vce_Mean=scale(churn$drop_vce_Mean)
churn$avgmou=scale(churn$avgmou)

hcluster=hclust(dist(churn),method = "complete")
plot(hcluster)
rect.hclust(hcluster,k=4)
clusterid=cutree(hcluster,k=4)
churn=cbind(churn,clusterid)

ggplot(churn,aes(avgmou,drop_vce_Mean,size=change_mou,color=factor(clusterid)))+geom_point()
