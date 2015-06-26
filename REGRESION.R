library(readxl)
data<-read_excel("data_rls_uti.xlsx",sheet =1,col_names = TRUE, na="")
View(data)
media_1<-mean (data[,1])
media_2<-mean ( data[,2]
data_2<-data.frame( )
for(i in 1:nrow(data)){
  data_2[i,1]<-data[i,1]-media_1
}
for(j in 1:nrow(data)){
  data_2[j,2]<-data[j,2]-media_2
}
names(data_2)<-names(data)
regresion<-lm(Utilidad ~ Ventas,data_2)
summary(regresion)
anova<-aov(regresion)
summary(anova )
#calcular el fractil de 0.025 con 38 grados de libertad
qt(0.975,df=38 )
#intervalos de confianza
confint(regresion,level=0.95)
a<-mean(data_2[,1])
b<-mean(data_2[,2])
names(regresion)
res<-regresion[["residuals"]]
predicciones<-regresion[["fitted.values"]]
predicciones
data2<-data.frame(data_2,predicciones,res)
View(data2)
mean(res)
data3<-data.frame(res)
View(data3)
windows()
par(mfrow=c(2,2))
hist(res,20)
qqnorm(res)
qqline(res,col="red")
plot(res,predicciones)
plot(data_2[,1],data_2[,2])

