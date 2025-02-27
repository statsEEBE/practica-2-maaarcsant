#Codigo para problema 2
mis_dades<-iris
mis_dades
dim(mis_dades)
names(mis_dades)
#media
mean(mis_dades$Petal.Length)
#desviación tipica
sd(mis_dades$Petal.Length)

hist(mis_dades$Petal.Length)

x<-mis_dades$Petal.Length
y<-mis_dades$Sepal.Length
plot(x,y)
#pendiente m
m<- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
m
b<-mean(y)-m*mean(x)
b

#prediccion para el valor 1.5
m*1.5+b

#altgr+4+espacio=~
mod<-lm(y~x)
mod

summary(mod)

#puntos dnd quiero hacer las mediciones
data.frame(x=x)
x
ypredict <- predict(mod,data.frame(x=x))
#recta de regresion
plot(x,y,col='red',pch=16)
lines(x,ypredict,col="black")

#coeficiente de determinacion
Rsq=sum((ypredict-mean(y))^2)/sum((y-mean(y))^2)
Rsq


summary(mod)
fgh
