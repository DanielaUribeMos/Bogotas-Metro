library(MASS)
library(rriskDistributions)
library(fitdistrplus)

#Prueba bondad
hist(Datos$Minuto, main="Histograma", las=1, probability = FALSE)
ajuste=fitdist(Datos$Minuto, "exp")
ajuste$estimate
plot(ajuste)
resultados=gofstat(ajuste)
resultados$chisqpvalue
boxplot(Datos$Minuto, notch=TRUE, main="Caracas")


#Matriz estados 1ra Mayo
estados1=c(0:57)
Mat1=matrix(0, nrow=58, ncol=58,dimnames=list(estados1,estados1))
filas=estados1
columnas=estados1


for(i in filas){
  for(j in columnas){
    if( (i<57) & (j==i+1)){
      Mat1[i+1, j+1]=11.9409
    }
    if((i<=180) & (i>0) & (j==0)){
      Mat1[i+1, j+1]=0.25
    }
    if((i>180) & (j==i-180)){
      Mat1[i+1, j+1]=0.25
    }
  }
}

for(i in 1: length(estados1)){
  Mat1[i, i]=-sum(Mat1[i,])
}

rowSums(Mat1)
View(Mat1)

#Matriz estados NQS
estados2=c(0:256)
Mat2=matrix(0, nrow=257, ncol=257)
filas2=estados2
columnas2=estados2

for(i in filas2){
  for(j in columnas2){
    if( (i<256) & (j==i+1)){
      Mat2[i+1, j+1]=8.9613
    }
    if((i<=180) &  (i>0) & (j==0)){
      Mat2[i+1, j+1]=(1/7)
    }
    if((i>180)& (j==i-180)){
      Mat2[i+1, j+1]=(1/7)
    }
  }
}

#0.14

for(i in 1: length(estados2))
{
  Mat2[i, i]=-sum(Mat2[i,])
}

rowSums(Mat2)
View(Mat2)


#Matriz estados Caracas
estados3=c(0:213)
Mat3=matrix(0, nrow=214, ncol=214)
filas3=estados3
columnas3=estados3

for(i in filas3){
  for(j in columnas3){
    if( (i<213) & (j==i+1)){
      Mat3[i+1, j+1]=15.5507
    }
    if((i<=180) &  (i>0) & (j==0)){
      Mat3[i+1, j+1]=(1/25)
    }
    if((i>180)& (j==i-180)){
      Mat3[i+1, j+1]=(1/25)
    }
  }
}

for(i in 1: length(estados3))
{
  Mat3[i, i]=-sum(Mat3[i,])
}

rowSums(Mat3)

library(expm)

minutos=c(0:60)

#Análisis transitorio: valor esperado 1ra Mayo

esperado1= c(0:60)
varianza1= c(0:60)

for(i in 0:60)
{
  alpha1<-c(1, rep(0, 57))
  prob1<- alpha1%*%expm(Mat1*i)
  esperado1[i+1]=rowSums(prob1*estados1)
  varianza1[i+1]=rowSums(prob1*(estados1^2))-(rowSums(prob1*estados1))^2
}


#Análisis transitorio: valor esperado NQS

esperado2= c(0:60)
varianza2= c(0:60)

for(i in 0:60)
{
  alpha2<-c(1, rep(0, 256))
  prob2<- alpha3%*%expm(Mat2*i)
  esperado2[i+1]=rowSums(prob2*estados2)
  varianza2[i+1]=rowSums(prob2*(estados2^2))-(rowSums(prob2*estados2))^2
}

#Análisis transitorio: valor esperado 

esperado3= c(0:60)
varianza3= c(0:60)

for(i in 0:60)
{
  alpha3<-c(1, rep(0, 213))
  prob3<- alpha3%*%expm(Mat3*i)
  esperado3[i+1]=rowSums(prob3*estados3)
  varianza3[i+1]=rowSums(prob3*(estados3^2))-(rowSums(prob3*estados3))^2
}

#Gráficas valor esperado y varianza

plot(minutos, esperado3, main = "Valor esperado", xlab = "Minuto", ylab = "Valor esperado", type = "l", col="red")
lines(minutos, esperado1, main = "Valor esperado", xlab = "Minuto", ylab = "Valor esperado", col="blue")
lines(minutos, esperado2, main = "Valor esperado", xlab = "Minuto", ylab = "Valor esperado", col="green")

plot(minutos, varianza3, main = "Varianaza", xlab = "Minuto", ylab = "Varianaza", type = "l", col="red")
lines(minutos, varianza1, main = "Varianaza", xlab = "Minuto", ylab = "Varianaza", col="blue")
lines(minutos, varianza2, main = "Varianaza", xlab = "Minuto", ylab = "Varianaza", col="green")

#Análisis estado estable: 1ra Mayo
cmtcMayo<-new( Class="ctmc", states= as.character(estados1), byrow=T, generator=Mat1)
print(cmtcMayo)
Re(steadyStates(cmtcMayo))
sum(Re(steadyStates(cmtcMayo)))

#Análisis estado estable: NQS
cmtcNQS<-new( Class="ctmc", states= as.character(estados2), byrow=T, generator=Mat2)
print(cmtcNQS)
Re(steadyStates(cmtcNQS))
sum(Re(steadyStates(cmtcNQS)))

#Análisis estado estable: Caracas
cmtcCaracas<-new( Class="ctmc", states= as.character(estados3), byrow=T, generator=Mat3)
print(cmtcCaracas)
Re(steadyStates(cmtcCaracas))
sum(Re(steadyStates(cmtcCaracas)))


#Line graph probabilidades estado estable
library(ggplot2)
library(dplyr)

plot(estados1, Re(steadyStates(cmtcMayo)), type = "l", col = "blue", lwd = 2, main = "Probabilidades estado estable 1ra mayo", 
  xlab="Estados", ylab="Probabilidad")
plot(estados2, Re(steadyStates(cmtcNQS)), type = "l", col = "green", lwd = 2, main = "Probabilidades estado estable NQS", 
     xlab="Estados", ylab="Probabilidad")
plot(estados3, Re(steadyStates(cmtcCaracas)), type = "l", col = "red", lwd = 2, main = "Probabilidades estado estable Caracas", 
     xlab="Estados", ylab="Probabilidad")

#Costos 1ra mayo
costo_mayo= 1800*rowSums((Re(steadyStates(cmtcMayo)))*estados1)

#Costos NQS
costo_NQS= 1800*rowSums((Re(steadyStates(cmtcNQS)))*estados2)

#Costos Caracas
costo_caracas= 1800*rowSums((Re(steadyStates(cmtcCaracas)))*estados3)

costo_total<-costo_mayo+ costo_NQS + costo_caracas
