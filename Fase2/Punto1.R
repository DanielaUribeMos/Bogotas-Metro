
#Definir estados
estados = c()

for(a in 0:4){
  for(b in 0:2){
    for(c in 0:3){
      for(d in 0:4)
      {
        estados = c(estados,paste(a,b,c,d, sep = ","))
      }
    }
  }
}

#Matriz
matrizQ=matrix(0, nrow=length(estados), ncol=length(estados))
dimnames(matrizQ) = list(estados, estados)

#Rellenar matriz

for (fila in estados){
  for (columna in estados){
    
    a = as.numeric(strsplit(fila, ",")[[1]][1])
    b = as.numeric(strsplit(fila, ",")[[1]][2])
    c = as.numeric(strsplit(fila, ",")[[1]][3])  
    d = as.numeric(strsplit(fila, ",")[[1]][4])
    
    p = as.numeric(strsplit(columna, ",")[[1]][1])
    q = as.numeric(strsplit(columna, ",")[[1]][2])
    r = as.numeric(strsplit(columna, ",")[[1]][3])
    s = as.numeric(strsplit(columna, ",")[[1]][4])
    
    if(a<4 & p==a+1 & b==q & c==r & d==s)
    {
      matrizQ[fila,columna]<-60
    }
    if(b<2 & a>=1 & q==b+1 & p==a-1 & c==r & d==s)
    {
      matrizQ[fila,columna]<-55*0.3*min(a, 3)
    }
    if(d<4 & a>=1 & s==d+1 & p==a-1 & b==q & c==r)
    {
      matrizQ[fila,columna]<-55*0.7*min(a, 3)
    }
    if(b<2 & c==r & q==b+1 & p==a & d==s)
    {
      matrizQ[fila,columna]<-24
    }
    if(c<3 & b>=1 & r==c+1 & q==b-1 & p==a & d==s)
    {
      matrizQ[fila,columna]<-20
    }
    if(d<4 & s==d+1 & r==c-1 & p==a & q==b & c>=1)
    {
      matrizQ[fila,columna]<-30*min(c, 2)
    }
    if(s==d-1 & a==p & b==q & c==r & d>=1)
    {
      matrizQ[fila,columna]<-65*min(d, 2)
    }
  }
}

rowSums(matrizQ)

#Rellenar la diagonal
for (fila in estados)
{
  for (columna in estados)
  {
    if(fila==columna)
    {
      matrizQ[fila,columna] <- -rowSums(matrizQ)[fila]
    }
  }
}

rowSums(matrizQ)


#Tiempo primera pasada m(0,0,0,0)->(4,2,3,4)
#Tiempo en estar llena
library(expm)
library(markovchain)
cadenaContinua <- new("ctmc", states = estados,
                      byrow = TRUE, generator = matrizQ)
ExpectedTime(cadenaContinua,1,300)


#Proporción estar llena

L=-1/diag(matrizQ)
alpha=rep(0,300)
names(alpha)=estados
alpha["0,0,0,0"]=1

M=function(Q, n)
{
  M=matrix(0, nrow=nrow(Q), ncol=ncol(Q))
  
  for(i in 0:n)
  {
    M=M + generatorToTransitionMatrix(Q)%^% i
  }
  
  return(M)
}

n=339
M_n=M(matrizQ, n-1)
E_t=alpha%*%M_n%*%L
tiempo_ob=2

M_t=alpha%*%M(matrizQ, n)*L


#Tiempo tickets
#Tiempo que la zona está llena
suma=0
sum_total=0
iterador=1
for (columna in estados)
{
  suma_total<-M_t[1, iterador]+suma_total
    p = as.numeric(strsplit(columna, ",")[[1]][1])
    if(p==4)
    {
      suma=suma + M_t[1, iterador]
    }
    iterador=iterador+1
}
suma
proporcion<-suma/suma_total
proporcion

#Estado estable
#Cantidad personas fila largo plazo

prob_estable<-Re(steadyStates(cadenaContinua))
estados_estable=c(rep(0, 300))
names(estados_estable)=estados
  
iterador=1;

for (columna in estados)
{
      actual=0
      
      p = as.numeric(strsplit(columna, ",")[[1]][1])
      q = as.numeric(strsplit(columna, ",")[[1]][2])
      r = as.numeric(strsplit(columna, ",")[[1]][3])
      s = as.numeric(strsplit(columna, ",")[[1]][4])
      
      print(p)
      print(q)
      print(r)
      print(s)
      if(p==4)
      {
        actual=1
      }
      if(q==2)
      {
        actual=actual+1
      }
      if(r==3)
      {
        actual=actual+1
      }
      if(s==4)
      {
        actual=actual+2
      }
      if(s==3)
      {
        actual=actual+1
      }
      
      estados_estable[iterador]=actual
      iterador=iterador+1
}

Valor_esperado= rowSums(estados_estable*prob_estable)
c(1,8)
