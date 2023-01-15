
shiny_function_1 = function(estado_tren, x, y )
{
  opt_opt <- 0
  opt_irre <- 0
  opt_reg <- 0
  
  reg_reg <-0
  reg_opt <- 0
  reg_irre <- 0
  
  for (i in 1:(length(estado_tren)-1))
  {
    if ( !(estado_tren[i]=="Fallas Irreparables" & estado_tren[i+1]=="Condiciones Óptimas"))
    {
      if (estado_tren[i] == "Condiciones Óptimas" &
          estado_tren[i+1] == "Condiciones Óptimas")
      {
        opt_opt = opt_opt + 1
      }
      if (estado_tren[i] == "Condiciones Óptimas" &
               estado_tren[i+1] == "Fallas Irreparables")
      {
        opt_irre = opt_irre + 1
      }
      if (estado_tren[i] == "Condiciones Óptimas" &
               estado_tren[i+1] == "Condiciones Regulares")
      {
        opt_reg = opt_reg + 1
      }
      if (estado_tren[i] == "Condiciones Regulares" &
               estado_tren[i+1] == "Condiciones Regulares")
      {
        reg_reg = reg_reg + 1
      }
      if (estado_tren[i] == "Condiciones Regulares" &
               estado_tren[i+1] == "Condiciones Óptimas")
      {
        reg_opt = reg_opt + 1
      }
      if (estado_tren[i] == "Condiciones Regulares" &
               estado_tren[i+1] == "Fallas Irreparables")
      {
        reg_irre = reg_irre + 1
      }
    }
  }
  
  prob_opt_opt = opt_opt / (opt_opt + opt_irre + opt_reg)
  prob_opt_irre = opt_irre / (opt_opt + opt_irre + opt_reg)
  prob_opt_reg = opt_reg / (opt_opt + opt_irre + opt_reg)
  
  prob_reg_reg = reg_reg / (reg_reg + reg_opt + reg_irre)
  prob_reg_opt = reg_opt / (reg_reg + reg_opt + reg_irre)
  prob_reg_irre = reg_irre / (reg_reg + reg_opt + reg_irre)
  
  estados_km <- c(rep(0, 69))
  
  for (i in 0:69)
  {
    estados_km[i + 1] <- i * 10
  }
  
  matriz_km = matrix(0,
                     nrow = length(estados_km),
                     ncol = length(estados_km))
  dimnames(matriz_km) = list(estados_km, estados_km)
  
  for (fila in 0:69)
  {
    for (columna in 0:69)
    {
      a = as.numeric(fila[[1]][1])
      i = as.numeric(columna[[1]][1])
      
      if (i - a >= 0 & i - a <= 3)
      {
        matriz_km[fila + 1, columna + 1] = 1 / 4
      }
      if (a == 67 & i == 69)
      {
        matriz_km[fila + 1, columna + 1] = 1 / 2
      }
      if (a == 68 & i == 69)
      {
        matriz_km[fila + 1, columna + 1] = 3 / 4
      }
      if (a == 69 & i == 69)
      {
        matriz_km[fila + 1, columna + 1] = 1
      }
    }
  }
  
  rowSums(matriz_km)
  
  estados_cond = c("F", "R", "D")
  
  matriz_cond = matrix(0,
                       nrow = length(estados_cond),
                       ncol = length(estados_cond))
  dimnames(matriz_cond) = list(estados_cond, estados_cond)
  
  for (fila in estados_cond)
  {
    for (columna in estados_cond)
    {
      b = fila[[1]][1]
      j = columna[[1]][1]
      
      if (b == "F" & j == "D")
      {
        matriz_cond[fila, columna] = prob_opt_irre
      }
      if (b == "F" & j == "R")
      {
        matriz_cond[fila, columna] = prob_opt_reg
      }
      if (b == "R" & j == "F")
      {
        matriz_cond[fila, columna] = prob_reg_opt
      }
      if (b == "R" & j == "D")
      {
        matriz_cond[fila, columna] = prob_reg_irre
      }
      if (b == "F" & j == "F")
      {
        matriz_cond[fila, columna] = prob_opt_opt
      }
      if (b == "R" & j == "R")
      {
        matriz_cond[fila, columna] = prob_reg_reg
      }
      if (b == "D" & j == "D")
      {
        matriz_cond[fila, columna] = 1
      }
      
    }
  }
  rowSums(matriz_cond)
  
  estados = c()
  condiciones = c("F", "R", "D")
  for (a in 0:69)
  {
    for (b in condiciones)
    {
      estados = c(estados, paste(a * 10, b, sep = ","))
    }
  }
  
  matrizP = matrix(0, nrow = length(estados), ncol = length(estados))
  dimnames(matrizP) = list(estados, estados)
  for (fila in estados)
  {
    for (columna in estados)
    {
      a = strsplit(fila, ",")[[1]][1]
      b = (strsplit(fila, ",")[[1]][2])
      i = strsplit(columna, ",")[[1]][1]
      j = (strsplit(columna, ",")[[1]][2])
      
      matrizP[fila, columna] <- matriz_km[a, i] * matriz_cond[b, j]
    }
  }
  rowSums(matrizP)
  pos = c()
  contador = 0
  for (fila in estados) {
    a = as.numeric(strsplit(fila, ",")[[1]][1])
    b = (strsplit(fila, ",")[[1]][2])
    contador = contador + 1
    if (a == 690 | b == "D") {
      pos = c(pos, contador)
    }
    
  }
  pos
  
  #Matriz Transitorios a absorbentes
  R = matrizP[-pos, pos]
  
  #Matriz Transitorios a transitorio
  Q = matrizP[-pos, -pos]
  
  #Matriz Identidad
  I = matrix(0, nrow = length(Q[, 1]), ncol = length(Q[1, ]))
  for (i in 1:dim(I)[1]) {
    for (j in 1:dim(I)[2]) {
      if (i == j) {
        I[i, j] = 1
      }
    }
  }
  
  #inversa
  #Matriz t de tiempos de absorci?n
  matrizT = (I - Q)
  t = solve(matrizT)
  alfa <- c(1, rep(0, length(t[1, ]) - 1))
  alfa
  tiempos_circulacion <- rowSums(alfa %*% t)
  
  
  #Matriz probabildiades en absorci?n
  vector_prob <- alfa %*% t %*% R
  
  #Gráfica probabilidades
  vector_graf <- c(rep(0, length(vector_prob) - 2))
  for (i in 1:(length(vector_prob) - 2))
  {
    vector_graf[i] <- vector_prob[i]
    
    if (i == (length(vector_prob) - 2))
    {
      vector_graf[i] <- vector_prob[i] + vector_prob[i + 1] + vector_prob[i +2]
    }
  }
  
  estados_graf <- c(rep(0, 69))
  for (i in 0:69)
  {
    estados_graf[i + 1] <- i * 10
  }
  data_frame_prob <- data.frame((vector_graf), estados_graf)
  
  plot_ly(
    data_frame_prob,
    x =  ~ estados_graf,
    y =  ~ vector_graf,
    name = "Probabilidad vida útil",
    type = "scatter",
    mode = "line+markers"
  )
  
  #Matriz probabilidades antes de absorción
  vector_prob2 <- alfa %*% t
  
  costo_acum = 0
  for (i in 1:72)
  {
    if (i >= 1 & i <= 69)
    {
      costo_acum = costo_acum + vector_prob[i] * x
    }
    if (i>69 & i<=71)
    {
      costo_acum = costo_acum + vector_prob[i] * y
    }
    if(i>71)
    {
      costo_acum = costo_acum + vector_prob[i] * ((x+y)/2)
    }
  }
  costo_acum
  return(costo_acum)
}

shiny_function_2 = function(estado_tren, x, y )
{
  opt_opt <- 0
  opt_irre <- 0
  opt_reg <- 0
  
  reg_reg <-0
  reg_opt <- 0
  reg_irre <- 0
  
  for (i in 1:(length(estado_tren)-1))
  {
    if ( !(estado_tren[i]=="Fallas Irreparables" & estado_tren[i+1]=="Condiciones Óptimas"))
    {
      if (estado_tren[i] == "Condiciones Óptimas" &
          estado_tren[i+1] == "Condiciones Óptimas")
      {
        opt_opt = opt_opt + 1
      }
      if (estado_tren[i] == "Condiciones Óptimas" &
          estado_tren[i+1] == "Fallas Irreparables")
      {
        opt_irre = opt_irre + 1
      }
      if (estado_tren[i] == "Condiciones Óptimas" &
          estado_tren[i+1] == "Condiciones Regulares")
      {
        opt_reg = opt_reg + 1
      }
      if (estado_tren[i] == "Condiciones Regulares" &
          estado_tren[i+1] == "Condiciones Regulares")
      {
        reg_reg = reg_reg + 1
      }
      if (estado_tren[i] == "Condiciones Regulares" &
          estado_tren[i+1] == "Condiciones Óptimas")
      {
        reg_opt = reg_opt + 1
      }
      if (estado_tren[i] == "Condiciones Regulares" &
          estado_tren[i+1] == "Fallas Irreparables")
      {
        reg_irre = reg_irre + 1
      }
    }
  }
  
  prob_opt_opt = opt_opt / (opt_opt + opt_irre + opt_reg)
  prob_opt_irre = opt_irre / (opt_opt + opt_irre + opt_reg)
  prob_opt_reg = opt_reg / (opt_opt + opt_irre + opt_reg)
  
  prob_reg_reg = reg_reg / (reg_reg + reg_opt + reg_irre)
  prob_reg_opt = reg_opt / (reg_reg + reg_opt + reg_irre)
  prob_reg_irre = reg_irre / (reg_reg + reg_opt + reg_irre)
  
  estados_km <- c(rep(0, 69))
  
  for (i in 0:69)
  {
    estados_km[i + 1] <- i * 10
  }
  
  matriz_km = matrix(0,
                     nrow = length(estados_km),
                     ncol = length(estados_km))
  
  dimnames(matriz_km) = list(estados_km, estados_km)
  
  for (fila in 0:69)
  {
    for (columna in 0:69)
    {
      a = as.numeric(fila[[1]][1])
      i = as.numeric(columna[[1]][1])
      
      if (i - a >= 0 & i - a <= 3)
      {
        matriz_km[fila + 1, columna + 1] = 1 / 4
      }
      if (a == 67 & i == 69)
      {
        matriz_km[fila + 1, columna + 1] = 1 / 2
      }
      if (a == 68 & i == 69)
      {
        matriz_km[fila + 1, columna + 1] = 3 / 4
      }
      if (a == 69 & i == 69)
      {
        matriz_km[fila + 1, columna + 1] = 1
      }
    }
  }
  
  rowSums(matriz_km)
  
  estados_cond = c("F", "R", "D")
  
  matriz_cond = matrix(0,
                       nrow = length(estados_cond),
                       ncol = length(estados_cond))
  dimnames(matriz_cond) = list(estados_cond, estados_cond)
  
  for (fila in estados_cond)
  {
    for (columna in estados_cond)
    {
      b = fila[[1]][1]
      j = columna[[1]][1]
      
      if (b == "F" & j == "D")
      {
        matriz_cond[fila, columna] = prob_opt_irre
      }
      if (b == "F" & j == "R")
      {
        matriz_cond[fila, columna] = prob_opt_reg
      }
      if (b == "R" & j == "F")
      {
        matriz_cond[fila, columna] = prob_reg_opt
      }
      if (b == "R" & j == "D")
      {
        matriz_cond[fila, columna] = prob_reg_irre
      }
      if (b == "F" & j == "F")
      {
        matriz_cond[fila, columna] = prob_opt_opt
      }
      if (b == "R" & j == "R")
      {
        matriz_cond[fila, columna] = prob_reg_reg
      }
      if (b == "D" & j == "D")
      {
        matriz_cond[fila, columna] = 1
      }
      
    }
  }
  rowSums(matriz_cond)
  
  estados = c()
  condiciones = c("F", "R", "D")
  for (a in 0:69)
  {
    for (b in condiciones)
    {
      estados = c(estados, paste(a * 10, b, sep = ","))
    }
  }
  
  matrizP = matrix(0, nrow = length(estados), ncol = length(estados))
  dimnames(matrizP) = list(estados, estados)
  for (fila in estados)
  {
    for (columna in estados)
    {
      a = strsplit(fila, ",")[[1]][1]
      b = (strsplit(fila, ",")[[1]][2])
      i = strsplit(columna, ",")[[1]][1]
      j = (strsplit(columna, ",")[[1]][2])
      
      matrizP[fila, columna] <- matriz_km[a, i] * matriz_cond[b, j]
    }
  }
  rowSums(matrizP)
  pos = c()
  contador = 0
  for (fila in estados) {
    a = as.numeric(strsplit(fila, ",")[[1]][1])
    b = (strsplit(fila, ",")[[1]][2])
    contador = contador + 1
    if (a == 690 | b == "D") {
      pos = c(pos, contador)
    }
    
  }
  pos
  
  #Matriz Transitorios a absorbentes
  R = matrizP[-pos, pos]
  
  #Matriz Transitorios a transitorio
  Q = matrizP[-pos, -pos]
  
  #Matriz Identidad
  I = matrix(0, nrow = length(Q[, 1]), ncol = length(Q[1, ]))
  for (i in 1:dim(I)[1]) {
    for (j in 1:dim(I)[2]) {
      if (i == j) {
        I[i, j] = 1
      }
    }
  }
  
  #inversa
  #Matriz t de tiempos de absorci?n
  matrizT = (I - Q)
  t = solve(matrizT)
  alfa <- c(1, rep(0, length(t[1, ]) - 1))
  tiempos_circulacion <- rowSums(alfa %*% t)
  
  
  #Matriz probabildiades en absorci?n
  vector_prob <- alfa %*% t %*% R
  
  #Gráfica probabilidades
  vector_graf <- c(rep(0, length(vector_prob) - 2))
  for (i in 1:(length(vector_prob) - 2))
  {
    vector_graf[i] <- vector_prob[i]
    
    if (i == (length(vector_prob) - 2))
    {
      vector_graf[i] <- vector_prob[i] + vector_prob[i + 1] + vector_prob[i +2]
    }
  }
  
  estados_graf <- c(rep(0, 69))
  for (i in 0:69)
  {
    estados_graf[i + 1] <- i * 10
  }
  data_frame_prob <- data.frame((vector_graf), estados_graf)
  
  plot_ly(
    data_frame_prob,
    x =  ~ estados_graf,
    y =  ~ vector_graf,
    name = "Probabilidad vida útil",
    type = "scatter",
    mode = "line+markers"
  )
  
  #Matriz probabilidades antes de absorción
  vector_prob2 <- alfa %*% t
  
  costo_acum_2 = 0
  for (i in 1:138)
  {
    if (i %% 2 == 0)
    {
      costo_acum_2 = costo_acum_2 + vector_prob2[i] * y
    }
    else
    {
      costo_acum_2 = costo_acum_2 + vector_prob2[i] * x
    }
  }
  costo_acum_2
  return(costo_acum_2)
}

shiny_function_3 = function(estado_tren)
{
  opt_opt <- 0
  opt_irre <- 0
  opt_reg <- 0
  
  reg_reg <-0
  reg_opt <- 0
  reg_irre <- 0
  
  for (i in 1:(length(estado_tren)-1))
  {
    if ( !(estado_tren[i]=="Fallas Irreparables" & estado_tren[i+1]=="Condiciones Óptimas"))
    {
      if (estado_tren[i] == "Condiciones Óptimas" &
          estado_tren[i+1] == "Condiciones Óptimas")
      {
        opt_opt = opt_opt + 1
      }
      if (estado_tren[i] == "Condiciones Óptimas" &
          estado_tren[i+1] == "Fallas Irreparables")
      {
        opt_irre = opt_irre + 1
      }
      if (estado_tren[i] == "Condiciones Óptimas" &
          estado_tren[i+1] == "Condiciones Regulares")
      {
        opt_reg = opt_reg + 1
      }
      if (estado_tren[i] == "Condiciones Regulares" &
          estado_tren[i+1] == "Condiciones Regulares")
      {
        reg_reg = reg_reg + 1
      }
      if (estado_tren[i] == "Condiciones Regulares" &
          estado_tren[i+1] == "Condiciones Óptimas")
      {
        reg_opt = reg_opt + 1
      }
      if (estado_tren[i] == "Condiciones Regulares" &
          estado_tren[i+1] == "Fallas Irreparables")
      {
        reg_irre = reg_irre + 1
      }
    }
  }
  
  prob_opt_opt = opt_opt / (opt_opt + opt_irre + opt_reg)
  prob_opt_irre = opt_irre / (opt_opt + opt_irre + opt_reg)
  prob_opt_reg = opt_reg / (opt_opt + opt_irre + opt_reg)
  
  prob_reg_reg = reg_reg / (reg_reg + reg_opt + reg_irre)
  prob_reg_opt = reg_opt / (reg_reg + reg_opt + reg_irre)
  prob_reg_irre = reg_irre / (reg_reg + reg_opt + reg_irre)
  
  estados_km <- c(rep(0, 69))
  
  for (i in 0:69)
  {
    estados_km[i + 1] <- i * 10
  }
  
  matriz_km = matrix(0,
                     nrow = length(estados_km),
                     ncol = length(estados_km))
  dimnames(matriz_km) = list(estados_km, estados_km)
  
  for (fila in 0:69)
  {
    for (columna in 0:69)
    {
      a = as.numeric(fila[[1]][1])
      i = as.numeric(columna[[1]][1])
      
      if (i - a >= 0 & i - a <= 3)
      {
        matriz_km[fila + 1, columna + 1] = 1 / 4
      }
      if (a == 67 & i == 69)
      {
        matriz_km[fila + 1, columna + 1] = 1 / 2
      }
      if (a == 68 & i == 69)
      {
        matriz_km[fila + 1, columna + 1] = 3 / 4
      }
      if (a == 69 & i == 69)
      {
        matriz_km[fila + 1, columna + 1] = 1
      }
    }
  }
  
  rowSums(matriz_km)
  
  estados_cond = c("F", "R", "D")
  
  matriz_cond = matrix(0,
                       nrow = length(estados_cond),
                       ncol = length(estados_cond))
  dimnames(matriz_cond) = list(estados_cond, estados_cond)
  
  for (fila in estados_cond)
  {
    for (columna in estados_cond)
    {
      b = fila[[1]][1]
      j = columna[[1]][1]
      
      if (b == "F" & j == "D")
      {
        matriz_cond[fila, columna] = prob_opt_irre
      }
      if (b == "F" & j == "R")
      {
        matriz_cond[fila, columna] = prob_opt_reg
      }
      if (b == "R" & j == "F")
      {
        matriz_cond[fila, columna] = prob_reg_opt
      }
      if (b == "R" & j == "D")
      {
        matriz_cond[fila, columna] = prob_reg_irre
      }
      if (b == "F" & j == "F")
      {
        matriz_cond[fila, columna] = prob_opt_opt
      }
      if (b == "R" & j == "R")
      {
        matriz_cond[fila, columna] = prob_reg_reg
      }
      if (b == "D" & j == "D")
      {
        matriz_cond[fila, columna] = 1
      }
      
    }
  }
  rowSums(matriz_cond)
  
  estados = c()
  condiciones = c("F", "R", "D")
  for (a in 0:69)
  {
    for (b in condiciones)
    {
      estados = c(estados, paste(a * 10, b, sep = ","))
    }
  }
  
  matrizP = matrix(0, nrow = length(estados), ncol = length(estados))
  dimnames(matrizP) = list(estados, estados)
  for (fila in estados)
  {
    for (columna in estados)
    {
      a = strsplit(fila, ",")[[1]][1]
      b = (strsplit(fila, ",")[[1]][2])
      i = strsplit(columna, ",")[[1]][1]
      j = (strsplit(columna, ",")[[1]][2])
      
      matrizP[fila, columna] <- matriz_km[a, i] * matriz_cond[b, j]
    }
  }
  rowSums(matrizP)
  pos = c()
  contador = 0
  for (fila in estados) {
    a = as.numeric(strsplit(fila, ",")[[1]][1])
    b = (strsplit(fila, ",")[[1]][2])
    contador = contador + 1
    if (a == 690 | b == "D") {
      pos = c(pos, contador)
    }
    
  }
  pos
  
  #Matriz Transitorios a absorbentes
  R = matrizP[-pos, pos]
  
  #Matriz Transitorios a transitorio
  Q = matrizP[-pos, -pos]
  
  #Matriz Identidad
  I = matrix(0, nrow = length(Q[, 1]), ncol = length(Q[1, ]))
  for (i in 1:dim(I)[1]) {
    for (j in 1:dim(I)[2]) {
      if (i == j) {
        I[i, j] = 1
      }
    }
  }
  
  #inversa
  #Matriz t de tiempos de absorci?n
  matrizT = (I - Q)
  t = solve(matrizT)
  alfa <- c(1, rep(0, length(t[1, ]) - 1))
  tiempos_circulacion <- rowSums(alfa %*% t)
  
  
  #Matriz probabildiades en absorci?n
  vector_prob <- alfa %*% t %*% R
  
  #Gráfica probabilidades
  vector_graf <- c(rep(0, length(vector_prob) - 2))
  for (i in 1:length(vector_prob) )
  {
    if(i <(length(vector_prob) - 2))
    {
      vector_graf[i] <- vector_prob[i]
    }
    else if(i==length(vector_prob) )
    {
      vector_graf[i-2] <- vector_prob[i]
    }
    
  }
  
  estados_graf <- c(rep(0, 69))
  for (i in 0:69)
  {
    estados_graf[i + 1] <- i * 10
  }
  data_frame_prob <- data.frame((vector_graf), estados_graf)
  
  plot_ly(
    data_frame_prob,
    x =  ~ estados_graf,
    y =  ~ vector_graf,
    name = "Probabilidad vida útil",
    type = "scatter",
    mode = "line+markers"
  )
  
  #Matriz probabilidades antes de absorción
  vector_prob2 <- alfa %*% t
  return(data_frame_prob)
}

shiny_function_4 = function(x,y,z)
{
library(MASS)
library(rriskDistributions)
library(fitdistrplus)
  # y=213
  # x=57
  # z=256
  #Matriz estados 1ra Mayo
  estados1=c(0:z)
  Mat1=matrix(0, nrow=z+1, ncol=z+1,dimnames=list(estados1,estados1))
  filas=estados1
  columnas=estados1


  for(i in filas){
    for(j in columnas){
      if( (i<z) & (j==i+1)){
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

  # rowSums(Mat1)

  library(expm)

  minutos=c(0:60)

  #Análisis transitorio: valor esperado 1ra Mayo

  esperado1= c(0:60)
  varianza1= c(0:60)

  for(i in 0:60)
  {
    alpha1<-c(1, rep(0, z))
    prob1<- alpha1%*%expm(Mat1*i)
    esperado1[i+1]=rowSums(prob1*estados1)
    varianza1[i+1]=rowSums(prob1*(estados1^2))-(rowSums(prob1*estados1))^2
  }
  
  data_frame<- data.frame((esperado1), minutos)
  data_frame
  
  plot_ly(
    data_frame,
    x =  ~ minutos,
    y =  ~ esperado1,
    name = "Probabilidad vida útil",
    type = "scatter",
    mode = "line+markers"
  )
  return(data_frame)
}

shiny_function_5 = function(x,y,z)
{
  library(MASS)
  library(rriskDistributions)
  library(fitdistrplus)
  

  #Matriz estados NQS
  estados2=c(0:x)
  Mat2=matrix(0, nrow=x+1, ncol=x+1)
  filas2=estados2
  columnas2=estados2

  for(i in filas2){
    for(j in columnas2){
      if( (i<x) & (j==i+1)){
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


  for(i in 1: length(estados2))
  {
    Mat2[i, i]=-sum(Mat2[i,])
  }

  # rowSums(Mat2)

  library(expm)

  minutos=c(0:60)

  #Análisis transitorio: valor esperado NQS

  esperado2= c(0:60)
  varianza2= c(0:60)

  for(i in 0:60)
  {
    alpha2<-c(1, rep(0, x))
    prob2<- alpha2%*%expm(Mat2*i)
    esperado2[i+1]=rowSums(prob2*estados2)
    varianza2[i+1]=rowSums(prob2*(estados2^2))-(rowSums(prob2*estados2))^2
  }

  data_frame<- data.frame((esperado2), minutos)
  data_frame

  plot_ly(
    data_frame,
    x =  ~ minutos,
    y =  ~ esperado1,
    name = "Probabilidad vida útil",
    type = "scatter",
    mode = "line+markers"
  )
  return(data_frame)
}

shiny_function_6 = function(x,y,z)
{

  library(MASS)
  library(rriskDistributions)
  library(fitdistrplus)

  #Matriz estados Caracas
  estados3=c(0:y)
  Mat3=matrix(0, nrow=y+1, ncol=y+1)
  filas3=estados3
  columnas3=estados3
  
  for(i in filas3){
    for(j in columnas3){
      if( (i<y) & (j==i+1)){
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
  
  library(expm)
  
  minutos=c(0:60)
  
  #Análisis transitorio: valor esperado Caracas
  
  esperado3= c(0:60)
  varianza3= c(0:60)
  
  for(i in 0:60)
  {
    alpha3<-c(1, rep(0, y))
    prob3<- alpha3%*%expm(Mat3*i)
    esperado3[i+1]=rowSums(prob3*estados3)
    varianza3[i+1]=rowSums(prob3*(estados3^2))-(rowSums(prob3*estados3))^2
  }
  
  data_frame<- data.frame((esperado3), minutos)
  data_frame
  return(data_frame)
}

shiny_function_7 = function(x,y,z)
{

  #Matriz estados 1ra Mayo
  estados1=c(0:z)
  Mat1=matrix(0, nrow=z+1, ncol=z+1,dimnames=list(estados1,estados1))
  filas=estados1
  columnas=estados1
  
  
  for(i in filas){
    for(j in columnas){
      if( (i<z) & (j==i+1)){
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
  
  library(expm)
  
  minutos=c(0:60)
  
  #Análisis transitorio: valor esperado 1ra Mayo
  
  esperado1= c(0:60)
  varianza1= c(0:60)
  
  for(i in 0:60)
  {
    alpha1<-c(1, rep(0, z))
    prob1<- alpha1%*%expm(Mat1*i)
    esperado1[i+1]=rowSums(prob1*estados1)
    varianza1[i+1]=rowSums(prob1*(estados1^2))-(rowSums(prob1*estados1))^2
  }

  
  data_frame<- data.frame((varianza1), minutos)
  data_frame
  return(data_frame)
}

shiny_function_8 = function(x,y,z)
{
  
  #Matriz estados NQS
  estados2=c(0:x)
  Mat2=matrix(0, nrow=x+1, ncol=x+1)
  filas2=estados2
  columnas2=estados2
  
  for(i in filas2){
    for(j in columnas2){
      if( (i<x) & (j==i+1)){
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
  
  
  for(i in 1: length(estados2))
  {
    Mat2[i, i]=-sum(Mat2[i,])
  }
  
  library(expm)
  
  minutos=c(0:60)
  
  #Análisis transitorio: valor esperado NQS
  
  esperado2= c(0:60)
  varianza2= c(0:60)
  
  for(i in 0:60)
  {
    alpha2<-c(1, rep(0, x))
    prob2<- alpha2%*%expm(Mat2*i)
    esperado2[i+1]=rowSums(prob2*estados2)
    varianza2[i+1]=rowSums(prob2*(estados2^2))-(rowSums(prob2*estados2))^2
  }
  
  data_frame<- data.frame((varianza2), minutos)
  data_frame
  return(data_frame)
}

shiny_function_9 = function(x,y,z)
{
  
  #Matriz estados Caracas
  estados3=c(0:y)
  Mat3=matrix(0, nrow=y+1, ncol=y+1)
  filas3=estados3
  columnas3=estados3
  
  for(i in filas3){
    for(j in columnas3){
      if( (i<y) & (j==i+1)){
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
  
 
  #Análisis transitorio: valor esperado Caracas
  
  esperado3= c(0:60)
  varianza3= c(0:60)
  
  for(i in 0:60)
  {
    alpha3<-c(1, rep(0, y))
    prob3<- alpha3%*%expm(Mat3*i)
    esperado3[i+1]=rowSums(prob3*estados3)
    varianza3[i+1]=rowSums(prob3*(estados3^2))-(rowSums(prob3*estados3))^2
  }

  
  data_frame<- data.frame((varianza3), minutos)
  data_frame
  return(data_frame)
}

shiny_function_10 = function(x,y,z)
{
  #Matriz estados 1ra Mayo
  estados1=c(0:z)
  Mat1=matrix(0, nrow=z+1, ncol=z+1,dimnames=list(estados1,estados1))
  filas=estados1
  columnas=estados1
  
  
  for(i in filas){
    for(j in columnas){
      if( (i<z) & (j==i+1)){
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
  
  library(expm)
  
  library(markovchain)
  #Análisis estado estable: 1ra Mayo
  cmtcMayo<-new( Class="ctmc", states= as.character(estados1), byrow=T, generator=Mat1)
  estable1<-t(Re(steadyStates(cmtcMayo)))
  
  data_frame<- data.frame((estable1), estados1)

  # plot_ly(
  #   data_frame,
  #   x =  ~ estados1,
  #   y =  ~ estable1,
  #   name = "Probabilidad vida útil",
  #   type = "scatter",
  #   mode = "line+markers"
  # )
  return(data_frame)
}

shiny_function_11 = function(x, y, z)
{
  
  estados2=c(0:x)
  Mat2=matrix(0, nrow=x+1, ncol=x+1)
  filas2=estados2
  columnas2=estados2
  
  for(i in filas2){
    for(j in columnas2){
      if( (i<x) & (j==i+1)){
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
  
  
  for(i in 1: length(estados2))
  {
    Mat2[i, i]=-sum(Mat2[i,])
  }
  
  library(expm)
  library(markovchain)
  
  cmtcNQS<-new( Class="ctmc", states= as.character(estados2), byrow=T, generator=Mat2)
  estable2<- t(Re(steadyStates(cmtcNQS)))
  
  data_frame<- data.frame((estable2), estados2)
  return(data_frame)
  
}

shiny_function_12 = function(x, y, z)
{
  estados3=c(0:y)
  Mat3=matrix(0, nrow=y+1, ncol=y+1)
  filas3=estados3
  columnas3=estados3
  
  for(i in filas3){
    for(j in columnas3){
      if( (i<y) & (j==i+1)){
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
  
  cmtcCaracas<-new( Class="ctmc", states= as.character(estados3), byrow=T, generator=Mat3)
  estable3<- t(Re(steadyStates(cmtcCaracas)))
  data_frame<- data.frame((estable3), estados3)
}

shiny_function_13 = function(costo1, costo2, epoca_entrada, estado_entrada)
{
  # costo1=150000
  # costo2=6000
  #Tasas
  tasas<-c(9,2,7,0.5)
  
  #Probabilidades
  probabilidades<-list()
  for(franja in 1:4)
  {
    probabilidades[[franja]]<-list()
    for(decision in 1:5)
    {
      probabilidades[[franja]][[decision]]<- matrix(0, nrow=151, ncol = 151)
    }
  }
  
  for(franja in 1:4)
  {
    for(decision in 1:5)
    {
      for(fila in 0:150)
      {
        for(columna in 0:150)
        {
          if(columna>0 & (columna+30*decision)<150 & 30*decision<150)
          {
            probabilidades[[franja]][[decision]][fila+1, columna+1]<- dpois(columna-fila+30*decision, 15*tasas[franja], log=FALSE)
          }
          else if ( columna==0 & (columna+30*decision)<150 & 30*decision<150)
          {
            probabilidades[[franja]][[decision]][fila+1, columna+1]<-ppois(30*decision-fila, 15*tasas[franja], lower.tail = TRUE, log.p=FALSE)
          }
          else if ((columna+30*decision)==150 & 30*decision<150)
          {
            probabilidades[[franja]][[decision]][fila+1, columna+1]<-1-ppois(150-fila-1, 15*tasas[franja], lower.tail = TRUE, log.p=FALSE)
          }
          else if (columna==0 & 30*decision>=150)
          {
            probabilidades[[franja]][[decision]][fila+1, columna+1]<-1
          }
        }
      }
      rowSums(probabilidades[[franja]][[decision]])
    }
  }
  
  ###----
  
  #Última época
  mat1<- matrix(1e15, nrow=151, ncol=72)
  mat2<- matrix("vacio", nrow=151, ncol=72)
  costo=0
  for(estado in 0:150)
  {
    min<-1e15
    
    for(decision in 1:5)
    {
      actual<-costo1*decision + estado*costo2
      
      for(estado2 in 0:150)
      {
        if(estado2==0)
        {
          costo_actual<- costo1
        }
        else
        {
          costo_actual<- costo1*ceiling(estado2/30)
        }
        actual<- actual + probabilidades[[4]][[decision]][estado +1, estado2+1]*costo_actual
      }
      
      if(actual<min)
      {
        mat1[estado+1, 72]<-actual
        mat2[estado+1, 72]<-decision
        min=actual
      }
      if(estado==estado_entrada)
      {
        costo=min
      }
    }
  }
  
  #Decisiones
  epocas<-c(rep(1, 20), rep(2, 20), rep(3, 16), rep(4, 16))
  
  
  for(epoca in 71:1)
  {
    for(estado in 0:150)
    {
      min<-1e15
      
      for(decision in 1:5)
      {
        actual<-costo1*decision+costo2*estado
        for(estado2 in 0:150)
        {
          actual<-actual + probabilidades[[epocas[epoca]]][[decision]][estado+1, estado2+1]*mat1[estado2+1, epoca+1]
        }
        
        if(actual<min)
        {
          mat1[estado+1, epoca]<-actual
          mat2[estado+1, epoca]<-decision
          min<-actual
        }
        if(epoca==epoca_entrada & estado==estado_entrada)
        {
          costo=min
        }
      }
    }
  }
  
  return(costo)
}

shiny_function_14 = function(costo1, costo2, epoca_entrada, estado_entrada)
{
  # costo1=150000
  # costo2=6000
  #Tasas
  tasas<-c(9,2,7,0.5)
  
  #Probabilidades
  probabilidades<-list()
  for(franja in 1:4)
  {
    probabilidades[[franja]]<-list()
    for(decision in 1:5)
    {
      probabilidades[[franja]][[decision]]<- matrix(0, nrow=151, ncol = 151)
    }
  }
  
  for(franja in 1:4)
  {
    for(decision in 1:5)
    {
      for(fila in 0:150)
      {
        for(columna in 0:150)
        {
          if(columna>0 & (columna+30*decision)<150 & 30*decision<150)
          {
            probabilidades[[franja]][[decision]][fila+1, columna+1]<- dpois(columna-fila+30*decision, 15*tasas[franja], log=FALSE)
          }
          else if ( columna==0 & (columna+30*decision)<150 & 30*decision<150)
          {
            probabilidades[[franja]][[decision]][fila+1, columna+1]<-ppois(30*decision-fila, 15*tasas[franja], lower.tail = TRUE, log.p=FALSE)
          }
          else if ((columna+30*decision)==150 & 30*decision<150)
          {
            probabilidades[[franja]][[decision]][fila+1, columna+1]<-1-ppois(150-fila-1, 15*tasas[franja], lower.tail = TRUE, log.p=FALSE)
          }
          else if (columna==0 & 30*decision>=150)
          {
            probabilidades[[franja]][[decision]][fila+1, columna+1]<-1
          }
        }
      }
      rowSums(probabilidades[[franja]][[decision]])
    }
  }
  
  ###----
  
  estado_entrada<-0
  epoca_entrada=1
  #Última época
  mat1<- matrix(1e15, nrow=151, ncol=72)
  mat2<- matrix("vacio", nrow=151, ncol=72)
  costo=0
  for(estado in 0:150)
  {
    min<-1e15
    
    for(decision in 1:5)
    {
      actual<-costo1*decision + estado*costo2
      
      for(estado2 in 0:150)
      {
        if(estado2==0)
        {
          costo_actual<- costo1
        }
        else
        {
          costo_actual<- costo1*ceiling(estado2/30)
        }
        actual<- actual + probabilidades[[4]][[decision]][estado +1, estado2+1]*costo_actual
      }
      
      if(actual<min)
      {
        mat1[estado+1, 72]<-actual
        mat2[estado+1, 72]<-decision
        min=actual
      }
      if(estado==estado_entrada)
      {
        costo=min
      }
    }
  }
  
  #Decisiones
  epocas<-c(rep(1, 20), rep(2, 20), rep(3, 16), rep(4, 16))
  
  
  for(epoca in 71:1)
  {
    for(estado in 0:150)
    {
      min<-1e15
      
      for(decision in 1:5)
      {
        actual<-costo1*decision+costo2*estado
        for(estado2 in 0:150)
        {
          actual<-actual + probabilidades[[epocas[epoca]]][[decision]][estado+1, estado2+1]*mat1[estado2+1, epoca+1]
        }
        
        if(actual<min)
        {
          mat1[estado+1, epoca]<-actual
          mat2[estado+1, epoca]<-decision
          min<-actual
        }
        if(epoca==epoca_entrada & estado==estado_entrada)
        {
          costo=min
        }
      }
    }
  }
  
  #Mapa de calor
  library(reshape2)
  library(ggplot2)
  decisionesOptimasLista <- melt(mat2[1:150,])
  decisionesOptimasLista$value <- as.factor(decisionesOptimasLista$value)
  colnames(decisionesOptimasLista) <- c("Estado", "Epoca", "Decision")
  
  ggplot(data = decisionesOptimasLista, aes(x = Epoca, y = Estado, fill = Decision)) +   geom_tile()
  
  return(decisionesOptimasLista)
}

shiny_function_15 = function()
{
  etiquetas="Negro: Caracas, Verde: NQS, Azul: 1raMayo"
  return(etiquetas)
}







