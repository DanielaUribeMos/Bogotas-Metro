#Archivo de datos

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

#Costo chatarrizacion
# costo_acum = 0
# for (i in 1:72)
# {
#   if (i >= 1 & i <= 69)
#   {
#     costo_acum = costo_acum + vector_prob[i] * 76500000
#   }
#   if (i>69 & i<=71)
#   {
#     costo_acum = costo_acum + vector_prob[i] * 85000000
#   }
#   if(i>71)
#   {
#     costo_acum = costo_acum + vector_prob[i] * ((76500000+85000000)/2)
#   }
# }
# costo_acum
# 
# 
# #Costo operación
# costo_acum_2 = 0
# for (i in 1:138)
# {
#   if (i %% 2 == 0)
#   {
#     costo_acum_2 = costo_acum_2 + vector_prob2[i] * 12500000
#   }
#   else
#   {
#     costo_acum_2 = costo_acum_2 + vector_prob2[i] * 10000000
#   }
# }
# costo_acum_2



#Funcion Shiny

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