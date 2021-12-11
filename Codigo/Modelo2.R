##Para entender la diferencia entre Modelo y Modelo1, ver el ppt "Proceso de Elección"

distribuye <- function(N, dist, ordentipos){
  tipos = length(dist)
  if (sum(dist) != 1){
    print("Error en funcion inicial")
    return(0)
  }
  pos = ceiling(cumsum(dist)*N)
  res <- 1:N
  c <- 1
  res[1:pos[1]] <- ordentipos[c]
  if (tipos > 1){
    for (i in 1: (tipos-1)){
      desde <- pos[i] + 1
      hasta <- pos[i+1]
      c <- c + 1
      res[desde:hasta] <- ordentipos[c]
    }
  }
  return(res)
}
##Parameters of the model
monteCarlos <- 0
#set.seed(4)
estado <- .Random.seed
#Para repetir el ultimo
#.Random.seed <- estado
Nc <- 20 #Numero total de consumidores
y <- 3.5 #Ingreso exogeno
a <- 1 #Preferencia individual
b <- 1 #Preferencia por mi tipo
d <- 1 #Cuanto no prefiere el otro tipo
k <- 3 #Tamaño de la memoria
epsilon <- 1 #Exploracion
Nr <- 3 #
touristtypes <- 2 #tipos de turistas
resorttypes <- 2 #tipos de resorts
qstar <- c(5,6,11) #Capacidad de produccion
uCost <- 0.5
#p0 <- rnorm(Nr,1,0.3)
p0 <- runif(Nr,0.5,1)
withPrice <- 1 #1 quiere decir que se toma en consideraicón el precio
lambdaplus <- 0.04
lambdaminus <- 0.02
mu <- (1)
# distTypeTuristas <-  c(0.33, 0.33, 0.34)
# cdTT <- c(1, 2, 3)
# distPrefTuristas <- c(0.33,0.33, 0.34)
# cdPT <- c(1, 2, 3)
# distTypeResorts <-  c(0.33,0.33,0.34)
# cdTR <- c(1, 2, 3)
# distTypeTuristas <-  c(0.11, 0.11, 0.11, 0.11, 0.11, 0.11, 0.11, 0.11, 0.12)
# cdTT <- c(1, 2, 3,1,2,3,1,2,3)
# distPrefTuristas <- c(0.33,0.33, 0.34)
# cdPT <- c(1, 2, 3)
# distTypeResorts <-  c(0.33,0.33,0.34)
# cdTR <- c(1, 2, 3)
# distTypeTuristas <- c(0.25, 0.25, 0.25, 0.25)
# cdTT <- c(1, 2, 1, 2)
distTypeTuristas <- c(0.5, 0.5)
cdTT <- c(1, 2)
distPrefTuristas <- c(10, 10)
#distPrefTuristas <- c(8, 2, 2, 8)
#distPrefTuristas <- c(5, 5, 5, 5)
#distPrefTuristas <- c(2, 8, 8, 2)

distPrefTuristas <- distPrefTuristas/Nc
cdPT <- c(2, 1, 1, 2)

distTypeResorts <-  c(0.33, 0.33, 0.34)
cdTR <- c(1, 1, 2)


T <- 500
## Consumidores

NcCurrRe <- 1 #Current Resorts
NcPreRe <- 2 # Previous Resort
NcPref <- 3 #Preference of tourist
Nctype <- 4
Nca <- 5
Ncb <- 6
Ncc <- 7
NcU <- 8
NcIncome <- 9
AtributosT <- 9

##Resorts

RCapacity <- 1
RPrecio <- 2
RVentas <- 3
RUCost <- 4
RType <- 5
AtributosR <- 5

turistas <- matrix(0, Nc, AtributosT)
colnames(turistas) <- c("Re_t", "Re_t-1", "Pref", "Type","a","b","c","U","Income" )
resorts <- matrix(0, Nr, AtributosR )
colnames(resorts) <- c("Q*", "P", "Vtas", "Cost", "Type")


congestion <- matrix(0, Nr, touristtypes)

#Condiciones iniciales
#turistas[,Nctype] <- sample(touristtypes, Nc, replace=T)
turistas[,Nctype] <- distribuye(Nc, distTypeTuristas, cdTT)
turistas[,Nca] <- a #runif(Nc,1,2)
turistas[,Ncb] <- b #runif(Nc,1,2) * 0
turistas[,Ncc] <- d #runif(Nc,1,2) * 0
#turistas[,NcPref] <- sample(resorttypes, Nc, replace = T) #Los turistas prefieren uno de los tipos de resorts
turistas[,NcPref] <- distribuye(Nc, distPrefTuristas, cdPT)
#turistas[,NcCurrRe] <- sample(Nr, Nc, replace = T)
turistas[,NcCurrRe] <- -1



resorts[,RPrecio] <- p0 #Precio Inicial
resorts[,RUCost] <- uCost #Costo
if (length(qstar)>1){
  resorts[,RCapacity] <- qstar #No se cubre toda la capacidad
}else{
  resorts[,RCapacity] <- qstar #No se cubre toda la capacidad
}
#resorts[,RType] <- sample(resorttypes, Nr, replace=T)
resorts[,RType] <- distribuye(Nr,distTypeResorts, cdTR)

listaResorts = 1:Nr

#Congestión inicial
turistas[,NcCurrRe] = sample(Nr, Nc, replace=T)
#Filas resorts, columnas tipos de turistas
for (i in 1:Nc){
  if (turistas[i,NcCurrRe] > 0){
    congestion[turistas[i,NcCurrRe],turistas[i,Nctype]] <- congestion[turistas[i,NcCurrRe],turistas[i,Nctype]] + 1
  }
}
init1 = ceiling(runif(1,0,5))
init2 = ceiling(runif(1,0,10))
congestion[1,] <- c(init1,5-init1)
init1 = ceiling(runif(1,0,5))
congestion[2,] <- c(init1,5-init2)
congestion[2,] <- c(init2,10-init2)

#congestion[1,] <- c(5,0)
#congestion[2,] <- c(5,0)
#congestion[3,] <- c(0,10)

memoria <- matrix(0, Nc, (k+1)) #memoria de visitas históricas que tienen los turistas, Es +1 por los nuevos
for (i in 1:Nc){
  memoria[i,1:k] <- sample(Nr,k)
}

#INE recogiendo info
nroDatos = Nr
datos <- matrix(0, T, nroDatos)
datosCong <- matrix(0,T,(Nr*touristtypes))
datosU <- matrix(0,T,2) #Primera columna solo utilidad, segunda excednete del consumiro
UsoMemoria = matrix(0,T,1)
dTurista <- matrix(0,T,5)
elTurista <- 11
#****************************************************
for (t in 1:T){
 if (t==74){
   l = 99 #No sirve para nada, es solo para controlar el debuggin
 } 
  turistas[,NcIncome] <- y #Toda la gente recibe el ingreso exógeno
  #Se actualizan las memorias de las personas
  for (i in 1:Nc){
    if (turistas[i,NcCurrRe]!=-1){ #Si estuvo en algún hotel
      if (length(intersect(memoria[i,1:k],turistas[i,NcCurrRe]))==0){ #Si el hotel donde estuvo, no lo tenía en la memoria
        idxs = which(memoria[i,1:k]==0) 
        if (length(idxs)>0){
          #Hay lugares vacíos
          memoria[i,idxs[1]] <- turistas[i,NcCurrRe]
        }else{
          #Se borra un lugar de la memoria
          if (k>1){
            memoria[i,1:(k-1)] <- memoria[i,2:k]        
          }
          memoria[i,k] <- turistas[i,NcCurrRe]
        }
      }
    }
  }
  turistas[,NcPreRe] = turistas[,NcCurrRe]
  turistas[,NcCurrRe] = -1 #Se comienza de cero para este año

  # if (t==80){
  #   auxmi = t * 5
  # }
  #Actualización de Precios
  for (i in 1:Nr){
    if ((resorts[i,RVentas]/resorts[i,RCapacity]) < mu){
      resorts[i,RPrecio] <- max(resorts[i,RPrecio] - lambdaminus,resorts[i,RUCost])
    }else{
      resorts[i,RPrecio] <- min(resorts[i,RPrecio] + lambdaplus, y) #Resorts know the maximum income

#      resorts[i,RPrecio] <- resorts[i,RPrecio] + lambdaplus #Resorts does not know the maximum income
    }
  }
  resorts[,RVentas] <- 0
  
  #Turistas se van de vacaciones
  C = 1
  listaAleatoria <- sample(Nc)
  for (i in 1:Nc){
    id <- listaAleatoria[i]
    newR <- 0
    alt_no_visitadas = setdiff(listaResorts,memoria[id,1:k])

    if ((length(alt_no_visitadas)==Nr) | (runif(1) < epsilon)){ #El epsilon de turistas que cambia
      if (length(alt_no_visitadas)==1){
        newR = alt_no_visitadas
      }else{
        if (length(alt_no_visitadas)>1){
          newR <- sample(alt_no_visitadas,1)          
        }
      }
    }
    
    limite = k
    if (newR>0){ 
      #Al nuevo hotel lo pongo en una memoria de trabajo
      #No va a la memoria de trabajo directamente
      limite = k+1
      memoria[id,limite] <- newR
    }
    utilidad = matrix(0, limite, 1) #Para calcular las utilidades de los hoteles en la memoria
    for (j in 1:limite){
      if (memoria[id,j]>0){ #Si tiene un hotel en la posicion j de la memoria
        elResort = memoria[id,j]
        preferencia = (turistas[id,Nca] / C) * ifelse(turistas[id,NcPref]==resorts[elResort,RType], 1 ,0) 
        crowding = turistas[id,Ncb] * congestion[elResort,turistas[id,Nctype]]/sum(congestion[elResort,]) - turistas[id,Ncc] * sum(congestion[elResort,-turistas[id,Nctype]] )/sum(congestion[elResort,])  
        if (is.na(crowding)==TRUE){crowding=0}
        utilidad[j] = preferencia + crowding - withPrice* resorts[elResort,RPrecio] #Utilidad del resort alternativo
      }
    }
    indice = sort(utilidad,decreasing=TRUE,index.return=TRUE) #utilidad tiene el mismo orden que la memoria
    #Se toma el que se puede
    for (j in 1:limite){
      if (memoria[id,indice$ix[j]]>0){ #Si en este lugar de la memoria hay un hotel
        elResort = memoria[id,indice$ix[j]]
        if ((resorts[elResort,RPrecio]<=turistas[id,NcIncome]) & (resorts[elResort,RVentas] < resorts[elResort,RCapacity])){
          resorts[elResort,RVentas] = resorts[elResort,RVentas] + 1 #Elijo este
          turistas[id,NcCurrRe] = elResort
          break
        }else{
          turistas[id,NcCurrRe] = -1
        }
      }
    }
  }#fin del For   
  
    #preferencia = turistas[i,Nca] / C * ifelse(turistas[i,NcPref]==turistas[i,NcCurrRe], 1 ,0) 
    #crowding = turistas[i,Ncb] * congestion[turistas[i,NcCurrRe],turistas[i,Nctype]] - turistas[i,Ncc] * sum(congestion[turistas[i,NcCurrRe],-turistas[i,Nctype]] )  
    #turistas[i,NcU] = preferencia + crowding
    
    
  #Calcular la congestión en los centros turisticos
  congestion <- congestion * 0
  for (i in 1:Nc){
    if (turistas[i,NcCurrRe] > 0){
      congestion[turistas[i,NcCurrRe],turistas[i,Nctype]] <- congestion[turistas[i,NcCurrRe],turistas[i,Nctype]] + 1
    }
  }
  #Guarda datos de congestión para el análisis
  datos[t,] = t(resorts[,RPrecio])
  for (j in 1:Nr){
    desde <- (j-1) * touristtypes + 1
    hasta <- j * touristtypes
    datosCong[t,desde:hasta] <- congestion[j,] 
  }

  #Compute maxUtility and compare
  auxU = 0
  auxUreal = 0
  auxE = 0
  auxEreal = 0
  for (id in 1:Nc){
    maxUtility = turistas[id,Nca] / C + turistas[id,Ncb] #Maxima utilidad que puede obtener un turista
    if (turistas[id,NcCurrRe]>0){
      preferencia = turistas[id,Nca] / C * ifelse(turistas[id,NcPref]==resorts[turistas[id,NcCurrRe],RType], 1 ,0) 
      crowding = turistas[id,Ncb] * congestion[turistas[id,NcCurrRe],turistas[id,Nctype]]/sum(congestion[turistas[id,NcCurrRe],]) - turistas[id,Ncc] * sum(congestion[turistas[id,NcCurrRe],-turistas[id,Nctype]] )/sum(congestion[turistas[id,NcCurrRe],])  
      realUtility = preferencia + crowding
      auxUreal = auxUreal + realUtility
      auxEreal = auxEreal + max((realUtility - resorts[turistas[id,NcCurrRe],RPrecio]),0)
      auxE = auxE + maxUtility - resorts[turistas[id,NcCurrRe],RUCost]
      #nada = paste(realUtility)
    }

    auxU = auxU + maxUtility
    #auxE = auxE + maxUtility - Nc * y / sum(resorts[,1])
    
  }
  if (auxU > 0) {
    datosU[t,1] = auxUreal / auxU
    datosU[t,2] = auxEreal / auxE
  } 
  
  #Uso de la memoria
  #Note que el denominador va por Nr, en lugar de k!
  UsoMemoria[t] = 1 - (length(which(memoria[,1:k]==0))+(Nc*(Nr-k)))/(Nc * Nr)
  
  #Dibuja dinámica
  rActual <- turistas[elTurista,NcCurrRe]
  rPrev <- turistas[elTurista,NcPreRe]
  elTipo <- turistas[elTurista,Nctype]
  #elOtro <- setdiff(1:touristtypes,elTipo)
  if (rActual == 3){
    rrPrev <- 1
  }else{
    rrPrev <- 3
  }
  dTurista[t,1] <- congestion[rPrev,elTipo]/sum(congestion[,elTipo])-congestion[rActual,elTipo]/sum(congestion[,elTipo])
  dTurista[t,2] <- resorts[rPrev,RPrecio] - resorts[rActual,RPrecio]
  dTurista[t,3] <- congestion[rActual,elTipo]/sum(congestion[rActual,])-congestion[rrPrev,elTipo]/sum(congestion[rrPrev,])
  dTurista[t,4] <- resorts[rActual,RPrecio] - resorts[rrPrev,RPrecio]
  dTurista[t,5] <- rActual
}
#***************************************************************
if (monteCarlos == 0){
  pstar = datos[,2] * 0
  pstar = pstar + Nc * y / sum(resorts[,1])

  plot(rowMeans(datos),type="l",ylim=c((c-0.05),(y + 0.05)), xlab="time",ylab="Mean price")
  lines(pstar, col="red")
  lines(UsoMemoria, col='blue')


  plot(datos[,1],type="l",ylim=c((c-0.05),(y + 0.05)), xlab="time",ylab="Individual Prices")
  colores <- c("incoloro","red", "blue", "yellow", "green")
  for (i in 2:ncol(datos)){
    lines(datos[,i], col=colores[i])
  }

  plot(datosU[,1], type="l", xlab="time", ylab="w", ylim=c(0,1), col="red")

# #Welfare
# # par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
# # plot(datosU[,1], type="l", xlab="time", ylab="W", ylim=c(0,1), col="red")
# # par(new = TRUE)                             # Add new plot
# # plot(datosU[,2], type="l", xlab="", ylab="", axes=FALSE, col="blue",lty=2)
# # axis(side = 4, at = pretty(range(datosU[,2])))      # Add second axis
# # mtext("Excedent", side = 4, line = 3)
# # legend(1,0.2,legend = c("Utility", "Excedent"),col=c("red","blue"),lty=1:2,cex=0.8)
# # #plot(datosU, type="l")
#
# #par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
  plot(datosU[,1], type="l", xlab="time", ylab="W and Excedent", ylim=c(0,1), col="red")
  lines(datosU[,2], type="l", col="blue",lty=2)
  legend(1,0.2,legend = c("Utility", "Excedent"),col=c("red","blue"),lty=1:2,cex=0.8)
# #plot(datosU, type="l")
  print(datosCong[10,])
}