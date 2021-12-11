T <- 100
## Consumidores
Nc <- 50 #Numero total de consumidores
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
Nr <- 2
RCapacity <- 1
RPrecio <- 2
RVentas <- 3
RUCost <- 4
RType <- 5
AtributosR <- 5

turistas <- matrix(0, Nc, AtributosT)
resorts <- matrix(0, Nr, AtributosR )

touristtypes <- 2 #tipos de turistas
resorttypes <- 2 #tipos de resorts

congestion <- matrix(0, Nr, touristtypes)

#Condiciones iniciales
turistas[,Nctype] <- sample(touristtypes, Nc, replace=T)
turistas[,Nca] <- 0 #runif(Nc,1,2)
turistas[,Ncb] <- 1 #runif(Nc,1,2) * 0
turistas[,Ncc] <- 0 #runif(Nc,1,2) * 0
turistas[,NcPref] <- sample(resorttypes, Nc, replace = T) #Los turistas prefieren uno de los tipos de resorts
turistas[,NcCurrRe] <- sample(Nr, Nc, replace = T)

resorts[,RPrecio] <- 1 #Precio Inicial
resorts[,RUCost] <- 0.5 #Costo
resorts[,RCapacity] <- ceiling(Nc/Nr) #No se cubre toda la capacidad
#resorts[,RType] <- sample(resorttypes, Nr, replace=T)
resorts[,RType] <- 1:Nr
 listaResorts = 1:Nr

#Congestión inicial
for (i in 1:Nc){
  congestion[turistas[i,NcCurrRe],turistas[i,Nctype]] <- congestion[turistas[i,NcCurrRe],turistas[i,Nctype]] + 1
}

y = 1
epsilon <- 0.2
withPrice <- 1 #1 quiere decir que se toma en consideraicón el precio

#INE recogiendo info
nroDatos = Nr
datos <- matrix(0, T, nroDatos)
datosCong <- matrix(0,T,(Nr+touristtypes))
datosU <- matrix(0,T,1)
for (t in 1:T){
  
  turistas[,NcIncome] <- y #Toda la gente recibe el ingreso exógeno
  turistas[,NcPreRe] <- turistas[,NcCurrRe] #La memoria
  # if (t==80){
  #   auxmi = t * 5
  # }
  #Actualización de Precios
  for (i in 1:Nr){
    if ((resorts[i,RVentas]/resorts[i,RCapacity]) < 0.9){
      resorts[i,RPrecio] <- max(resorts[i,RPrecio] - 0.02,resorts[i,RUCost])
    }else{
      #resorts[i,RPrecio] <- min(resorts[i,RPrecio] + 0.02, y)
      resorts[i,RPrecio] <- resorts[i,RPrecio] + 0.02
    }
  }
  resorts[,RVentas] <- 0
  
  #Turistas se van de vacaciones
  C = 1
  listaAleatoria <- sample(Nc)
  for (i in 1:Nc){
    id <- listaAleatoria[i]
    if (turistas[id,NcPreRe] == 0){
      #Si no se fue de turismo el año pasado este año busca uno al azar!!
      newR <- sample(listaResorts,1)
      if ((resorts[newR,RPrecio]<=turistas[id,NcIncome]) & (resorts[newR,RVentas] < resorts[newR,RCapacity])){
        resorts[newR,RVentas] = resorts[newR,RVentas] + 1 #Elijo este
        turistas[id,NcCurrRe] = newR
      }
      next()
    }
    if (runif(1) < epsilon){ #El epsilon de turistas cambia
      #Prueba con algún resort
      newR <- sample(as.vector(listaResorts[-turistas[id,NcPreRe]]),1)
      preferencia = (turistas[id,Nca] / C) * ifelse(turistas[id,NcPref]==resorts[newR,RType], 1 ,0) 
      crowding = turistas[id,Ncb] * congestion[newR,turistas[id,Nctype]]/sum(congestion[newR,]) - turistas[id,Ncc] * sum(congestion[newR,-turistas[id,Nctype]] )/sum(congestion[newR,])  
      if (is.na(crowding)==TRUE){crowding=0}
      utilityNew = preferencia + crowding - withPrice* resorts[newR,RPrecio] #Utilidad del resort alternativo
      #Calculo la utilidad que obtuve en el que estuve anteriormente
      preferencia = turistas[id,Nca] / C * ifelse(turistas[id,NcPref]==resorts[turistas[id,NcCurrRe],RType], 1 ,0) 
      crowding = turistas[id,Ncb] * congestion[turistas[id,NcPreRe],turistas[id,Nctype]]/sum(congestion[turistas[id,NcPreRe],]) - turistas[id,Ncc] * sum(congestion[turistas[id,NcPreRe],-turistas[id,Nctype]] ) /sum(congestion[turistas[id,NcPreRe],]) 
      if (is.na(crowding)==TRUE){crowding=0}
      utilityOld = preferencia + crowding - withPrice*resorts[turistas[id,NcPreRe],RPrecio]
      #Cambia si hay lugar, si la utilidad corregia por precio es mejor y si el precio se puede pagar
      if ((utilityNew > utilityOld) & (resorts[newR,RPrecio]<=turistas[id,NcIncome]) & (resorts[newR,RVentas] < resorts[newR,RCapacity])){
          resorts[newR,RVentas] = resorts[newR,RVentas] + 1 #Se cambia de resort!!
          turistas[id,NcCurrRe] = newR
      }else{
        #Se queda en el viejo si puede pagarlo y hay lugar
        if ((resorts[turistas[id,NcPreRe],RPrecio]<=turistas[id,NcIncome]) & (resorts[turistas[id,NcPreRe],RVentas] < resorts[turistas[id,NcPreRe],RCapacity])){
          resorts[turistas[id,NcPreRe],RVentas] = resorts[turistas[id,NcPreRe],RVentas] + 1
          turistas[id,NcCurrRe] = turistas[id,NcPreRe]
        }else{
        #no se va de turismo, porque o no le alcanza para pagar o no hay lugar en los que quería
        turistas[id,NcCurrRe] = 0
        }
      }
    }else{
      #Se queda en el viejo si puede pagarlo y hay lugar
      if ((resorts[turistas[id,NcPreRe],RPrecio]<=turistas[id,NcIncome]) & (resorts[turistas[id,NcPreRe],RVentas] < resorts[turistas[id,NcPreRe],RCapacity])){
        resorts[turistas[id,NcPreRe],RVentas] = resorts[turistas[id,NcPreRe],RVentas] + 1
        turistas[id,NcCurrRe] = turistas[id,NcPreRe]
      }else{
        #no se va de turismo
        turistas[id,NcCurrRe] = 0
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
  
  datos[t,] = t(resorts[,RPrecio])
  for (k in 1:touristtypes){
    desde <- (k-1) * touristtypes + 1
    hasta <- k * touristtypes
    datosCong[t,desde:hasta] <- congestion[k,] 
  }

  #Compute maxUtility and compare
  auxU = 0
  auxUreal = 0
  for (id in 1:Nc){
    if (turistas[id,NcCurrRe]>0){
      preferencia = turistas[id,Nca] / C * ifelse(turistas[id,NcPref]==resorts[turistas[id,NcCurrRe],RType], 1 ,0) 
      crowding = turistas[id,Ncb] * congestion[turistas[id,NcCurrRe],turistas[id,Nctype]]/sum(congestion[turistas[id,NcCurrRe],]) - turistas[id,Ncc] * sum(congestion[turistas[id,NcCurrRe],-turistas[id,Nctype]] )/sum(congestion[turistas[id,NcCurrRe],])  
      realUtility = preferencia + crowding
      auxUreal = auxUreal + realUtility
      nada = paste(realUtility)
      maxUtility = turistas[id,Nca] / C + turistas[id,Ncb]
      auxU = auxU + maxUtility
    }
  }
  if (auxU > 0)  datosU[t] = auxUreal / auxU
}

plot(datos[,1],type="l")
lines(datos[,2], col="red")
