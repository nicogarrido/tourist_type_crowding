##Para entender la diferencia entre Modelo y Modelo1, ver el ppt "Proceso de Elección"


##Parameters of the model
#set.seed(4)
estado <- .Random.seed
#Para repetir el ultimo
#.Random.seed <- estado

Nr <- 10 #
qstar <- 10 #Capacidad de produccion
pstar = 1
M <- pstar * Nr * qstar #Ingreso exogeno
uCost <- 0.75
p0 <- runif(Nr,pstar,(2*pstar))
lambdaplus <- 0.02
lambdaminus <- 0.1


T <- 100
## Consumidores

##Resorts

RCapacity <- 1
RPrecio <- 2
RVentas <- 3
RUCost <- 4
RType <- 5
AtributosR <- 5

resorts <- matrix(0, Nr, AtributosR )
colnames(resorts) <- c("Q*", "P", "Vtas", "Cost", "Type")

resorts[,RPrecio] <- p0 #Precio Inicial
resorts[,RUCost] <- uCost #Costo
resorts[,RCapacity] <- qstar #No se cubre toda la capacidad

#INE recogiendo info
nroDatos = Nr
datos <- matrix(0, T, nroDatos)
datos2 <- matrix(0,T,4) #min, max, 1 y media
for (t in 1:T){
 if (t==99){
   l = 99 #No sirve para nada, es solo para controlar el debuggin
 } 
  Budget = M

  #Actualización de Precios
  for (i in 1:Nr){
    if (resorts[i,RVentas] < resorts[i,RCapacity]){
      resorts[i,RPrecio] <- max(resorts[i,RPrecio] - lambdaminus,resorts[i,RUCost])
    }else{
      resorts[i,RPrecio] <- resorts[i,RPrecio] + lambdaplus #Resorts does not know the maximum income
    }
  }
  resorts[,RVentas] <- 0
  
  indice = sort(resorts[,2],index.return=TRUE) #utilidad tiene el mismo orden que la memoria
  for (i in 1:Nr){
    idfirm <- indice$ix[i]
    for (q in 1:resorts[idfirm,RCapacity]){
      if (Budget > resorts[idfirm,RPrecio]){
        Budget = Budget - resorts[idfirm,RPrecio] 
        resorts[idfirm,RVentas] = resorts[idfirm,RVentas] + 1 
      }else{
        break
      }
      
    }
  }
  
  datos[t,] <- resorts[,RPrecio]
  datos2[t,1] <- min(resorts[,RPrecio])
  datos2[t,2] <- max(resorts[,RPrecio])
  datos2[t,3] <- mean(resorts[,RPrecio])
  datos2[t,4] <- resorts[1,RPrecio]
}

pstar = datos[,2] * 0
pstar = pstar + Nc * y / sum(resorts[,1])

plot(datos2[,1],type="l",ylim=c(0.7,1.5), xlab="time",ylab="Individual Prices")
colores <- c("incoloro","red", "blue", "yellow", "green")
for (i in 2:4){
  lines(datos2[,i], col=colores[i])
}


plot(datos[,1],type="l",ylim=c(0.7,1.5), xlab="time",ylab="Individual Prices")
colores <- c("incoloro","red", "blue", "yellow", "green")
for (i in 2:ncol(datos)){
  lines(datos[,i], col=colores[i])
}

