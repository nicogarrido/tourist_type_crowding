###Un poquito de MonteCarlo

MCT = 100
MCDatos = matrix(0,MCT,2)
T = 500
rango = ((T-50):T)

for (ttt in 1:MCT){
  source('~/Dropbox/Disco/Investigacion/Turismo/Agent Based/Codigo/Modelo2.R')
  MCDatos[ttt,1] <- mean(datosU[rango,1]) #Utility
  MCDatos[ttt,2] <- mean(datosU[rango,2]) #Excedent
  
}

print(paste(mean(MCDatos[,1]),var(MCDatos[,1])))
print(paste(mean(MCDatos[,2]),var(MCDatos[,2])))

print(summary(MCDatos[,1]))
print(summary(MCDatos[,2]))
