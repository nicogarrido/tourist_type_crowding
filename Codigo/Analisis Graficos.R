

#Grafico para el caso de tres firmas

plot(datos[,1],type="l",ylim=c(0.5,1.05), xlab="time",ylab="Individual Prices")
#colores <- c("incoloro","red", "blue", "yellow", "green")
lines(datos[,2], type="l", col="blue")
lines(datos[,3], type="l", col="red",lty=2)
legend(140,1,legend = c("Firm 1, type 1", "Firm 2, type 1", "Firm 3, type 2"),col=c("black","blue","red"),lty=c(1,1,2),cex=0.8)





#Figura 4
plot(datosU[,1], type="l", xlab="time", ylab="W and Excedent", ylim=c(0,1), col="black")
lines(datosU[,2], type="l", col="blue",lty=2)
lines(datoslpMlm, type="l", col="red",lty=3)
lines(datoslpmlm, type="l", col="green",lty=4)
legend(1,0.2,legend = c("Utility", "Excedent lplus = lminus","Excedent lplus > lminus","Excedent lplus < lminus"),
       col=c("black","blue","red","green"),lty=1:4,cex=0.8)