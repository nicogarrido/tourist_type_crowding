##Diagram

a = 5
b = 3
d = 2

alpha = seq(0,1,0.1)
unos = rep(1,length(alpha))

ypref = a + (b+d)*alpha - d
ynopref = (b+d)*alpha - d

ymin = min(ypref,ynopref)
ymax = max(ypref,ynopref)
eje = min(ypref) * unos
change = max(ynopref) * unos

plot(alpha, ypref, type="l", col="red",ylim=c(ymin,ymax))
lines(alpha, ynopref,col="blue")
lines(alpha,eje)
lines(alpha,change)

