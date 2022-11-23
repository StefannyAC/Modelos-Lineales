datos <- read.csv2(file.choose(), header = T, sep=";")
P <- datos$Precio.millones
Km <- datos$Km.miles 
tr <- datos$Transmision #Factor 0 automatica 1 mecánica
di <- datos$Direction #Factor
c <- datos$Color #Factor
v <- datos$Version #Factor
y <- datos$Year
a <- as.character(datos$Año)
shapiro.test(P) #Dado que P no es normal, usamos una transformación
hist(P)
w <- P^(1/2)
shapiro.test(w)
hist(w)
plot(density(w))
require(nortest) #esto es para hacer varias pruebas de normalidad 
valoresp <- c(shapiro.test(w)$p.value, ad.test(w)$p.value, cvm.test(w)$p.value, lillie.test(w)$p.value, pearson.test(w)$p.value)
valoresp

modelo0 <- lm(w~y) #Sig 4.7e-15 RSE 0.3213 R^2 0.7422 
summary(modelo0)
plot(y,w)
abline(modelo2, col ="red", lwd =2)

#Como y es el más significativo, a ese modelo comenzaremos a agregarle variables
modelo1 <- lm(w~y+v) #Sig 2.977e-15 R^2 0.8661 RSE 0.2316 ---> Este está mejor que el primero
summary(modelo1)
anova(modelo0,modelo1) #Por los residuales y el R^2adj , es mejor este modelo anova arroja p 4.406e-05

#Eligiendo el modelo9 seguiremos adicionándole variables a este
modelo2 <- lm(w~y+v+tr) #Sig <2.2e-16 RSE 0.1691 R^2 0.9286 
summary(modelo2)
anova(modelo1,modelo2) #1.377e-06

#Si dejamos el tr y adicionamos c, obtenemos el mejor modelo RSE 0.1586 R^2 0.9372 p <2.2e-16
modelo3 <- lm(w~y+v+tr+c) 
summary(modelo3)
plot(modelo3)
shapiro.test(modelo3$residuals) #Los residuales son normales

#PARA LAS GRÁFICAS 
#COLOR VS AÑO
library(ggplot2)
library(scales)
g = ggplot(datos, aes(a, fill=c) ) +
  labs(title = "Color vs año")+ylab("") +xlab("Año")+
  theme(plot.title = element_text(size = rel(1), colour = "black")) #0000CC

g+geom_bar(position="dodge2", size=0.1) + scale_fill_manual(values = alpha(c("#003399", "#339966","gray", "black","#666666","#CC0000"), 1))
  
#PRECIO VS COLOR
#stripchart(P~c,method="overplot",vertical=TRUE,pch=19, col ="#339966",
           #main="Precio vs Color",ylab="")
g3 = ggplot(datos, aes(c, w) ) +
  labs(title = "Precio vs Color")+ylab("Raiz precio en millones") + xlab("Color")+
  theme(plot.title = element_text(size = rel(1), colour = "black")) #0000CC

gg1 = g3+geom_point( col = "#339966")

Grafico1 = gg1 + 
  theme (axis.text.x = element_text(angle=45, hjust=1))
Grafico1 + coord_fixed(ratio=(0.3)/(0.2)) +scale_y_continuous(limit = c(4,7))


#PRECIO VS TRANSMISION
#(P~tr,method="overplot",vertical=TRUE,pch=19, col = "#339966",
           #main="Precio vs Transmisión",ylab="",xlim = c(0,3))
g4 = ggplot(datos, aes(tr, w) ) +
  labs(title = "Precio vs Transmisión")+ylab("Raiz precio en millones") + xlab("Transimisión")+
  theme(plot.title = element_text(size = rel(1), colour = "black")) #0000CC

gg4 = g4+geom_point( col = "#339966") 

gg4 + coord_fixed(ratio=(0.04)/(0.06)) +scale_y_continuous(breaks = seq(4,8,1.2))

#PRECIO VS DIRECCION
#stripchart(P~di,method="overplot",vertical=TRUE,pch=19,lty=1:5, col ="#339966",
           #main="Precio vs Dirección",ylab="", xlim = c(0,3))
g5 = ggplot(datos, aes(di, w) ) +
  labs(title = "Precio vs Dirección")+ylab("Raiz precio en millones") + xlab("Dirección")+
  theme(plot.title = element_text(size = rel(1), colour = "black")) #0000CC

gg5 = g5+geom_point( col = "#339966") 
gg5 + coord_fixed(ratio=(0.04)/(0.06)) +scale_y_continuous(breaks = seq(4,8,1.2))

#PRECIO VS AÑO
#stripchart(P~y,method="overplot",vertical=TRUE,pch=19, col ="#339966",
           #main="Precio vs Año",ylab="")

g6 = ggplot(datos, aes(y, w) ) +
  labs(title = "Precio vs Año")+ylab("Raiz precio en millones") + xlab("Año")+
  theme(plot.title = element_text(size = rel(1), colour = "black")) #0000CC

gg6 = g6+geom_point( col = "#339966") 
gg6 + coord_fixed(ratio=(0.1)/(0.04)) +scale_y_continuous(limit = c(4,7))+scale_x_continuous(breaks=seq(2009,2018,3))

#PRECIO VS KILOMETRAJE
#plot(Km,P,pch=19, col ="#339966", main="Precio vs Kilometraje", ylab="", xlab="")

g1 = ggplot(datos, aes(Km, w) ) +
  labs(title = "Precio vs Kilometraje")+ylab("Raiz precio en millones") +xlab("Kilometraje en miles")+
  theme(plot.title = element_text(size = rel(1), colour = "black")) #0000CC

gg3=g1+geom_point( col = "#339966") 
gg3 + coord_fixed(ratio=(2)/(0.045)) +scale_y_continuous(limit = c(4,7))+scale_x_continuous(breaks=seq(12,175,70))
#PRECIO VS VERSION
#stripchart(P~v,method="overplot",vertical=TRUE,pch=19, col ="#339966",
           #main="Precio vs Versión",ylab="" , cex.axis=0.7, las=3)

g2 = ggplot(datos, aes(v, w)) +
  labs(title = "Precio vs Versión")+ylab("Raiz precio en millones") + xlab("Versión")+
  theme(plot.title = element_text(size = rel(1), colour = "black"))

gg = g2+geom_point(col = "#339966") 

Grafico2 = gg + 
  theme (axis.text.x = element_text(angle=45, hjust=1))
Grafico2 + coord_fixed(ratio=(0.2)/0.11) +scale_y_continuous(limit = c(4,7))
