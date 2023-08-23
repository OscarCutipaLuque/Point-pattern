#=========================================
# Curso: Estadística Espacial
# Tema : Procesos Puntuales
# Autor: Oscar Cutipa Luque
#=========================================
# Cargar Librerías
library(spatstat)
# Data a utilizar
bei

par(mar = c(1, 1, 1, 1))
plot(bei$window, main = "Ubicaciones de árboles en la selva tropical", col = "skyblue")
plot(bei, cex = 0.3,col = "black",alpha = 0.5, main = "Ubicaciones de árboles en la selva tropical",add = TRUE)

# Covariable espacial elevación
plot(bei.extra$elev, main = "Elevación de la selva tropical")
plot(bei, cex = 0.4,alpha = 0.2, add = TRUE, col = "black")

# Covariable espacial Grad
plot(bei.extra$grad, main ="Pendiente de la selva tropical",col = colorRampPalette(c("blue", "green")))
plot(bei, cex = 0.4,alpha = 0.2, add = TRUE, col = "black")

# Conteo de cuadrante
CD = quadratcount(bei)
quadrat.test(CD)
# Prueba chi cuadrado
quadrat.test(bei)
plot(CD, main = "Conteo", col = "black", main = "Conteo de cuadrantes")
plot(bei,add = TRUE,cex = 0.2, col = "black")
# Función 
K.bei = Kest(bei)
plot(Kest(bei, var.approx = TRUE))
E <- envelope(bei, Kest, nsim = 50, correction = 'border')
plot(E)

# Intensidad por kernels
Den.pp = density(bei)
plot(Den.pp, main = "Densidad estimada por Kernels")
plot(bei, cex = 0.4,alpha = 0.2, add = TRUE, col = "black")
# Modelo
attach(bei.extra)
# Primer modelo
Model1 = ppm(bei ~ elev)
Model2 = ppm(bei ~ grad)
Model3 = ppm(bei ~ grad + elev)

AIC(Model1)
AIC(Model2)
AIC(Model3)
as.numeric(logLik(Model1))
logLik(Model2)
logLik(Model3)

D = data.frame(Modelo1 = c(AIC(Model1),as.numeric(logLik(Model1))),
               Modelo2 = c(AIC(Model2),as.numeric(logLik(Model2))),
               Modelo3 = c(AIC(Model3),as.numeric(logLik(Model3))))
rownames(D) = c("AIC","LogLikelihood")
library(kableExtra)
kbl(D)
D %>%
  kbl(caption = "Indicadores del Modelo") %>%
  kable_classic(full_width = F, html_font = "Cambria")

