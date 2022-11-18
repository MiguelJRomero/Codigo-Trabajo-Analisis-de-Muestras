#### Código Trabajo Análisis de Muestras - 2ª Parte ####

##############################################################################################################

### Generación de datos ###


if (!require("sae")) install.packages("sae")
library("sae")

NIU = 100452943
set.seed(NIU)
cual = sample(1:17,1)   # Devuelve 8: Castilla-La Mancha

data(incomedata)
datosECV = incomedata
datos16 = subset(datosECV, (datosECV$labor > 0))
datos16$age = datos16$age - 1
nrows = dim(datos16)[[1]]
datos16$horas = round(rnorm(nrows,34,3), 1)
datos16$horas[(datos16$labor == 2) | (datos16$labor == 3)] = 0
datos16$income = round(jitter(datos16$income), 1)
datos16$income[datos16$labor == 2] = datos16$income[datos16$labor == 2]*0.7
datos16$income[datos16$labor == 3] = 0
datosFinal = data.frame(ca = datos16$ac, prov = datos16$prov, 
                        provlab = datos16$provlab, gen = datos16$gen, 
                        edad = datos16$age, nac = datos16$nat, 
                        neduc=datos16$educ, sitemp = datos16$labor, 
                        ingnorm = datos16$income, horas = datos16$horas,
                        factorel = round(datos16$weight, 1))

datos_CastillaLaMancha = datosFinal[datosFinal[,1] == 8,]
write.table(datos_CastillaLaMancha, "datos_CastillaLaMancha.txt", row.names = FALSE)

datos = datos_CastillaLaMancha


##############################################################################################################

### Estimación de los ingresos medios de empleados de cada sexo ###

##############################################################################################################


### Definición de variables ###


datos.M = datos[which(datos$sitemp == 1 & datos$gen == 1),]  # Datos de empleados masculinos
datos.F = datos[which(datos$sitemp == 1 & datos$gen == 2),]  # Datos de empleadas femeninas

ing.M = datos.M[,9]             # Ingresos anuales de empleados masculinos
ing.F = datos.F[,9]             # Ingresos anuales de empleadas femeninas

w.M = datos.M[,11]              # Pesos de muestreo para empleados masculinos
w.F = datos.F[,11]              # Pesos de muestreo para empleadas femeninas

pinc.M = 1/w.M                  # Probabilidades de inclusión de empleados masculinos
pinc.F = 1/w.F                  # Probabilidades de inclusión de empleadas femeninas

N.M = round(sum(datos.M[,11]))  # Estimación de tamaño poblacional de empleados masculinos
N.F = round(sum(datos.F[,11]))  # Estimación de tamaño poblacional de empleadas femeninas

##############################################################################################################

### Estimación de los ingresos medios ###

if (!require("mase")) install.packages("mase")
library("mase")

media.M = as.numeric(horvitzThompson(ing.M, pi = pinc.M, N = N.M)[2])  # Estimación del ingreso medio anual de empleados masculinos.
media.F = as.numeric(horvitzThompson(ing.F, pi = pinc.F, N = N.F)[2])  # Estimación del ingreso medio anual de empleadas femeninas.

# media.M = sum(ing.M/(N.M*pinc.M))
# media.F = sum(ing.F/(N.F*pinc.F))

c(media.M, media.F)

##############################################################################################################

### Estimación de los errores de muestreo ###

Matriz.pinc.M = pinc.M%*%t(pinc.M)  # Matriz de probabilidades de inclusión de segundo orden de empleados masculinos.
diag(Matriz.pinc.M) = pinc.M
Matriz.pinc.F = pinc.F%*%t(pinc.F)  # Matriz de probabilidades de inclusión de segundo orden de empleadas femeninas.
diag(Matriz.pinc.F) = pinc.F        

EM.M = sqrt(as.numeric(horvitzThompson(ing.M, pi = pinc.M, pi2 = Matriz.pinc.M,   # Estimación del error muestral del estimador de Horvitz-Thompson del ingreso medio de empleados masculinos.
                                       N = N.M, var_est = TRUE, 
                                       var_method = "LinHT")[4]))
EM.F = sqrt(as.numeric(horvitzThompson(ing.F, pi = pinc.F, pi2 = Matriz.pinc.F,   # Estimación del error muestral del estimador de Horvitz-Thompson del ingreso medio de empleadas femeninas.
                                       N = N.F, var_est = TRUE,
                                       var_method = "LinHT")[4]))

# EM.M = sqrt(sum((1-pinc.M)/(pinc.M^2)*(ing.M/N.M)^2))
# EM.F = sqrt(sum((1-pinc.F)/(pinc.F^2)*(ing.F/N.F)^2))

round(c(EM.M, EM.F), 5)

##############################################################################################################

### Intervalos de confianza de los ingresos medios ###

## Intervalos de confianza basados en la desigualdad de Chebyshev ##

IC.Chebyshev.media.M = c(media.M - EM.M/sqrt(0.025),media.M + EM.M/sqrt(0.025))  # Intervalo de confianza al 97,5% del ingreso medio de empleados masculinos.
IC.Chebyshev.media.F = c(media.F - EM.F/sqrt(0.025),media.F + EM.F/sqrt(0.025))  # Intervalo de confianza al 97,5% del ingreso medio de empleadas femeninos.

IC.Chebyshev.media.M
IC.Chebyshev.media.F


## Intervalos de confianza basados en la aproximación a la normal ##

IC.normal.media.M = c(media.M - qnorm(0.9875)*EM.M, media.M + qnorm(0.9875)*EM.M)  # Intervalo de confianza al 97,5% del ingreso medio de empleados masculinos.
IC.normal.media.F = c(media.F - qnorm(0.9875)*EM.F, media.F + qnorm(0.9875)*EM.F)  # Intervalo de confianza al 97,5% del ingreso medio de empleadas femeninos.

IC.normal.media.M
IC.normal.media.F


### Estimación de los coeficientes de variación ###

media.pesada.M = sum(w.M*ing.M)/sum(w.M)                       # Media ponderada de los ingresos de empleados masculinos.
var.pesada.M = sum(w.M*(ing.M - media.pesada.M)^2)/sum(w.M)    # Varianza ponderada de los ingresos de empleados masculinos.
cv.M = sqrt(var.pesada.M)/media.pesada.M                       # Estimación del coeficiente de variación de los ingresos de empleados masculinos.

media.pesada.F = sum(w.F*ing.F)/sum(w.F)                       # Media ponderada de los ingresos de empleadas femeninas.
var.pesada.F = sum(w.F*(ing.F - media.pesada.F)^2)/sum(w.F)    # Varianza ponderada de los ingresos de empleados masculinos.
cv.F = sqrt(var.pesada.F)/media.pesada.F                       # Estimación del coeficiente de variación de los ingresos de empleadas femeninas.

c(cv.M, cv.F)

##############################################################################################################

### Histogramas y diagramas de caja ###

## Histogramas ##

intervalos.sturges <- function(x, cantidad = NULL) {           # Función que calcula los extremos de los intervalos dados por la regla de Sturges.
  if (is.null(cantidad) == TRUE) {
    cantidad = ceiling(1 + log(length(x))/log(2))
  }
  longitud = ceiling((max(x) - min(x))/cantidad)
  intervalos = seq(from = floor(min(x)), to = floor(min(x)) + longitud*cantidad,
                   by = longitud)
  return(intervalos)
}


if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

df.M = data.frame(ingresos = ing.M, pesos = w.M)                           # Dataframe con los ingresos y pesos masculinos.

ggplot(data = df.M, aes(x = ingresos, y=..density.., weight = pesos)) +    # Histograma normalizado de los ingresos de empleados masculinos.
  geom_histogram(fill = "#999999", color = "black",
                 breaks = intervalos.sturges(ing.M)) + 
  scale_x_continuous(limits = c(0, 45000), breaks = intervalos.sturges(ing.M)) +
  geom_density(color = "black", linewidth = 0.75) +
  theme_bw() +
  ggtitle("Histograma de ingresos anuales de los empleados masculinos") +
  xlab("Ingresos (euros)") + 
  ylab("Densidad")


df.F = data.frame(ingresos = ing.F, pesos = w.F)                           # Dataframe con los ingresos y pesos masculinos.

ggplot(data = df.F, aes(x = ingresos, y=..density.., weight = pesos)) +    # Histograma normalizado de los ingresos de empleados masculinos.
  geom_histogram(fill = "#999999", color = "black",
                 breaks = intervalos.sturges(ing.F)) +
  scale_x_continuous(limits = c(0, 40000), breaks = intervalos.sturges(ing.F)) +
  geom_density(color = "black", linewidth = 0.75) +
  theme_bw() +
  ggtitle("Histograma de ingresos anuales de las empleadas femeninas") +
  xlab("Ingresos (euros)") +
  ylab("Densidad")


## Diagramas de caja ##

df.M = data.frame(df.M, Sexo = rep("Hombres",nrow(df.M)))
df.F = data.frame(df.F, Sexo = rep("Mujeres",nrow(df.F)))
df = rbind(df.M, df.F)                                      # Dataframe con los ingresos de ambos sexos agrupados.

ggplot(data = df, aes(group = Sexo, y = ingresos, weight = pesos, fill = Sexo)) +      # Diagramas de caja de los ingresos de ambos sexos.
  stat_boxplot(geom = "errorbar") +  
  geom_boxplot() +
  scale_fill_manual(values = c("gray","gray")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "bottom") +
  ggtitle("Diagramas de caja de ingresos anuales de hombres y mujeres") +
  ylab("Ingresos (euros)")


##############################################################################################################

### Estimación de la tasa de individuos en riesgo de pobreza de cada sexo ###

##############################################################################################################


### Estimación de la proporción de individuos en riesgo de pobreza ###

z = 6700                              # Límite de riesgo de pobreza.
riesgo.M = ifelse(ing.M < z, 1, 0)    # Variable binaria de riesgo de pobreza masculino.
riesgo.F = ifelse(ing.F < z, 1, 0)    # Variable binaria de riesgo de pobreza femenino.

prop.M = as.numeric(horvitzThompson(riesgo.M, pi = pinc.M, N = N.M)[2])  # Estimación de la proporción de empleados masculinos en riesgo de pobreza.
prop.F = as.numeric(horvitzThompson(riesgo.F, pi = pinc.F, N = N.F)[2])  # Estimación de la proporción de empleadas femeninas en riesgo de pobreza.

# prop.M = sum(riesgo.M/(N.M*pinc.M))   
# prop.F = sum(riesgo.F/(N.F*pinc.F))   

c(prop.M, prop.F)

##############################################################################################################

### Estimación de los errores de muestreo ###

EM.prop.M = sqrt(as.numeric(horvitzThompson(riesgo.M, N = N.M, pi = pinc.M,        # Estimación del error muestral del estimador de la proporción de empleados masculinos en riesgo de pobreza.
                                            pi2 = Matriz.pinc.M, var_est = TRUE,
                                            var_method = "LinHT")[4]))
EM.prop.F = sqrt(as.numeric(horvitzThompson(riesgo.F, N = N.F, pi = pinc.F,        # Estimación del error muestral del estimador de la proporción de empleados masculinos en riesgo de pobreza.
                                            pi2 = Matriz.pinc.F, var_est = TRUE,
                                            var_method = "LinHT")[4]))

# EM.prop.M = sqrt(sum((1-pinc.M)/(pinc.M^2)*(riesgo.M/N.M)^2))  
# EM.prop.F = sqrt(sum((1-pinc.F)/(pinc.F^2)*(riesgo.F/N.F)^2))  

c(EM.prop.M, EM.prop.F)

##############################################################################################################

### Intervalos de confianza de la proporción de individuos en riesgo de pobreza ###

## Intervalos de confianza basados en la desigualdad de Chebyshev ##

IC.Chebyshev.prop.M = c(max(0, prop.M - EM.prop.M/sqrt(0.025)),    # Intervalo de confianza al 97,5% para la proporción de empleados masculinos en riesgo de pobreza.  
                        min(1, prop.M + EM.prop.M/sqrt(0.025)))             
IC.Chebyshev.prop.F = c(max(0, prop.F - EM.prop.F/sqrt(0.025)),    # Intervalo de confianza al 97,5% para la proporción de empleadas femeninas en riesgo de pobreza.
                        min(1, prop.F + EM.prop.F/sqrt(0.025)))            

IC.Chebyshev.prop.M
IC.Chebyshev.prop.F


## Intervalos de confianza basados en la aproximación a la normal ##

IC.normal.prop.M = c(max(0, prop.M - qnorm(0.9875)*EM.prop.M),     # Intervalo de confianza al 97,5% para la proporción de empleados masculinos en riesgo de pobreza.
                     min(1, prop.M + qnorm(0.9875)*EM.prop.M))   
IC.normal.prop.F = c(max(0, prop.F - qnorm(0.9875)*EM.prop.F),     # Intervalo de confianza al 97,5% para la proporción de empleadas femeninas en riesgo de pobreza.
                     min(1, prop.F + qnorm(0.9875)*EM.prop.F))   

IC.normal.prop.M
IC.normal.prop.F

##############################################################################################################

### Estimación de los coeficientes de variación ###

prop.pesada.M = sum(w.M*riesgo.M)/sum(w.M)                              # Media ponderada del riesgo de pobreza masculino.
var.riesgo.pesada.M = sum(w.M*(riesgo.M - prop.pesada.M)^2)/sum(w.M)    # Varianza ponderada del riesgo de pobreza masculino.
cv.riesgo.M = sqrt(var.riesgo.pesada.M)/prop.pesada.M                   # Estimación del coeficiente de variación del riesgo de pobreza masculino.

prop.pesada.F = sum(w.F*riesgo.F)/sum(w.F)                              # Media ponderada del riesgo de pobreza femenino..
var.riesgo.pesada.F = sum(w.F*(riesgo.F - prop.pesada.F)^2)/sum(w.F)    # Varianza ponderada del riesgo de pobreza femenino.
cv.riesgo.F = sqrt(var.riesgo.pesada.F)/prop.pesada.F                   # Estimación del coeficiente de variación del riesgo de pobreza femenino.

c(cv.riesgo.M, cv.riesgo.F)

##############################################################################################################

### Estimación de la proporción de individuos en riesgo de pobreza por provincias ###

datos.M = mutate(datos.M, riesgo = ifelse(ingnorm < z, 1, 0))   # Datos de empleados masculinos incluyendo la variable de riesgo de pobreza         
datos.F = mutate(datos.F, riesgo = ifelse(ingnorm < z, 1, 0))   # Datos de empleadas femeninas incluyendo la variable de riesgo de pobreza     

Albacete.M = filter(datos.M, provlab == "Albacete")             # Datos de empleados masculinos de Albacete
Albacete.F = filter(datos.F, provlab == "Albacete")             # Datos de empleadas femeninas de Albacete

CiudadReal.M = filter(datos.M, provlab == "CiudadReal")         # Datos de empleados masculinos de Ciudad Real
CiudadReal.F = filter(datos.F, provlab == "CiudadReal")         # Datos de empleadas femeninas de Ciudad Real

Cuenca.M = filter(datos.M, provlab == "Cuenca")                 # Datos de empleados masculinos de Cuenca
Cuenca.F = filter(datos.F, provlab == "Cuenca")                 # Datos de empleadas femeninas de Cuenca

Guadalajara.M = filter(datos.M, provlab == "Guadalajara")       # Datos de empleados masculinos de Guadalajara
Guadalajara.F = filter(datos.F, provlab == "Guadalajara")       # Datos de empleadas femeninas de Guadalajara

Toledo.M = filter(datos.M, provlab == "Toledo")                 # Datos de empleados masculinos de Toledo
Toledo.F = filter(datos.F, provlab == "Toledo")                 # Datos de empleadas femeninas de Toledo

N.Albacete.M = round(sum(Albacete.M[,11]))                      # Estimación de tamaño poblacional de empleados masculinos de Albacete
N.Albacete.F = round(sum(Albacete.F[,11]))                      # Estimación de tamaño poblacional de empleadas femeninas de Albacete

N.CiudadReal.M = round(sum(CiudadReal.M[,11]))                  # Estimación de tamaño poblacional de empleados masculinos de Ciudad Real
N.CiudadReal.F = round(sum(CiudadReal.F[,11]))                  # Estimación de tamaño poblacional de empleadas femeninas de Ciudad Real

N.Cuenca.M = round(sum(Cuenca.M[,11]))                          # Estimación de tamaño poblacional de empleados masculinos de Cuenca
N.Cuenca.F = round(sum(Cuenca.F[,11]))                          # Estimación de tamaño poblacional de empleadas femeninas de Cuenca

N.Guadalajara.M = round(sum(Guadalajara.M[,11]))                # Estimación de tamaño poblacional de empleados masculinos de Guadalajara
N.Guadalajara.F = round(sum(Guadalajara.F[,11]))                # Estimación de tamaño poblacional de empleadas femeninas de Guadalajara

N.Toledo.M = round(sum(Toledo.M[,11]))                          # Estimación de tamaño poblacional de empleados masculinos de Toledo
N.Toledo.F = round(sum(Toledo.F[,11]))                          # Estimación de tamaño poblacional de empleadas femeninas de Toledo


prop.Albacete.M = as.numeric(horvitzThompson(Albacete.M[,12], N = N.Albacete.M,         # Estimación de la proporción de empleados masculinos de Albacete en riesgo de pobreza.
                                             pi = 1/Albacete.M[,11])[2])
prop.Albacete.F = as.numeric(horvitzThompson(Albacete.F[,12],N = N.Albacete.F,          # Estimación de la proporción de empleadas femeninas de Albacete en riesgo de pobreza.
                                             pi = 1/Albacete.F[,11])[2])

prop.CiudadReal.M = as.numeric(horvitzThompson(CiudadReal.M[,12], N = N.CiudadReal.M,   # Estimación de la proporción de empleados masculinos de Ciudad Real en riesgo de pobreza.
                                               pi = 1/CiudadReal.M[,11])[2])
prop.CiudadReal.F = as.numeric(horvitzThompson(CiudadReal.F[,12], N = N.CiudadReal.F,   # Estimación de la proporción de empleadas femeninas de Ciudad Real en riesgo de pobreza.
                                               pi = 1/CiudadReal.F[,11])[2])

prop.Cuenca.M = as.numeric(horvitzThompson(Cuenca.M[,12], N = N.Cuenca.M,               # Estimación de la proporción de empleados masculinos de Cuenca en riesgo de pobreza.
                                           pi = 1/Cuenca.M[,11],)[2])
prop.Cuenca.F = as.numeric(horvitzThompson(Cuenca.F[,12], N = N.Cuenca.F,               # Estimación de la proporción de empleadas femeninas de Cuenca en riesgo de pobreza.
                                           pi = 1/Cuenca.F[,11])[2])

prop.Guadalajara.M = as.numeric(horvitzThompson(Guadalajara.M[,12],                     # Estimación de la proporción de empleados masculinos de Guadalajara en riesgo de pobreza.
                                                N = N.Guadalajara.M,
                                                pi = 1/Guadalajara.M[,11])[2])
prop.Guadalajara.F = as.numeric(horvitzThompson(Guadalajara.F[,12],                     # Estimación de la proporción de empleadas femeninas de Guadalajara en riesgo de pobreza.
                                                N = N.Guadalajara.F,
                                                pi = 1/Guadalajara.F[,11] )[2])

prop.Toledo.M = as.numeric(horvitzThompson(Toledo.M[,12], N = N.Toledo.M,               # Estimación de la proporción de empleados masculinos de Toledo en riesgo de pobreza.
                                           pi = 1/Toledo.M[,11])[2])
prop.Toledo.F = as.numeric(horvitzThompson(Toledo.F[,12], N = N.Toledo.F,               # Estimación de la proporción de empleadas femeninas de Toledo en riesgo de pobreza.
                                           pi = 1/Toledo.F[,11])[2])

# prop.Albacete.M = sum(Albacete.M[,12]/(N.Albacete.M*1/Albacete.M[,11]))
# prop.Albacete.F = sum(Albacete.F[,12]/(N.Albacete.F*1/Albacete.F[,11]))

# prop.CiudadReal.M = sum(CiudadReal.M[,12]/(N.CiudadReal.M*1/CiudadReal.M[,11]))
# prop.CiudadReal.F = sum(CiudadReal.F[,12]/(N.CiudadReal.F*1/CiudadReal.F[,11]))

# prop.Cuenca.M = sum(Cuenca.M[,12]/(N.Cuenca.M*1/Cuenca.M[,11]))
# prop.Cuenca.F = sum(Cuenca.F[,12]/(N.Cuenca.F*1/Cuenca.F[,11]))

# prop.Guadalajara.M = sum(Guadalajara.M[,12]/(N.Guadalajara.M*1/Guadalajara.M[,11]))
# prop.Guadalajara.F = sum(Guadalajara.F[,12]/(N.Guadalajara.F*1/Guadalajara.F[,11]))

# prop.Toledo.M = sum(Toledo.M[,12]/(N.Toledo.M*1/Toledo.M[,11]))
# prop.Toledo.F = sum(Toledo.F[,12]/(N.Toledo.F*1/Toledo.F[,11]))

c(prop.Albacete.M, prop.Albacete.F)
c(prop.CiudadReal.M, prop.CiudadReal.F)
c(prop.Cuenca.M, prop.Cuenca.F)
c(prop.Guadalajara.M, prop.Guadalajara.F)
c(prop.Toledo.M, prop.Toledo.F)


if (!require("sp")) install.packages("sp")
library(sp)
if (!require("colorspace")) install.packages("colorspace")
library(colorspace)

elmapa = readRDS("gadm36_ESP_2_sp.rds")
CM = elmapa[elmapa$NAME_1=="Castilla-La Mancha",]
CM$cantidadM = c(prop.Albacete.M, prop.CiudadReal.M, prop.Cuenca.M,
                 prop.Guadalajara.M, prop.Toledo.M)
CM$cantidadF = c(prop.Albacete.F, prop.CiudadReal.F, prop.Cuenca.F,
                 prop.Guadalajara.F, prop.Toledo.F)

textos = c("Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo")
coordenadas = coordinates(CM)
lista = list("sp.text", coordenadas, textos)

spplot(CM, "cantidadM", sp.layout = lista,                    # Gráfico de mapa de proporciones provinciales de empleados masculinos en riesgo de pobreza.
       col.regions = rev(sequential_hcl(1000, h = 250)))
spplot(CM, "cantidadF", sp.layout = lista,                    # Gráfico de mapa de proporciones provinciales de empleadas femeninas en riesgo de pobreza.
       col.regions = rev(sequential_hcl(n = 1000, h = 355)))

##############################################################################################################

### Estimación del total de individuos en riesgo de pobreza ###

tot.M = N.M*prop.M         # Estimación del número de empleados masculinos total en riesgo de pobreza.
tot.F = N.F*prop.F         # Estimación del número de empleadas femeninas total en riesgo de pobreza.

EM.tot.M = N.M*EM.prop.M   # Estimación del error muestral del estimador del número total de empleados masculinos en riesgo de pobreza.
EM.tot.F = N.F*EM.prop.F   # Estimación del error muestral del estimador del número total de empleadas femeninas en riesgo de pobreza.

cv.tot.M = EM.tot.M/tot.M  # Estimación del coeficiente de variación del estimador del número total de empleados masculinos en riesgo de pobreza.
cv.tot.F = EM.tot.F/tot.F  # Estimación del coeficiente de variación del estimador del número total de empleadas femeninas en riesgo de pobreza.

c(tot.M, tot.F)
c(EM.tot.M, EM.tot.F)
c(cv.tot.M, cv.tot.F)

##############################################################################################################
