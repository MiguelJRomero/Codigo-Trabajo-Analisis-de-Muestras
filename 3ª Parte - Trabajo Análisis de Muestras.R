#### Código Trabajo Análisis de Muestras - 3ª Parte ####

##############################################################################################################

### Generación de datos ###

if (!require(sae)) install.packages("sae")
library("sae")
data(incomedata)

set.seed(292)
NIU = 100452943
plus = round(runif(1,0,500))
set.seed(NIU+plus)
cual = sample(1:17,1)       # Devuelve 1: Andalucía

datosECV = incomedata
datosECVmas16 = subset(datosECV, (datosECV$labor>0))
datosECVmas16$age = datosECVmas16$age - 1
nrows = dim(datosECVmas16)[[1]]
datosECVmas16$horas = round(rnorm(nrows,34,3), 1)
datosECVmas16$horas[(datosECVmas16$labor==2) | (datosECVmas16$labor == 3)] = 0
datosECVmas16$income = round(jitter(datosECVmas16$income),1)
datosECVmas16$income[datosECVmas16$educ==1] = 
  rnorm(length(datosECVmas16$income[datosECVmas16$educ==1]),
        datosECVmas16$income[datosECVmas16$educ==1]*0.8, 200)
datosECVmas16$income[datosECVmas16$educ==3] = 
  rnorm(length(datosECVmas16$income[datosECVmas16$educ==3]),
        datosECVmas16$income[datosECVmas16$educ==3]*1.2, 100)
datosECVmas16$income[datosECVmas16$labor==2] = 
  datosECVmas16$income[datosECVmas16$labor==2]*0.7
datosECVmas16$income[datosECVmas16$labor==3] = 0
datosFinal = 
  data.frame(ca=datosECVmas16$ac, prov=datosECVmas16$prov, 
             provlab=datosECVmas16$provlab, gen=datosECVmas16$gen, 
             edad=datosECVmas16$age, nac=datosECVmas16$nat, 
             neduc=datosECVmas16$educ, sitemp=datosECVmas16$labor, 
             ingnorm=datosECVmas16$income, horas=datosECVmas16$horas)

datos_Andalucia = datosFinal[datosFinal[,1]==1,]
write.table(datos_Andalucia,"datos_Andalucia.txt",row.names=FALSE)

datos = datos_Andalucia
datos = datos[which(datos$sitemp!=3),]
datos[which(datos$sitemp==1),8] = 0
datos[which(datos$sitemp==2),8] = 1


##############################################################################################################

### Estimación de los ingresos medios y tasa de paro por estimadores de muestreo simple aleatorio ###

##############################################################################################################


### Generación de la muestra ###

N.act = nrow(datos)                                                                     # Tamaño de la población activa
n.act = ceiling(N.act/10)                                                               # Tamaño de la muestra de población activa

mas.act = datos[sample(1:N.act, n.act, replace = FALSE),]                               # Muestra de población activa
mas.emp = mas.act[which(mas.act$sitemp == 0),]                                          # Submuestra de empleados

N.emp = length(which(datos$sitemp == 0))                                                # Número total de empleados
n.emp = nrow(mas.emp)                                                                   # Tamaño de la submuestra de empleados


##############################################################################################################


### Estimación de los ingresos medios ###

if (!require("survey")) install.packages("survey")                                      
library("survey")

mas.emp$tamaño = N.emp                                                                   
diseño.emp.mas = svydesign(id = ~1, fpc = ~tamaño, data = mas.emp)                      # Diseño muestral del muestreo aleatorio simple de empleados

svymean(~ingnorm, diseño.emp.mas, na.rm = TRUE)                                         # Estimación de los ingresos medios de empleados y de su error estándar
confint(svymean(~ingnorm, diseño.emp.mas, na.rm = TRUE))                                # Intervalo de confianza de los ingresos medios de empleados


##############################################################################################################


### Estimación de la tasa de paro ###

mas.act$tamaño = N.act                                               
diseño.act.mas = svydesign(id = ~1, fpc = ~tamaño, data = mas.act)                      # Diseño muestral del muestreo aleatorio simple de población activa

svymean(~sitemp, diseño.act.mas, na.rm = TRUE)                                          # Estimación de la tasa de paro y de su error estándar
confint(svymean(~sitemp, diseño.act.mas, na.rm = TRUE))                                 # Intervalo de confianza de la tasa de paro


##############################################################################################################

### Estimación de los ingresos medios y tasa de paro por estimadores de razón y regresión ###

##############################################################################################################


### Estimación de los ingresos medios a través del estimador de razón ###

media.horas.trabajo.emp = mean(datos[which(datos$sitemp == 0),10])                      # Horas de trabajo promedio de los empleados
predict(svyratio(~ingnorm, ~horas, design = diseño.emp.mas),                            # Estimación de los ingresos medios de empleados
        total = media.horas.trabajo.emp, se = FALSE)

R.ingresos = mean(mas.emp$ingnorm)/mean(mas.emp$horas)                                  # Estimación de la razón entre ingresos medios y horas de trabajo promedio de empleados
ing.medios.razon = R.ingresos*media.horas.trabajo.emp                                   # Estimación de los ingresos medios de empleados
var.ing.medios.razon = (1/n.emp - 1/N.emp)*                                             # Estimación de la arianza del estimador de razón de los ingresos medios de empleados
                       sum((mas.emp$ingnorm - R.ingresos*mas.emp$horas)^2)/(n.emp-1)

sqrt(var.ing.medios.razon)                                                              # Estimación del error estándar del estimador de razón de los ingresos medios de empleados

centro = ing.medios.razon
radio = qnorm(0.025, lower.tail = FALSE)*sqrt(var.ing.medios.razon)
IC.ing.medios.razon = c(centro - radio, centro + radio)

IC.ing.medios.razon                                                                     # Intervalo de confianza de los ingresos medios de empleados


##############################################################################################################


### Estimación de la tasa de paro a través del estimador de razón ###

media.horas.trabajo.act = mean(datos$horas)                                             # Horas de trabajo promedio de la poblacióna activa
predict(svyratio(~sitemp, ~horas, design = diseño.act.mas),                             # Estimación de la tasa de paro
        total = media.horas.trabajo.act, se = FALSE)

R.tasa.paro = mean(mas.act$sitemp)/mean(mas.act$horas)                                  # Estimación de la razón entre tasa de paro y horas de trabajo promedio de la población activa
tasa.paro.razon = R.tasa.paro*media.horas.trabajo.act                                   # Estimación de la tasa de paro
var.tasa.paro.razon = (1/n.act - 1/N.act)*                                              # Estimación de la varianza del estimador de razón de la tasa de paro
                      sum((mas.act$sitemp - R.tasa.paro*mas.act$horas)^2)/(n.act-1)

sqrt(var.tasa.paro.razon)                                                               # Estimación del error estándar del estimador de razón de la tasa de paro  

centro = tasa.paro.razon
radio = qnorm(0.025, lower.tail = FALSE)*sqrt(var.tasa.paro.razon)
IC.tasa.paro.razon = c(max(0, centro - radio), min(1, centro + radio))

IC.tasa.paro.razon                                                                      # Intervalo de confianza de la tasa de paro


##############################################################################################################


### Estimación de los ingresos medios a través del estimador de regresión ###

diseño.emp.regresion = calibrate(diseño.emp.mas, formula = ~horas,                      # Diseño muestral para el estimador de regresión
                                 population = c("(Intercept)" = N.emp,
                                 horas = media.horas.trabajo.emp*N.emp))
svymean(~ingnorm, diseño.emp.regresion)[1]                                              # Estimación de los ingresos medios de empleados

b.ingresos = cov(mas.emp$ingnorm, mas.emp$horas)/var(mas.emp$horas)                     # Estimación de la pendiente de regresión entre ingresos medios y horas trabajadas de los empleados
ing.medios.regresion = mean(mas.emp$ingnorm) +                                          # Estimación de los ingresos medios de empleados
                       b.ingresos*(media.horas.trabajo.emp - mean(mas.emp$horas))
var.ing.medios.regresion = (1/n.emp - 1/N.emp)*                                         # Estimación de la varianza del estimador de regresión de los ingresos medios de empleados
                           (sum((mas.emp$ingnorm - mean(mas.emp$ingnorm))^2)/
                           (n.emp-2) - cov(mas.emp$ingnorm,mas.emp$horas)^2/
                           var(mas.emp$horas))

sqrt(var.ing.medios.regresion)                                                          # Estimación del error estándar del estimador de regresión de los ingresos medios de empleados

centro = ing.medios.regresion
radio = qnorm(0.025, lower.tail = FALSE)*sqrt(var.ing.medios.regresion)
IC.ing.medios.regresion = c(centro - radio, centro + radio)

IC.ing.medios.regresion                                                                 # Intervalo de confianza de los ingresos medios de empleados


##############################################################################################################


### Estimación de la tasa de paro a través del estimador de regresión ###

diseño.act.regresion = calibrate(diseño.act.mas, formula = ~horas,                      # Diseño muestral para el estimador de regresión
                                 population = c("(Intercept)" = N.act,
                                 horas = media.horas.trabajo.act*N.act))
svymean(~sitemp, diseño.act.regresion)[1]                                               # Estimación de la tasa de paro

b.tasa.paro = cov(mas.act$sitemp, mas.act$horas)/var(mas.act$horas)                     # Estimación de la pendiente de regresión entre situación de empleo y horas trabajadas de la población activa
tasa.paro.regresion = mean(mas.act$sitemp) +                                            # Estimación de la tasa de paro
                      b.tasa.paro*(media.horas.trabajo.act - mean(mas.act$horas))       # Estimación de la varianza del estimador de regresión de la tasa de paro
var.tasa.paro.regresion = (1/n.act-1/N.act)*
                          (sum((mas.act$sitemp-mean(mas.act$sitemp))^2)/
                          (n.act-2)-cov(mas.act$sitemp,mas.act$horas)^2/
                          var(mas.act$horas))

sqrt(var.tasa.paro.regresion)                                                           # Estimación del error estándar del estimador de regresión de la tasa de paro

centro = tasa.paro.regresion
radio = qnorm(0.025, lower.tail = FALSE)*sqrt(var.tasa.paro.regresion)
IC.tasa.paro.regresion = c(centro - radio, centro + radio)

IC.tasa.paro.regresion                                                                  # Intervalo de confianza de la tasa de paro


##############################################################################################################

### Estimación de los ingresos medios y tasa de paro por estimadores de muestreo estratificado ###

##############################################################################################################

### Definición de tamaños poblacionales, muestrales y pesos de los empleados de cada estrato ###

mas.emp.1 = mas.emp[which(mas.emp$neduc == 1),]                                         # Submuestra de empleados con nivel educativo de primaria
mas.emp.2 = mas.emp[which(mas.emp$neduc == 2),]                                         # Submuestra de empleados con nivel educativo de secunaria                                      
mas.emp.3 = mas.emp[which(mas.emp$neduc == 3),]                                         # Submuestra de empleados con nivel educativo de postsecundaria

N1.emp = length(which(datos$neduc == 1 & datos$sitemp == 0))                            # Número total de empleados con nivel educativo de primaria
N2.emp = length(which(datos$neduc == 2 & datos$sitemp == 0))                            # Número total de empleados con nivel educativo de secundaria
N3.emp = length(which(datos$neduc == 3 & datos$sitemp == 0))                            # Número total de empleados con nivel educativo de postsecundaria

W1.emp = N1.emp/N.emp                                                                   # Peso del estrato de empleados con nivel educativo de primaria
W2.emp = N2.emp/N.emp                                                                   # Peso del estrato de empleados con nivel educativo de secundaria
W3.emp = N3.emp/N.emp                                                                   # Peso del estrato de empleados con nivel educativo de postsecundaria

n1.emp = nrow(mas.emp.1)                                                                # Tamaño de la submuestra de empleados con nivel educativo de primaria
n2.emp = nrow(mas.emp.2)                                                                # Tamaño de la submuestra de empleados con nivel educativo de secundaria
n3.emp = nrow(mas.emp.3)                                                                # Tamaño de la submuestra de empleados con nivel educativo de postsecundaria


##############################################################################################################


### Estimación de los ingresos medios a través del estimador de muestreo estratificado ###

if (!require("sampling")) install.packages("sampling")
library("sampling")

datos = datos[order(datos$neduc),]                                                      # Generación de una muestra de empleados estratificada por niveles educativos
mae.emp.estratos = strata(datos[which(datos$sitemp == 0),], "neduc",
                          size = c(n1.emp,n2.emp,n3.emp), method = "srswor")
datos$neduc = as.factor(datos$neduc)
muestra.emp.estratos = getdata(datos, mae.emp.estratos)
muestra.emp.estratos$tamaño = with(muestra.emp.estratos,
                                   ifelse(neduc == "1", N1.emp,
                                          ifelse(neduc == "2", N2.emp, N3.emp)))
muestra.emp.estratos$pesos = 1/muestra.emp.estratos$Prob

mas.emp = mas.emp[order(mas.emp$neduc),]
muestra.emp.estratos[,1:10] = mas.emp[,c(1:6,8:10,7)]                                   # Sustitución de datos de empleados por los de la submuestra original

diseño.emp.estratos = svydesign(id = ~1, weights = ~pesos, fpc = ~tamaño,               # Diseño muestral del muestreo aleatorio estratificado de empleados
                                strat = ~neduc, data = muestra.emp.estratos)
svymean(~ingnorm, diseño.emp.estratos, na.rm = TRUE)                                    # Estimación de los ingresos medios globales de empleados y de su error estándar
confint(svymean(~ingnorm, diseño.emp.estratos, na.rm = TRUE))                           # Intervalo de confianza de los ingresos medios de empleados

ing.medios.estratos = svyby(~ingnorm, ~neduc, diseño.emp.estratos, svymean)             # Estimación de los ingresos medios de empleados de cada estrato
ing.medios.estratos

c(ing.medios.estratos[1,2]-ing.medios.estratos[1,3]/sqrt(0.05/3),                       # Intervalo de confianza por Chebyshev de los ingresos medios de empleados con nivel educativo de primaria
  ing.medios.estratos[1,2]+ing.medios.estratos[1,3]/sqrt(0.05/3))
confint(ing.medios.estratos, level = 1-0.05/3)[2,]                                      # Intervalo de confianza por aproximación a la normal de los ingresos medios de empleados con nivel educativo de secundaria
c(max(0,ing.medios.estratos[3,2]-ing.medios.estratos[3,3]/sqrt(0.05/3)),                # Intervalo de confianza por Chebyshev de los ingresos medios de empleados con nivel educativo de postsecundaria
  ing.medios.estratos[3,2]+ing.medios.estratos[3,3]/sqrt(0.05/3))

# confint(ing.medios.estratos, level = 1-0.05/3)                                        # Intervalos de confianza por aproximación a la normal de los ingresos medios de empleados de cada estrato


##############################################################################################################


### Definición de tamaños poblacionales, muestrales y pesos de los miembros de la población activa de cada estrato ###

mas.act.1 = mas.act[which(mas.act$neduc == 1),]                                         # Submuestra de miembros de la poblacióna activa con nivel educativo de primaria
mas.act.2 = mas.act[which(mas.act$neduc == 2),]                                         # Submuestra de miembros de la poblacióna activa con nivel educativo de secundaria
mas.act.3 = mas.act[which(mas.act$neduc == 3),]                                         # Submuestra de miembros de la poblacióna activa con nivel educativo de postsecundaria

N1.act = length(which(datos$neduc == 1))                                                # Número total de miembros de la poblacióna activa con nivel educativo de primaria
N2.act = length(which(datos$neduc == 2))                                                # Número total de miembros de la poblacióna activa con nivel educativo de secundaria
N3.act = length(which(datos$neduc == 3))                                                # Número total de miembros de la poblacióna activa con nivel educativo de postsecundaria

W1.act = N1.act/N.act                                                                   # Peso del estrato de miembros de la población activa con nivel educativo de primaria
W2.act = N2.act/N.act                                                                   # Peso del estrato de miembros de la población activa con nivel educativo de secundaria
W3.act = N3.act/N.act                                                                   # Peso del estrato de miembros de la población activa con nivel educativo de postsecundaria

n1.act = nrow(mas.act.1)                                                                # Tamaño de la submuestra de miembros de la poblacióna activa con nivel educativo de primaria
n2.act = nrow(mas.act.2)                                                                # Tamaño de la submuestra de miembros de la poblacióna activa con nivel educativo de secundaria
n3.act = nrow(mas.act.3)                                                                # Tamaño de la submuestra de miembros de la poblacióna activa con nivel educativo de postsecundaria


##############################################################################################################


### Estimación de la tasa de paro a través del estimador de muestreo estratificado ###

mae.act.estratos = strata(datos, "neduc",                    # Generación de una muestra de miembros de la población activa estratificada por niveles educativos
                          size = c(n1.act,n2.act,n3.act), method = "srswor")
datos$neduc = as.factor(datos$neduc)
muestra.act.estratos = getdata(datos, mae.act.estratos)
muestra.act.estratos$tamaño = with(muestra.act.estratos,
                                   ifelse(neduc == "1", N1.act,
                                          ifelse(neduc == "2", N2.act, N3.act)))
muestra.act.estratos$pesos = 1/muestra.act.estratos$Prob

mas.act = mas.act[order(mas.act$neduc),]
muestra.act.estratos[,1:10] = mas.act[,c(1:6,8:10,7)]                                   # Sustitución de datos por los de la muestra original  

diseño.act.estratos = svydesign(id = ~1, weights = ~pesos, fpc = ~tamaño,               # Diseño muestral del muestreo aleatorio estratificado de miembros de la poblacióna ctiva
                                strat = ~neduc, data = muestra.act.estratos)
svymean(~sitemp, diseño.act.estratos, na.rm = TRUE)                                     # Estimación de la tasa de paro global y de su error estándar
confint(svymean(~sitemp, diseño.act.estratos, na.rm = TRUE))                            # Intervalo de confianza de la tasa de paro global

tasa.paro.estratos = svyby(~sitemp, ~neduc, diseño.act.estratos, svymean)               # Estimación de la tasa de paro de cada estrato
tasa.paro.estratos

confint(tasa.paro.estratos, level = 1-0.05/3)[1:2,]                                     # Intervalos de confianza por aproximación a la normal de la tasa de paro de miembros de la población activa con niveles educativos de primaria y secundaria
c(max(0,tasa.paro.estratos[3,2]-tasa.paro.estratos[3,3]/sqrt(0.05/3)),                  # Intervalo de confianza por Chebyshev de la tasa de paro de miembros de la población activa con nivel educativo de postsecundaria
  min(1,tasa.paro.estratos[3,2]+tasa.paro.estratos[3,3]/sqrt(0.05/3)))

# confint(tasa.paro.estratos, level = 0.95)                                             # Intervalos de confianza por aproximacióna a la normal de la tasa de paro de cada estrato con nivel de confianza no corregido


##############################################################################################################


### Histogramas y gráficos de caja de ingresos y situación laboral ###

## Histogramas de ingresos de los empleados ##

intervalos.sturges <- function(x, cantidad = NULL) {                                    # Función que calcula los extremos de los intervalos dados por la regla de Sturges.
  if (is.null(cantidad) == TRUE) {
    cantidad = ceiling(1 + log(length(x))/log(2))
  }
  longitud = ceiling((max(x) - min(x))/cantidad)
  intervalos = seq(from = floor(min(x)), to = floor(min(x)) + longitud*cantidad,
                   by = longitud)
  return(intervalos)
}

muestra.emp.1 = muestra.emp.estratos[which(muestra.emp.estratos$neduc == 1),]           # Submuestra de empleados con nivel educativo de primaria con pesos incorporados
muestra.emp.2 = muestra.emp.estratos[which(muestra.emp.estratos$neduc == 2),]           # Submuestra de empleados con nivel educativo de secundaria con pesos incorporados
muestra.emp.3 = muestra.emp.estratos[which(muestra.emp.estratos$neduc == 3),]           # Submuestra de empleados con nivel educativo de postsecundaria con pesos incorporados

library(ggplot2)

histograma <- function (df, variable.x, limites, titulo, xlabel, ylabel) {              # Función para construir los histogramas de ingresos de los empleados
  ggplot(data = df, aes(x = variable.x, y=..density.., weight = pesos)) +    
    geom_histogram(fill = "#BEBEBE", color = "black",
                   breaks = intervalos.sturges(variable.x)) + 
    scale_x_continuous(limits = limites, breaks = intervalos.sturges(variable.x)) +
    geom_density(color = "black", lwd = 0.75)  +
    ggtitle(titulo) +
    xlab(xlabel) + 
    ylab(ylabel) +
    theme(panel.border = element_rect(fill = "transparent", color = 1, size = 2),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.background = element_rect(fill = "white"),
          plot.title = element_text(family = "mono", size = 15, hjust = 0.5),
          axis.title.x = element_text(family = "mono", face = "bold"),
          axis.title.y = element_text(family = "mono", face = "bold"))
}

histograma(muestra.emp.estratos, muestra.emp.estratos$ingnorm, c(0,45000),              # Histograma de ingresos de los empleados
           "Histograma de ingresos de los empleados", "Ingresos (euros)",
           "Densidad")

histograma(muestra.emp.1, muestra.emp.1$ingnorm, c(0,25000),                            # Histograma de ingresos de los empleados con nivel educativo de primaria
           "Histograma de ingresos de los empleados con educación primaria",
           "Ingresos (euros)", "Densidad")

histograma(muestra.emp.2, muestra.emp.2$ingnorm, c(0,40000),                            # Histograma de ingresos de los empleados con nivel educativo de secundaria
           "Histograma de ingresos de los empleados con educación secundaria",
           "Ingresos (euros)", "Densidad")

histograma(muestra.emp.3, muestra.emp.3$ingnorm, c(0,50000),                            # Histograma de ingresos de los empleados con nivel educativo de postsecundaria
           "Histograma de ingresos de los empleados con educación postsecundaria",
           "Ingresos (euros)", "Densidad")

# svyhist(~ingnorm, diseño.emp.estratos, breaks = intervalos.sturges(mas.emp$ingnorm),    # Histograma de ingresos de los empleados
#         main = "Histograma de ingresos de empleados",)  


## Diagramas de caja de ingresos de los empleados ##

datos.diagrama.caja.emp = rbind(muestra.emp.estratos,muestra.emp.estratos)              # Agrupación de los datos por grupos: global y de cada estrato
datos.diagrama.caja.emp[1:78,10] = 0
datos.diagrama.caja.emp$neduc = factor(datos.diagrama.caja.emp$neduc,
                                       labels = c("Total", "Primaria",
                                                  "Secundaria", "Postsecundaria"))

ggplot(data = datos.diagrama.caja.emp, aes(y = ingnorm, group = neduc,                  # Diagramas de caja de los ingresos de los empleados
                                           weight = pesos, fill = neduc)) +      
  stat_boxplot(geom = "errorbar") +  
  geom_boxplot() +
  scale_fill_manual(values = c("#FFC425", "#F8766D", "#00BA38", "#619CFF")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Diagramas de caja de ingresos de empleados por nivel educativo") +
  ylab("Ingresos (euros)") +
  theme(panel.border = element_rect(fill = "transparent", color = "#030303",
                                    size = 2),
        plot.background = element_rect(fill = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF"),
        plot.title=element_text(family = "mono", size = 15, hjust = 0.5),
        axis.title.y = element_text(family = "mono", face = "bold"),
        legend.position = "right",
        legend.background = element_rect(fill = "lavenderblush", colour = 1))

## Diagramas de barras apiladas ##

frec.abs = c(n.act - svymean(~sitemp, diseño.act.estratos, na.rm = TRUE)[1]*n.act,      # Frecuencias absolutas de empleados y desempleados teniendo en cuenta los pesos de cada estrato
             svymean(~sitemp, diseño.act.estratos, na.rm = TRUE)[1]*n.act,
            length(which(mas.act.1$sitemp == 0)),
            length(which(mas.act.1$sitemp == 1)),
            length(which(mas.act.2$sitemp == 0)),
            length(which(mas.act.2$sitemp == 1)),
            length(which(mas.act.3$sitemp == 0)),
            length(which(mas.act.3$sitemp == 1)))

datos.diagrama.barra.abs = data.frame(neduc=c("Total","Total",                          # Tabla de totales de empleados y desempleados por estratos teniendo en cuenta los pesos de cada estrato  
                                              "Primaria","Primaria",
                                              "Secundaria","Secundaria",
                                              "Postsecundaria","Postsecundaria"),
                                      sitemp = rep(c("Empleado","Desempleado"),4),
                                      totales = frec.abs,
                                      posicion = c(4,4,1,1,2,2,3,3))

Nivel.educativo.abs = reorder(datos.diagrama.barra.abs$neduc,                           # Factor de niveles educativos
                              datos.diagrama.barra.abs$posicion)

ggplot(datos.diagrama.barra.abs, aes(fill=sitemp, y=totales,                            # Diagrama de barras apiladas de frecuencias absolutas de empleados y desempleados
                                     x=Nivel.educativo.abs)) + 
  geom_bar(sitemp="stack", stat="identity") + 
  geom_text(y = -2, aes(label = neduc)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Diagrama de barra de frecuencias absolutas de empleados y desempleados") +
  ylab("Número de personas") +
  theme(panel.border = element_rect(fill = "transparent", color = "#030303",
                                    size = 2),
        plot.background = element_rect(fill = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF"),
        plot.title=element_text(family = "mono", size = 15, hjust = 0.5),
        axis.title.y = element_text(family = "mono", face = "bold"),
        legend.position = "right",
        legend.background = element_rect(fill = "lavenderblush", colour = 1))


frec.rel = c(1 - svymean(~sitemp, diseño.act.estratos, na.rm = TRUE)[1],                # Frecuencias relativas de empleados y desempleados teniendo en cuenta los pesos de cada estrato
             svymean(~sitemp, diseño.act.estratos, na.rm = TRUE)[1],
            length(which(mas.act.1$sitemp == 0))/n1.act,
            length(which(mas.act.1$sitemp == 1))/n1.act,
            length(which(mas.act.2$sitemp == 0))/n2.act,
            length(which(mas.act.2$sitemp == 1))/n2.act,
            length(which(mas.act.3$sitemp == 0))/n3.act,
            length(which(mas.act.3$sitemp == 1))/n3.act)

datos.diagrama.barra.rel = data.frame(neduc=c("Total","Total",                          # Tabla de proporciones de empleados y desempleados por estratos teniendo en cuenta los pesos de cada estrato   
                                               "Primaria","Primaria",
                                               "Secundaria","Secundaria",
                                               "Postsecundaria","Postsecundaria"),
                                      sitemp = rep(c("Empleado","Desempleado"),4), 
                                      totales = frec.rel,
                                      posicion = c(4,4,1,1,2,2,3,3))

Nivel.educativo.rel = reorder(datos.diagrama.barra.rel$neduc,                           # Factor de niveles educativos
                              datos.diagrama.barra.rel$posicion)

ggplot(datos.diagrama.barra.prop, aes(fill=sitemp, y=totales,                           # Diagrama de barras apiladas de frecuencias relativas de empleados y desempleados
                                      x=Nivel.educativo.rel)) + 
  geom_bar(sitemp="stack", stat="identity") +
  geom_text(y = -0.015, aes(label = neduc)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Diagrama de barra de frecuencias relativas de empleados y desempleados") +
  ylab("Número de personas") +
  theme(panel.border = element_rect(fill = "transparent", color = "#030303",
                                    size = 2),
        plot.background = element_rect(fill = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF"),
        plot.title=element_text(family = "mono", size = 15, hjust = 0.5),
        axis.title.y = element_text(family = "mono", face = "bold"),
        legend.position = "right",
        legend.background = element_rect(fill = "lavenderblush", colour = 1))


##############################################################################################################


### Cálculo de afijación de Neyman para estimación de los ingresos medios ###

vars.emp = c(var(mas.emp.1$ingnorm), var(mas.emp.2$ingnorm), var(mas.emp.3$ingnorm))    # Vector de varianzas estimadas de ingresos de cada estrato
pesos.emp = c(W1.emp, W2.emp, W3.emp)                                                   # Vector de pesos de cada estrato

1/n.emp*sum((1-sum(pesos.emp*sqrt(vars.emp))/sqrt(vars.emp))*pesos.emp*vars.emp)        # Estimación de diferencia de varianzas de afijación proporcional y de Neyman (con varianzas estimadas) en la estimación de ingresos medios


suma.sd.emp = sum(pesos.emp*sqrt(vars.emp))                                             
n1.ingresos.neyman = round(n.emp*W1.emp*sqrt(var(mas.emp.1$ingnorm))/suma.sd.emp)       # Estimación del tamaño muestral óptimo en la estimación de ingresos medios para el nivel educativo de primaria
n2.ingresos.neyman = round(n.emp*W2.emp*sqrt(var(mas.emp.2$ingnorm))/suma.sd.emp)       # Estimación del tamaño muestral óptimo en la estimación de ingresos medios para el nivel educativo de secundaria
n3.ingresos.neyman = round(n.emp*W3.emp*sqrt(var(mas.emp.3$ingnorm))/suma.sd.emp)       # Estimación del tamaño muestral óptimo en la estimación de ingresos medios para el nivel educativo de postsecundaria

c(n1.ingresos.neyman,n2.ingresos.neyman,n3.ingresos.neyman)                             # Afijación de Neyman con varianzas estimadas para la estimación de los ingresos medios de empleados

# afijacion.proporcional.ingresos = round(n.emp*pesos.emp)                              # Afijación proporcional para la estimación de los ingresos medios de empleados


##############################################################################################################


### Cálculo de afijación de Neyman para estimación de la tasa de paro ###

vars.act = c(var(mas.act.1$sitemp), var(mas.act.2$sitemp), var(mas.act.3$sitemp))       # Vector de varianzas estimadas de situación de empleo de cada estrato
pesos.act = c(W1.act, W2.act, W3.act)                                                   # Vector de pesos de cada estrato

1/n.act*sum((1-sum(pesos.act*sqrt(vars.act))/sqrt(vars.act))*pesos.act*vars.act)        # Estimación de diferencia de varianzas de afijación proporcional y de Neyman (con varianzas estimadas) en la estimación de tasa de paro


suma.sd.act = sum(pesos.act*sqrt(vars.act))                             
n1.tasa.paro.neyman = round(n.act*W1.act*sqrt(var(mas.act.1$sitemp))/suma.sd.act)       # Estimación del tamaño muestral óptimo en la estimación de la tasa de paro para el nivel educativo de primaria   
n2.tasa.paro.neyman = round(n.act*W2.act*sqrt(var(mas.act.2$sitemp))/suma.sd.act)       # Estimación del tamaño muestral óptimo en la estimación de la tasa de paro para el nivel educativo de secundaria   
n3.tasa.paro.neyman = round(n.act*W3.act*sqrt(var(mas.act.3$sitemp))/suma.sd.act)       # Estimación del tamaño muestral óptimo en la estimación de la tasa de paro para el nivel educativo de postsecundaria

c(n1.tasa.paro.neyman,n2.tasa.paro.neyman,n3.tasa.paro.neyman)                          # Afijación de Neyman con varianzas estimadas para la estimación de la tasa de paro

# afijacion.proporcional.tasa.paro = round(n.act*pesos.act)                             # Afijación proporcional para la estimación de la tasa de paro

