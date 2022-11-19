# Ejercicio Práctico N°13
# Grupo N°2
# Integrantes:
#   Ivan Zuñiga Quiroz
#   Bastian Soto Jaña
#   Gabriela Fernández Rodríguez
#   Nicolás Gabrielli

# Librerias importadas.

library(ez)
library(ggpubr)
library(tidyr)
library(dplyr)
library(boot)
library(simpleboot)
library(bootES)
library(WRS2)
library(stringr)
library(leaps)
library(car)
library(scatterplot3d)

#-------------------------- Enunciado:  ----------------------------------------
# Un estudio recolectó medidas anatómicas de 247 hombres y 260 mujeres 
# (Heinz et al., 2003). Estas mediciones están disponibles en el archivo 
# Body-EP12.csv que acompaña a este enunciado. El estudio incluyó nueve
# mediciones del esqueleto (ocho diámetros y una profundidad de hueso a hueso) y
# doce mediciones de grosor (circunferencias) que incluyen el tejido. 

# La siguiente tabla detalla las variables registradas en este estudio: 

# Columna                  Descripción                                          Unidad
# Biacromial.diameter      Diámetro biacromial (a la altura de los hombros)       cm
# Biiliac.diameter         Diámetro biiliaco (a la altura de la pelvis)           cm
# Bitrochanteric.diameter  Diámetro bitrocantéreo (a la altura de las caderas)    cm
# Chest.depth              Profundidad del pecho (entre la espina y el esternón   cm
                          # a la altura de los pezones) 
# Chest.diameter           Diámetro del pecho (a la altura de los pezones)        cm
# Elbows.diameter          Suma de los diámetros de los codos                     cm
# Wrists.diameter          Suma de los diámetros de las muñecas                   cm
# Knees.diameter           Suma de los diámetros de las rodillas                  cm
# Ankles.diameter          Suma de los diámetros de los tobillos                  cm
# Shoulder.Girth           Grosor de los hombros sobre los músculos deltoides     cm
# Chest.Girth              Grosor del pecho, sobre tejido mamario en mujeres y    cm
                          # a la altura de los pezones en varones 
# Waist.Girth              Grosor a la altura de la cintura                       cm
# Navel.Girth              Grosor a la altura del ombligo                         cm
# Hip.Girth                Grosor a la altura de las caderas                      cm
# Thigh.Girth              Grosor promedio de ambos muslos bajo el pliegue del    cm
                          # glúteo
# Bicep.Girth              Grosor promedio de ambos bíceps, brazos flectados      cm
# Forearm.Girth            Grosor promedio de ambos antebrazos, brazos            cm
                          # extendidos palmas hacia arriba 
# Knee.Girth               Grosor promedio de ambas rodillas, posición levemente  cm
                          # flectada, medición arriba de la rótula 
# Calf.Maximum.Girth       Grosor promedio de la parte más ancha de ambas         cm
                          # pantorrillas
# Ankle.Minimum.Girth      Grosor promedio de la parte más delgada de ambos       cm
                          # tobillos
# Wrist.Minimum.Girth      Grosor promedio de la parte más delgada de ambas       cm
                          # muñecas 
# Age                      Edad                                                   Años
# Weight                   Peso                                                   Kg
# Height                   Estatura                                               cm
# Gender                   Género                                                 1: hombre 0: mujer


# Pregunta

# Se pide construir un modelo de regresión lineal simple y otro de regresión 
# lineal múltiple para predecir la variable Peso, de acuerdo con las siguientes 
# instrucciones:

datos <- read.csv2(file.choose(), 
                   encoding = "latin1", 
                   stringsAsFactors = TRUE, 
                   check.names = F)

  # 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro 
  #    dígitos del RUN (sin considerar el dígito verificador) del integrante de 
  #    menor edad del equipo.

set.seed(3728)

  # 2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) 
  #    o 50 hombres (si la semilla es impar).

  # semilla es par, luego utilizamos una muestra de 50 mujeres

mujeres <- datos %>% filter(Gender == "0")
nombre.variables <- colnames(mujeres)

indices.muestra <- sample(nrow(mujeres),size=50)
muestra.mujeres <- mujeres[indices.muestra,]

  # 3. Seleccionar de forma aleatoria ocho posibles variables predictoras.
set.seed(1998)
nombre.variables <- colnames(mujeres)
nombre.8var <- sample(nombre.variables,8, replace = FALSE)

muestra.mujeres.8variables <- muestra.mujeres %>% select(nombre.8var)

  # De manera aleatoria, se seleccionaron las siguientes 8 variables:

  # - Knee.Girth: Grosor promedio de ambas rodillas, posición levemente flectada,
  #               medición arriba de la rótula.
  # - Ankles.diameter: Suma de los diámetros de los tobillos.
  # - Wrist.Minimum.Girth: Grosor promedio de la parte más delgada de ambas muñecas.
  # - Thigh.Girth: Grosor promedio de ambos muslos bajo el pliegue del glúteo.
  # - Wrists.diameter: Suma de los diámetros de las muñecas.
  # - Chest.diameter: Diámetro del pecho (a la altura de los pezones).
  # - Forearm.Girth: Grosor promedio de ambos antebrazos, brazos extendidos palmas 
  #                  hacia arriba.
  # - Ankle.Minimum.Girth: Grosor promedio de la parte más delgada de ambos tobillos.



  # 4. Seleccionar, entre las variables que no fueron escogidas en el punto 
  #    anterior, una que el equipo considere que podría ser útil para predecir 
  #    la variable Peso, justificando bien esta selección.

muestra.filtrada <- muestra.mujeres %>% select(!nombre.8var)

matriz.covarianza <- cor(muestra.filtrada, y= muestra.mujeres$Weight)
print(matriz.covarianza)

# La variable que se decide agregar para poder predecir el peso, es Hip.Girth 
# (Grosor a la altura de las caderas), se tomó la decisión de elegir esta opción,
# por que posee la correlación más fuerte a diferencia de las otras variables.

  # 5. Usando el entorno R, construir un modelo de regresión lineal simple con 
  #    el predictor seleccionado en el paso anterior.

# r <- cor(muestra.mujeres$Height,muestra.mujeres$Weight)
# muestra.pesoYcadera <- select(muestra.mujeres,Weight,Hip.Girth)

# Ajustar modelo con R.
modelo <- lm(muestra.mujeres$Weight ~ muestra.mujeres$Hip.Girth , data = muestra.mujeres)
print(summary (modelo))

# Graficar el modelo .
p <- ggscatter(muestra.mujeres, x = "wt", y = "mpg", color = " blue ", fill = " blue ",
               xlab = " Peso [lb x 1000] ", ylab = " Rendimiento [ millas /galón]")

p <- p + geom_smooth(method = lm, se = FALSE, colour = "red")
print( p )

# Crear gráficos para evaluar el modelo .
plot ( modelo )

  # 6. Usando herramientas para la exploración de modelos del entorno R, escoger
  #    entre dos y cinco predictores de entre las variables seleccionadas en los 
  #    puntos 3 y 4 (9 en total) para construir un modelo de regresión lineal múltiple.

nombre.9var <- c(nombre.8var, "Hip.Girth")

muestra.mujeres.9variables <- muestra.mujeres %>% select(nombre.9var)

# Ajustar modelo con todos los subconjuntos.
modelos <- regsubsets(muestra.mujeres$Weight ~ ., 
                      data = muestra.mujeres.9variables, 
                      method = "exhaustive", nbest = 1, nvmax = 9)
print(plot(modelos))

# Knee.Girth, Forearm.Girth y Hip.Girth

  # 7. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema 
  #    con las condiciones que deben cumplir.

# Ajustar modelo.
modelo <- lm(muestra.mujeres$Weight ~  
               muestra.mujeres$Knee.Girth +
               muestra.mujeres$Forearm.Girth +
               muestra.mujeres$Hip.Girth, data = datos)
print(modelo)

# Comprobar independencia de los residuos.
cat("Prueba de Durbin-Watson para autocorrelaciones ")
cat("entre errores:\n")
print(durbinWatsonTest(modelo))

# Comprobar normalidad de los residuos.
cat("\nPrueba de normalidad para los residuos:\n")
print(shapiro.test(modelo$residuals))

# Comprobar homocedasticidad de los residuos.
cat("Prueba de homocedasticidad para los residuos:\n")
print(ncvTest(modelo))

# Comprobar la multicolinealidad.
vifs <- vif(modelo)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs:\n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

  # 8. Evaluar el poder predictivo del modelo en datos no utilizados para 
  #    construirlo (o utilizando validación cruzada).



#datos.prueba <- muestra.mujeres[!indices.muestra,]

# Se crea un conjuntos de entrenamiento y prueba
# Se establece una semilla con los ultimos digitos del rut de Ivan.
set.seed(3345)
# Se calcula el tamaño de los datos de prueba.
n <- nrow(muestra.mujeres)
# Se crea el tamaño del entrenamiento (80% de los datos)
tamaño_entrenamiento <- floor(0.8 * n)
muestra <- sample.int(n = n, size = tamaño_entrenamiento, replace = FALSE)
# Se crea la variable de entrenamiento
entrenamiento <- muestra.mujeres[muestra, ]
# Se crea la prueba a realizar
prueba_2 <- muestra.mujeres[-muestra, ]
# Se crea el modelo
modelo_2 <- lm(muestra.mujeres$Weight ~  
               muestra.mujeres$Ankles.diameter, data = datos)

# Se ajusta el modelo con el conjunto de entrenamiento.
print(summary(modelo_2))

# Se calcula el error cuadratico medio para el conjunto de entrenamiento.
ecm_entrenamiento <- mean(modelo_2$residuals ** 2)
cat("MSE para el conjunto de entrenamiento: ", ecm_entrenamiento, "\n")

# Se realizan las predicciones para el conjunto de prueba.
predicciones <- predict(modelo_2, prueba_2)
# Se calcula el error cuadratico medio (ECM) para el conjunto de prueba.
error <- sapply(prueba_2[["Weight"]],as.double) - predicciones
ecm_prueba <- mean(error ** 2)
cat("MSE para el conjunto de prueba: ", ecm_prueba)

# El MSE de entrenamiento es 103.8907 y el MSE de prueba es 100.4517 

# Se puede ver que el error cuadrado promedio del conjunto de entrenamiento
# es muy similar al del conjunto de prueba, por lo que se puede concluir que es generalizable.