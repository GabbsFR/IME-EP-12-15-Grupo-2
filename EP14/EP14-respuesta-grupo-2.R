# Ejercicio Práctico N°14
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
library(pROC)
library(caret)
library(tidyverse)

#-------------------------- Enunciado:  ----------------------------------------
# Para esta actividad usaremos los datos de medidas anatómicas recolectados por 
# Heinz et al. (2003) que ya conocimos en el ejercicio práctico anterior. 
# Como este ejercicio requiere de una variable dicotómica, vamos a realizar 
# lo siguiente:

# ▪ Crear la variable IMC (índice de masa corporal) como el peso de una persona 
#   (en kilogramos) dividida por el cuadrado de su estatura (en metros).
# ▪ Si bien esta variable se usa para clasificar a las personas en varias clases
#   de estado nutricional (bajo peso,sobrepeso (IMC ≥ 25,0) y no sobrepeso (IMC < 25,0) .                                                                                                      normal, sobrepeso, obesidad, obesidad mórbida), para efectos de este ejercicio, usaremos dos clases:
# ▪ Crear la variable dicotómica EN (estado nutricional) de acuerdo al valor 
#   de IMC de cada persona.

# Recordando las variables registradas del archivo del ejercico practico anterior: 

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

# ---------------------------- Preguntas ---------------------------------------

# 1) Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del
#    RUN (sin considerar el dígito verificador) del integrante de mayor edad del equipo.

# Se establece la semilla con los ultimos cuatro digitos del RUT del integrante
# Bastián Soto rut: 19.495.542-1
set.seed(5542)


# 2) Seleccionar una muestra de 120 mujeres (si la semilla es un número par) o 
#    120 hombres (si la semilla es impar) asegurando que la mitad tenga estado
#    nutricional “sobrepeso” y la otra mitad “no sobrepeso”.
#    Dividir esta muestra en dos conjuntos: los datos de 80 personas (40 con EN “sobrepeso”)
#    para utilizar en la construcción de los modelos y 40 personas (20 con EN “sobrepeso”) 
#    para poder evaluarlos. 

# Se realiza la lectura del archivo y se almacena en la variable "datos".
datos <- read.csv2(file.choose(), 
                   encoding = "latin1", 
                   stringsAsFactors = TRUE, 
                   check.names = F)


datos[["IMC"]] <- datos$Weight/((datos$Height)/100)^2

# Estado Nutricional 
# IMC >= 25,0 : Sobrepeso
# IMC < 25,0 : No sobrepeso 

# Creamos la variable dicotómica 
# 1: Sobrepeso
# 0: No sobrepeso

condicion <- ifelse(datos[["IMC"]] >= 25.0, 1, 0)
datos[["EN"]] <- factor(condicion)


# Se filtran los datos, correspondientes a las mujeres debido a que la semilla
# establecida es par. (Gender == 0)
mujeres <- datos %>% filter(Gender == "0")
# Se saca las 60 muestras con sobrepeso y las 60 sin sobrepeso
sobrepeso <- mujeres %>% filter(EN == 1)
muestra_sobrepeso <- sobrepeso[sample(nrow(sobrepeso),60,replace = TRUE),]
noSobrepeso <- mujeres %>% filter(EN == 0)
muestra_nosobrepeso <- noSobrepeso[sample(nrow(noSobrepeso),60),]

# Se separa el conjunto de entrenamiento y prueba 40 y 20 de cada muestra
test_sobrepeso <- sample.int(nrow(muestra_sobrepeso),40, replace = FALSE)
test_nosobrepeso <- sample.int(nrow(muestra_nosobrepeso),40, replace = FALSE)

# Se necesita dividir el conjunto de mujeres tanto de las que estan en sobrepeso
# como las que no lo estan ya que para ajustar el modelo, primeramente se necesita
# un modelo de entrenamiento y otro de prueba.
sobrepeso40 <- muestra_sobrepeso[test_sobrepeso,]
sobrepeso20 <- muestra_sobrepeso[-test_sobrepeso,]
nosobrepeso40 <- muestra_nosobrepeso[test_nosobrepeso,]
nosobrepeso20 <-muestra_nosobrepeso[-test_nosobrepeso,]



# Se requiere tomar 2 grupos de muestras de mujer
# el primero "mujeres80" corresponde a 80 mujeres, de las cuales, 40 estan en
# sobrepeso y las otras 40 no estan en sobrepeso
# y el segundo grupo "mujeres40" corresponde a 40 mujeres de las cuales 20 estan
# en sobrepeso y las otras 20 no estan en sobrepeso
# Estos 2 grupos corresponden a conjuntos de entrenamiento y prueba, donde 
# mujeres80 es para entrenamiento y mujeres 40 para prueba.
mujeres80_entrenamiento <- rbind(sobrepeso40,nosobrepeso40)
mujeres40_prueba <- rbind(sobrepeso20,nosobrepeso20)


# 3) Recordar las ocho posibles variables predictoras seleccionadas de forma 
#    aleatoria en el ejercicio anterior.

# En el ejercicio practico anterior, se seleccionaron las siguientes 8 variables:

# - Knee.Girth: Grosor promedio de ambas rodillas, posición levemente flectada,
#               medición arriba de la rótula.
# - Ankles.diameter: Suma de los diámetros de los tobillos.
# - Wrist.Minimum.Girth: Grosor promedio de la parte más delgada de ambas muñecas.
# - Thigh.Girth: Grosor promedio de ambos muslos bajo el pliegue del glúteo.
# - Wrists.diameter: Suma de los diámetros de las muñecas.
# - Chest.diameter: Diámetro del pecho (a la altura de los pezones).
# - Forearm.Girth: Grosor promedio de ambos antebrazos, brazos extendidos palmas 
#                  hacia arriba.
# - Ankle.Minimum.Girth: Grosor promedio de la parte más delgada de ambos tobillos


# Luego se requiere sumar a estas 8 variables, el estado nutricional EN y el peso.

set.seed(1998)
nombre.variables <- colnames(mujeres)
nombre.8var <- sample(nombre.variables,8, replace = FALSE)

# 4) Seleccionar, de las otras variables, una que el equipo considere que podría 
#    ser útil para predecir la clase EN, justificando bien esta selección.

# 
muestra.filtrada <- mujeres80_entrenamiento %>% select(!nombre.8var) %>% select(!IMC)

# Ajustar modelo nulo.
nulo <- glm(EN ~ 1, family = binomial(link = "logit"), data = muestra.filtrada)

# Ajustar modelo completo.
cat("\n\n")
completo <- glm(EN ~ ., family = binomial(link = "logit"),
                data = muestra.filtrada)

# Ajustar modelo con regresión escalonada.
cat("Modelo con regresión escalonada\n")
cat("--------------------------------------\n")
mejor <- step(nulo, scope = list(lower = nulo, upper = completo),
              direction = "both", trace = 0)

print(summary(mejor))

# Luego con los datos obtenidos tenemos dos posibles predictores para elegir,
# el peso(Weight) o la altura (Height) de los cuales escogeremos el peso.


# 5) Usando el entorno R y paquetes estándares, construir un modelo de regresión 
#    logística con el predictor seleccionado en el paso anterior y utilizando de
#    la muestra obtenida.

# Ajustar modelo.
modelo <- glm(EN ~ Weight, family = binomial(link = "logit"), 
              data = mujeres80_entrenamiento)

cat("\n Modelo regresión logística simple")
print(summary(modelo))

# Evaluar el modelo con el conjunto de prueba. 
cat("Evaluación del modelo a partir del conjunto de prueba:\n")

umbral <- 0.5
probs <- predict(modelo, mujeres40_prueba, type = "response")
preds <- sapply(probs, function(p) ifelse(p >= umbral, "Sobrepeso", "No sobrepeso"))

preds <- factor(preds, levels= levels(mujeres40_prueba[["EN"]]))

ROC_prueba <- roc(mujeres40_prueba[["EN"]], probs)
plot(ROC_prueba)

matriz_confusion <- confusionMatrix(preds, mujeres40_prueba[["EN"]])
print(matriz_confusion)

# Se puede ver que la curva se aleja bastante de la diagonal, por lo que se puede
# concluir que el conjunto prueba es un buen modelo. 



# 6) Usando herramientas estándares1 para la exploración de modelos del entorno R,
#    buscar entre dos y cinco predictores de entre las 9 variables seleccionadas en
#    pasos anteriores para construir un modelo de regresión logística múltiple.

# Recordando las variables seleccionadas en el ejercicio anterior: 

# - Knee.Girth
# - Ankles.diameter
# - Wrist.Minimum.Girth
# - Thigh.Girth
# - Wrists.diameter
# - Chest.diameter
# - Forearm.Girth
# - Ankle.Minimum.Girth


# Se seleccionan las variables a trabajar.
mujeres80_entrenamiento <- mujeres80_entrenamiento %>% select(Knee.Girth,
                                                              Ankles.diameter,
                                                              Wrist.Minimum.Girth,
                                                              Thigh.Girth,
                                                              Wrists.diameter,
                                                              Chest.diameter,
                                                              Forearm.Girth,
                                                              Ankle.Minimum.Girth,
                                                              EN,
                                                              Weight)

mujeres40_prueba <- mujeres40_prueba %>% select(Knee.Girth,
                                                 Ankles.diameter,
                                                 Wrist.Minimum.Girth,
                                                 Thigh.Girth,
                                                 Wrists.diameter,
                                                 Chest.diameter,
                                                 Forearm.Girth,
                                                 Ankle.Minimum.Girth,
                                                 EN,
                                                 Weight)

reg_multiple_predictores <- regsubsets(EN ~ ., data = mujeres80_entrenamiento, nbest = 1, 
                                       nvmax = 5, method = "exhaustive")
plot(reg_multiple_predictores)

# Según el método de exploración de subconjuntos, el mejor modelo que usa 
# entre 2 y 5 variables es el que usa como predictores el grosor de ambos muslos
# y la suma de los diámetros de las muñecas. 

# Ajustar el modelo con los predictores seleccionados. 

modelo_RLM <- glm(EN ~ Thigh.Girth + Wrists.diameter, data = mujeres80_entrenamiento,
                           family = binomial(link = "logit"))
cat("\nModelo de regresión logística múltiple\n")
print(summary(modelo_RLM))


# 7) Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste 
#    y son generalizables) y “arreglarlos” en caso de que tengan algún problema.

# --------------- EVALUACIÓN DEL MODELO RLM -----------------------
# Obtener los residuos y las estadísticas .
output <- data.frame (predicted.probabilities = fitted(modelo_RLM))
output [["standardized.residuals"]] <- rstandard(modelo_RLM)
output [["studentized.residuals"]] <- rstudent( modelo_RLM )
output [["cooks.distance"]] <- cooks.distance(modelo_RLM)
output [["dfbeta"]] <- dfbeta(modelo_RLM )
output [["dffit"]] <- dffits(modelo_RLM)
output [["leverage"]] <- hatvalues(modelo_RLM)

# Evaluar residuos estandarizados que escapen a la normalidad.
# 95 % de los residuos estandarizados deberían estar entre
# -1.96 y 1.96 , y 99 % entre -2.58 y 2.58.
sospechosos1 <- which (abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1 )
cat ("\n\n")
cat (" Residuos estandarizados fuera del 95 % esperado \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - -\n")
print(rownames(mujeres80_entrenamiento[sospechosos1, ]) )

# Revisar casos con distancia de Cook mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat ("\n\n")
cat ("Residuales con una distancia de Cook alta \n")
cat ("- - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n")
print(rownames(mujeres80_entrenamiento[sospechosos2, ]))

# Revisar casos cuyo apalancamiento sea más del doble
# o triple del apalancamiento promedio .
leverage.promedio <- ncol(mujeres80_entrenamiento)/nrow(mujeres80_entrenamiento)
sospechosos3 <- which(output [["leverage "]] > leverage.promedio)
sospechosos3 <- sort(sospechosos3)

cat ("\n\n")

cat (" Residuales con levarage fuera de rango ( > ")
cat (round(leverage.promedio, 3) , ")", "\n", sep = "")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(rownames(mujeres80_entrenamiento[sospechosos3, ]) )

# Revisar casos con DFBeta >= 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1 ,1 ,any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4 ) <- NULL
cat ("\n\n")
cat (" Residuales con DFBeta sobre 1\n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - -\n")
print(rownames(mujeres80_entrenamiento[sospechosos4 , ]))

# Detalle de las observaciones posiblemente atí picas .
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4)
sospechosos <- sort (unique(sospechosos))
cat ("\n\n")
cat (" Casos sospechosos \n")
cat (" - - - - - - - - - - -- - - - - -\n")
print(mujeres80_entrenamiento[sospechosos, ])
cat("\n\n")
print(output[sospechosos , ])


# 8) Usando código estándar evaluar el poder predictivo de los modelos con los 
#    datos de las 40 personas que no se incluyeron en su construcción en términos
#    de sensibilidad y especificidad.



#----------- EVALUAR PODER PREDICTIVO PARA RLM -------
# Evaluar el modelo con el conjunto de entrenamiento
cat ("Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict(modelo_RLM, mujeres80_entrenamiento, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e , function (p) ifelse( p >= umbral , "Sobrepeso", "No sobrepeso"))
preds_e <- factor ( preds_e , levels = levels ( mujeres80_entrenamiento[["EN"]]) )

ROC_e <- roc(mujeres80_entrenamiento[["EN"]], probs_e)
plot(ROC_e)

matriz_e <- confusionMatrix(preds_e , mujeres80_entrenamiento[["EN"]])
print(matriz_e)



# Evaluar el modelo con el conjunto de prueba.
cat ("Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict(modelo_RLM, mujeres40_prueba , type = "response")

preds_p <- sapply(probs_p , function (p) ifelse ( p >= umbral , "Sobrepeso", "No sobrepeso") )
preds_p <- factor(preds_p , levels = levels ( mujeres40_prueba[["EN"]]) )

ROC_p <- roc(mujeres40_prueba[["EN"]] , probs_p)
plot(ROC_p)

matriz_p <-confusionMatrix(preds_p , mujeres40_prueba[["EN"]])
print(matriz_p)

