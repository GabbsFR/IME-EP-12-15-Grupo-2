# Ejercicio Práctico N°15: REGRESIÓN LINEAL Y LOGÍSTICA CON CARET
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
library(leaps)
#-------------------------- Enunciado:  ----------------------------------------

# Para esta actividad usaremos los datos de medidas anatómicas recolectados por 
# Heinz et al. (2003) que ya conocimos en los ejercicios prácticos anteriores, 
# añadiendo las variables ICM y EN creada en el ejercicio práctico anterior.

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

# 1) Definir la semilla a utilizar, que corresponde a los primeros cinco dígitos
#    del RUN del integrante de mayor edad del equipo.

# Se establece la semilla con los primeros cinco digitos del RUT del integrante
# Bastián Soto rut: 19.495.542-1
set.seed(19495)

# Se realiza la lectura del archivo y se almacena en la variable "datos".
datos <- read.csv2(file.choose(), 
                   encoding = "latin1", 
                   stringsAsFactors = TRUE, 
                   check.names = F)

# Se define el Indice Masa Corporal
datos[["IMC"]] <- datos$Weight/((datos$Height)/100)^2

# Estado Nutricional 
# IMC >= 25,0 : Sobrepeso
# IMC < 25,0 : No sobrepeso 

# Creamos la variable dicotómica 
# 1: Sobrepeso
# 0: No sobrepeso

condicion <- ifelse(datos[["IMC"]] >= 25.0, 1, 0)
datos[["EN"]] <- factor(condicion)

# 2) Seleccionar una muestra de 100 personas, asegurando que la mitad tenga estado
#    nutricional “sobrepeso” y la otra mitad “no sobrepeso”.

# a diferencia de los ejercicos practicos anteriores, se debe seleccionar 100 
# personas, independiente del sexo, solo se requiere asegurar que la mitad este
# sobrepeso y la otra mitad que no lo esté.

# Se saca las 50 muestras con sobrepeso y las 50 sin sobrepeso
sobrepeso <- datos %>% filter(EN == 1)
muestra_sobrepeso <- sobrepeso[sample(nrow(sobrepeso),50),]
noSobrepeso <- datos %>% filter(EN == 0)
muestra_nosobrepeso <- noSobrepeso[sample(nrow(noSobrepeso),50),]
# se crea la variable muestra_datos, con las 100 personas solicitadas.
muestra_datos<- rbind(muestra_sobrepeso,muestra_nosobrepeso)


# 3) Usando las herramientas del paquete leaps, realizar una búsqueda exhaustiva
#    para seleccionar entre dos y ocho predictores que ayuden a estimar la variable
#    Peso (Weight), obviamente sin considerar las nuevas variables IMC ni EN, y 
#    luego utilizar las funciones del paquete caret para construir un modelo de regresión
#    lineal múltiple con los predictores escogidos y evaluarlo usando bootstrapping.

set.seed(19495)

# Se separa el conjunto de entrenamiento y prueba 40 y 20 de cada muestra
test_sobrepeso <- sample.int(nrow(muestra_sobrepeso),40, replace = FALSE)
test_nosobrepeso <- sample.int(nrow(muestra_nosobrepeso),40, replace = FALSE)

# Se necesita dividir el conjunto de mujeres tanto de las que estan en sobrepeso
# como las que no lo estan ya que para ajustar el modelo, primeramente se necesita
# un modelo de entrenamiento y otro de prueba.
sobrepeso40 <- muestra_sobrepeso[test_sobrepeso,]
sobrepeso10 <- muestra_sobrepeso[-test_sobrepeso,]
nosobrepeso40 <- muestra_nosobrepeso[test_nosobrepeso,]
nosobrepeso10 <-muestra_nosobrepeso[-test_nosobrepeso,]

entrenamiento_con_IMC_EN <- rbind(sobrepeso40,nosobrepeso40)
prueba_con_IMC_EN <- rbind(sobrepeso10,nosobrepeso10)

borrar <-c("EN","IMC")
entrenamiento <- entrenamiento_con_IMC_EN[,!(names(entrenamiento_con_IMC_EN) %in% borrar)]
prueba <- prueba_con_IMC_EN[,!(names(prueba_con_IMC_EN) %in% borrar)]
nombre.variables <- colnames(entrenamiento)

# Se realiza la busqueda exaustiva con regsubsets sin considerar las variables
# IMC y EN.

# Ajustar modelo con todos los subconjuntos
modelos <- regsubsets(Weight ~ ., data = entrenamiento,
                      method = "exhaustive", nbest = 1, nvmax = 8)

print(plot(modelos))

# Viendo la matriz obtenida, elegimos "Chest.depth", "Chest.diameter",
# "Waist.Girth", "Hip.Girth", "Forearm.Girth", "Calf.Maximum.Girth",
# "Age" y "Height"

seleccionados <- c("Chest.depth", "Chest.diameter","Waist.Girth", "Hip.Girth", 
                   "Forearm.Girth", "Calf.Maximum.Girth", "Age", "Height")

# Uso de caret    



# 4) Haciendo un poco de investigación sobre el paquete caret, en particular cómo 
#    hacer Recursive Feature Elimination (RFE), construir un modelo de regresión 
#    lineal múltiple para predecir la variable IMC que incluya entre 10 y 20 
#    predictores, seleccionando el conjunto de variables que maximice R2 y que use 
#    cinco repeticiones de validación cruzada de cinco pliegues para evitar el 
#    sobreajuste (obviamente no se debe considerar las variables Peso, Estatura 
#    ni estado nutricional –Weight, Height, EN respectivamente).



# 5) Usando RFE, construir un modelo de regresión logística múltiple para la variable
#    EN que incluya el conjunto, de entre dos y seis, predictores que entregue la
#    mejor curva ROC y que utilice validación cruzada dejando uno fuera para evitar
#    el sobreajuste (obviamente no se debe considerar las variables Peso, Estatura 
#    –Weight y Height respectivamente– ni IMC).



# 6) Pronunciarse sobre la confiabilidad y el poder predictivo de los modelos.

