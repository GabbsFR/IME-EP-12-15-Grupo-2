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

# Se crea la variable borrar, para eliminar las variables que no se deben utilizar
borrar <-c("EN","IMC")

muestra_sin_EN_IMC <- muestra_datos[,!(names(muestra_datos) %in% borrar)]
nombre.variables <- colnames(muestra_sin_EN_IMC)

# Se realiza la busqueda exaustiva con regsubsets sin considerar las variables
# IMC y EN.

# Ajustar modelo con todos los subconjuntos
modelos <- regsubsets(Weight ~ ., data = muestra_sin_EN_IMC,
                      method = "exhaustive", nbest = 1, nvmax = 8)

print(plot(modelos))

# Viendo la matriz obtenida, elegimos "Chest.depth", "Chest.diameter",
# "Waist.Girth", "Hip.Girth", "Forearm.Girth", "Calf.Maximum.Girth",
# "Age" y "Height"
seleccionados_mas_peso <- c("Chest.depth", "Chest.diameter","Waist.Girth", "Hip.Girth", 
                   "Forearm.Girth", "Calf.Maximum.Girth", "Age", "Height","Weight")
muestra_datos_modelo <- muestra_sin_EN_IMC %>%select(seleccionados_mas_peso)

# Usamo caret para ajustar el modelo.    

# Ajustar modelo usando validación cruzada de 5 pliegues.
modelo <- train(Weight ~ ., data = muestra_datos_modelo, method = "lm",
                trControl = trainControl(method = "boot", number = 5))
print(summary(modelo))


vifs <- vif(modelo$finalModel)
print(vifs)

# Modificamos el predictor eliminando la variable "waist.Girth"
muestra_datos_modelo_p1 <- muestra_datos_modelo %>% select(!"Waist.Girth")
# Ajustar modelo usando validación cruzada de 5 pliegues.
modelo_p1 <- train(Weight ~ ., data = muestra_datos_modelo_p1, method = "lm",
                trControl = trainControl(method = "boot", number = 5))
print(summary(modelo_p1))

vifs_p1 <- vif(modelo_p1$finalModel)
print(vifs_p1)

# Modificamos el predictor eliminando la variable "Age"
muestra_datos_modelo_p2 <- muestra_datos_modelo_p1 %>% select(!"Age")

# Ajustar modelo usando validación cruzada de 5 pliegues.
modelo_p2 <- train(Weight ~ ., data = muestra_datos_modelo_p2, method = "lm",
                   trControl = trainControl(method = "boot", number = 5))
print(summary(modelo_p2))

vifs_p2 <- vif(modelo_p2$finalModel)
print(vifs_p2)

# Modificamos el predictor eliminando la variable "Forearm.Girth"
muestra_datos_modelo_p3 <- muestra_datos_modelo_p2 %>% select(!"Forearm.Girth")

# Ajustar modelo usando validación cruzada de 5 pliegues.
modelo_p3 <- train(Weight ~ ., data = muestra_datos_modelo_p3, method = "lm",
                   trControl = trainControl(method = "boot", number = 5))
print(summary(modelo_p3))

vifs_p3 <- vif(modelo_p3$finalModel)
print(vifs_p3)

# Modificamos el predictor eliminando la variable "Chest.diameter"
muestra_datos_modelo_p4 <- muestra_datos_modelo_p3 %>% select(!"Chest.diameter")
# Ajustar modelo usando validación cruzada de 5 pliegues.
modelo_p4 <- train(Weight ~ ., data = muestra_datos_modelo_p4, method = "lm",
                   trControl = trainControl(method = "boot", number = 5))
print(summary(modelo_p4))

vifs_p4 <- vif(modelo_p4$finalModel)
print(vifs_p4)


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

