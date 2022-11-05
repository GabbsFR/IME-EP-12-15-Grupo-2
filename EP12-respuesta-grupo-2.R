# Ejercicio Práctico N°12
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



#-------------------------- Enunciado:  ----------------------------------------
# En el trabajo de título de un estudiante del DIINF se reportan los siguientes
# tiempos de ejecución (en milisegundos) medidos para dos versiones de un algoritmo 
# genético para resolver instancias del problemadel vendedor viajero disponibles 
# en repositorios públicos. 

# | Instancia A | Tiempo A | Instancia B | Tiempo B |
# |     137     |  210041  |      11     |  2830464 |
# |      19     |  783108  |      64     |   180141 |
# |      87     |  834565  |     104     |   994830 |
# |     150     |   70599  |      82     |  6684497 |
# |      18     | 8576989  |      96     |    35974 |
# |      91     |  251843  |     175     |  5743260 |
# |     147     | 4428151  |      84     |  4629726 |
# |     139     |   48667  |     106     |    48408 |
# |      69     |   48705  |     121     |  2196277 |
# |      21     |  885722  |       6     |    92932 |

#-------------------------- Pregunta N°1  --------------------------------------
# ¿Es uno de los algoritmos más rápido que el otro?

# Hipotesis
  # H0: El tiempo medio de ejecución de los algoritmos A y B es igual (Ta = Tb).
  # HA: El tiempo medio de ejecución de los algoritmos A y B es distinto (Ta != Tb).


  # Se establece un nivel de confianza del 99% (alfa = 0.01)
  alfa <- 0.01   
  # Dataframe de los datos
  Instancia_A <- c(137, 19, 87, 150, 18, 91, 147, 139, 69, 21)
  Tiempo_A <- c(210041, 783108, 834565, 70599, 8576989, 251843, 4428151, 48667,
                48705, 885722)
  Instancia_B <- c(11, 64, 104, 82, 96, 175, 84, 106, 121, 6)
  Tiempo_B <- c(2830464, 180141, 994830, 6684497, 35974, 5743260, 4629726, 48408,
                2196277, 92932)
  # Se sabe que las muestras son independientes entre si, por lo cual, vamos a
  # separar los datos en datos_A (para las Instancia_A y Tiempo_A) y datos_B (instancia_B 
  # y Tiempo_B) con el fin de evauluar la condición de normalidad, entre las muestras.
  datos_A <- data.frame(Instancia_A, Tiempo_A)
  datos_B <- data.frame(Instancia_B, Tiempo_B)

  # Se hace la prueba de shapiro wilk, para verificar que las muestras cumplen con
  # el supuesto de normalidad.
  prueba_normalidad_A <- shapiro.test(datos_A$Tiempo_A)
  prueba_normalidad_B <- shapiro.test(datos_B$Tiempo_B)

  # Se muestra los resultados del test shapiro wilk
  print(prueba_normalidad_A)
  print(prueba_normalidad_B)

  # Se logra apreciar con la prueba shapiro wilk, que uno de los tiempos (tiempo A)
  # No cumple con la condición de normalidad, por lo cual, se debe aplicar una transformación
  
  # para este caso, se utiliza la transformación logaritmica, porque asegura que la
  # distribución se asemeje a la normal.
  # se debe aplicar la transformación para ambas muestras.
  logTiempoA <- log(datos_A$Tiempo_A)
  logTiempoB <- log(datos_B$Tiempo_B)
  
  # Se aplica la prueba shapiro wilk para verificar la condición de normalidad.
  print(shapiro.test(logTiempoA))
  print(shapiro.test(logTiempoB))
  
  # Se demuestra que ahora si se cumple la condición de normalidad en ambas muestras
  # de tiempo (Tiempo_A , Tiempo_B).
  # Se realiza la prueba t de student para 2 muestras independientes.
  # con el fin de saber si los tiempos medios son iguales o distintos.
  prueba_t <- t.test(x = logTiempoA,
                     y = logTiempoB,
                     paired = FALSE,
                     alternative = "two.sided",
                     mu = 0,
                     conf.level = 1-alfa)

  # Se muestra los resultados de la prueba t de student
  print(prueba_t)

# Resultado:
  # Como resultado, obtenemos un valor de p = o,5634, por lo que se observa, dicho
  # valor es mayor al alfa establecido (0.01).

  # Conclusión
  # Como p = 0.5634 > alfa = 0.01
  # Se rechaza la hipotesis nula a favor de la anternativa, en consecuencia, se 
  # puede concluir con un 99% de confianza que si  existe diferencia entre los 
  # tiempos medios de los algoritmos A y B.


#-------------------------- Pregunta N°2  --------------------------------------
  # Analice la primera pregunta abordada en el ejercicio práctico 11, con los mismos
  # datos, utilizando un método robusto adecuado.

# Enunciado Pregunta N°1 del EP 11

  # Un alumno del curso de IMEH (Inferencia y Modelos Estadísticos en Halloween) de
  # la Universidad de Comediantes de Chile desea saber si la media de edad de las 
  # personas de orientación heterosexual que viven en la región de Valparaíso
  # es la misma para aquellas heterosexuales que viven en la región del Biobío.
  
  # Se utiliza la prueba de yuen para dos muestras independientes.
  
  # Estadístico de interés: la media de la edad de las personas.
  
# Hipotesis:
  # Planteamiento de Hipótesis. 
  # H0: La edad media de las personas heterosexuales que viven en la Región de Valparaíso y en la Región
  # del Biobío es la misma.
  
  # HA: La edad media de las personas heterosexuales que viven en la Región de Valparaíso y en la Región 
  # del Biobío es distinta.
  
  # Definiendo uA como el promedio de las edades de las personas que viven en la Región de Valparaíso, 
  # y uB, como el promedio de las edades de las personas que viven en la Región del Biobío. 
  
  # Reescribiendo las hipótesis: 
  # H0: uA = uB 
  # HA: uA != uB
  set.seed(3331)
  datos <- read.csv2(file.choose(), stringsAsFactors = TRUE, check.names = F)
  
  # Se aplica un filtro para obtener los datos de la personas que viven en la 
  # Región de Valparaíso y la Región del Biobío.
  valpo <- datos %>% filter(region == "Región de Valparaíso")
  biobio <- datos %>% filter(region == "Región del Biobío")
  
  # Se aplica un filtro para obtener los datos de las personas de orientación heterosexual  en la 
  # Región de Valparaíso.
  heteroValpo <- valpo %>% filter(r23 == "Heterosexual (Atracción hacia el sexo opuesto)")
  
  # Se filtra para obtener los datos de las personas de orientación heterosexual en la 
  # Región del Biobío.
  heteroBiobio <- biobio %>% filter(r23 == "Heterosexual (Atracción hacia el sexo opuesto)")
  
  # Se obtiene la edad de las personas de orientación sexual tanto en valparaíso 
  # como en la región de biobío.
  edadheteroValpo <- heteroValpo[["edad"]]
  edadheteroBiobio <- heteroBiobio[["edad"]]
  
  # Se crean las tablas de las muestras a trabajar.
  muestraheteroValpo <- sample(edadheteroValpo, 260)
  muestraheteroBiobio<- sample(edadheteroBiobio, 300)
  
  # Desde aquí comienza la prueba de Yuen para dos muestras independientes.
  edad <- c(muestraheteroValpo,muestraheteroBiobio)
  region<- c(rep("Valparaíso", length(muestraheteroValpo)), rep("Biobío", length(muestraheteroBiobio)))
  datos_pregunta_2 <- data.frame(edad, region)
  
  # Comprobar normalidad. 
  g <- ggqqplot(datos_pregunta_2, x = "edad", facet.by = "region", 
                xlab = "edad", ylab = "region", 
                title = "Muestras originales",
                palette = c("blue", "red"), color = "region")
  
  print(g)
  
  # Establecer nivel de significación. 
  alpha_2 <- 0.05
  
  # Ver poda del 20%. 
  gamma <- 0.2
  n_muestra_valpo <- length(muestraheteroValpo)
  n_muestra_biobio <- length(muestraheteroBiobio)
  
  poda_valpo <- n_muestra_valpo * gamma 
  poda_biobio <- n_muestra_biobio * gamma 
  
  muestra_valpo_truncada <- muestraheteroValpo[poda_valpo:(n_muestra_valpo - poda_valpo)]
  muestra_biobio_truncada <- muestraheteroBiobio[poda_biobio:(n_muestra_biobio - poda_biobio)]
  
  edad <- c(muestra_valpo_truncada, muestra_biobio_truncada)
  region <- c(rep("Valparaíso", length(muestra_valpo_truncada)), rep("Biobío", length(muestra_biobio_truncada)))
  datos_pregunta_2_truncados <- data.frame(edad, region)
  
  g <- ggqqplot(datos_pregunta_2_truncados, x = "edad", facet.by = "region", 
                xlab = "edad", ylab = "region", 
                title = "Muestras con medias truncadas.",
                palette = c("blue", "red"), color = "region") 
  print(g)
  
  # Aplicar prueba de Yuen. 
  prueba <- yuen(edad ~ region, data = datos_pregunta_2, tr = gamma)
  print(prueba)


#-------------------------- Pregunta N°3  --------------------------------------
  # Analice la segunda pregunta abordada en el ejercicio práctico 11, con los mismos 
  # datos, utilizando un método robusto adecuado.
  
  
  # En promedio, el ingreso per cápita (ytotcorh / numper) en Chile es similar en
  # hogares donde el nivel educacional (educ) del jefe o jefa del hogar es:
  # - Profesional Completo
  # - Postgrado Incompleto
  # - Postgrado Completo
  
  
  # Planteamiento de Hipótesis. 
  # H0: En promedio el ingreso per cápita es igual en hogares donde el nivel educacional
  #     del jefe o jefa del hogar es Profesional Completo, Postgrado Incompleto
  #     o Postgrado Completo.
  
  # HA: En promedio el ingreso per cápita es distinto en hogares donde el nivel educacional
  #     del jefe o jefa del hogar es Profesional Completo, Postgrado Incompleto
  #     o Postgrado Completo.
  
  # Filtrar datos
  
  profesionalCompleto.total <- datos %>% filter(str_detect(educ, "Profesional Completo"))
  profesionalCompleto.info <- select(profesionalCompleto.total,"numper","ytotcorh")
  
  postgradoIncompleto.total <- datos %>% filter(str_detect(educ, "Postgrado Incompleto"))
  postgradoIncompleto.info <- select(postgradoIncompleto.total,"numper","ytotcorh")
  
  postgradoCompleto.total <- datos %>% filter(str_detect(educ, "Postgrado Completo"))
  postgradoCompleto.info <- select(postgradoCompleto.total,"numper","ytotcorh")
  
  
  profesionalCompleto.info$perCapita <- 
    profesionalCompleto.info$ytotcorh / profesionalCompleto.info$numper
  
  postgradoIncompleto.info$perCapita <- 
    postgradoIncompleto.info$ytotcorh / postgradoIncompleto.info$numper
  
  postgradoCompleto.info$perCapita <- 
    postgradoCompleto.info$ytotcorh / postgradoCompleto.info$numper
  
  
  profesionalCompleto.perCapita <- select(profesionalCompleto.info,"perCapita")
  postgradoIncompleto.perCapita <- select(postgradoIncompleto.info,"perCapita")
  postgradoCompleto.perCapita <- select(postgradoCompleto.info,"perCapita")
  
  # Obtener muestras aleatorias originales
  
  set.seed(321)
  muestra.profesionalCompleto <-
    profesionalCompleto.perCapita[sample(1:nrow(profesionalCompleto.perCapita),
                                         150), ]
  
  set.seed(321)
  muestra.postradoIncompleto <- 
    postgradoIncompleto.perCapita[sample(1:nrow(postgradoIncompleto.perCapita), 
                                         150), ]
  set.seed(321)
  muestra.postradoCompleto <- 
    postgradoCompleto.perCapita[sample(1:nrow(postgradoCompleto.perCapita), 
                                       150), ]
  
  # Se genera un dataframe con las muestras que se requieren utilizar.
  #muestraProfesional <- data.frame(muestra.profesionalCompleto,muestra.postradoIncompleto,
  #                              muestra.postradoCompleto)
  ingresos <- c(muestra.profesionalCompleto, muestra.postradoIncompleto,  muestra.postradoCompleto)
  grado <- c(rep("Profesional Completo", length(muestra.profesionalCompleto)),
             rep("Postgrado Incompleto", length(muestra.postradoIncompleto)),
             rep("Postgrado Completo", length(muestra.profesionalCompleto)))
  
  datos_pregunta_3 <- data.frame(ingresos, grado)
  # Fijar nivel de significación.
  alfa <- 0.05
  
  # Comparar los diferentes algoritmos usando medias truncadas .
  cat (" Comparación entre grupos usando medias truncadas \n\n")
  gamma <- 0.2
  
  set.seed(999)
  
  medias_truncadas<-t1way(ingresos ~ grado , data = datos_pregunta_3, tr = gamma,
                          alpha = alfa )
  
  print(medias_truncadas)
  
  if( medias_truncadas $p.value < alfa ) {
  cat ("\ nProcedimiento post - hoc\n\n")
  set.seed(999)
    post_hoc <- lincon(ingresos ~ grado , data = datos_pregunta_3 , tr = gamma ,
                          alpha = alfa )
    print (post_hoc)
  }
  # Comparar los diferentes algoritmos usando bootstrap .
  cat(" Comparación entre grupos usando bootstrap \n\n")
  muestras <- 999
  set.seed(999)
  bootstrap<-t1waybt(ingresos ~ grado , data = datos_pregunta_3 , tr = gamma ,
                     nboot = muestras)
  #
  print(medias_truncadas)
  if(medias_truncadas $p.value < alfa ){
    cat ("\ nProcedimiento post - hoc\n\n")
    set.seed(666)
    post_hoc <- mcppb20(ingresos ~ grado , data = datos_pregunta_3 , tr = gamma ,
                        nboot = muestras)
    # 
    print(post_hoc)
  }
  
  
  
  