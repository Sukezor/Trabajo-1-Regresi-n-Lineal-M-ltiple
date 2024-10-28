#--------------------------------------------------------------------------------------------------

#Definicion de variables

#Variables Cualitativas

#Marca, Modelo, Procesador, RAM, Storage, TarjetaGrafica, SistemaOperativo, warranty..years

#Variables cualitativas

#Screenzise, weight, battery, price

#Var respuesta= Precio (Price)

colnames(LLL) <- c("Marca", "Modelo", "Procesador", "RAM", "Almacenamiento",
                   "Pantalla", "TarjetaGrafica", "SistemaOperativo", "Peso",
                   "Bateria", "Precio", "Garantia")

LLL$Marca <- as.factor(LLL$Marca)
LLL$Procesador <- as.factor(LLL$Procesador)
LLL$RAM <- as.factor(LLL$RAM)
LLL$Almacenamiento <- as.factor(LLL$Almacenamiento)
LLL$TarjetaGrafica <- as.factor(LLL$TarjetaGrafica)
LLL$SistemaOperativo <- as.factor(LLL$SistemaOperativo)
LLL$Garantia <- as.factor(LLL$Garantia)
LLL$Pantalla <- as.factor(LLL$Pantalla)


#Categorizacion de variables
#No tengo en cuenta el Modelo, solo es una identificacion

summary(LLL)

#Creo un subset de las variables cuantitativas

var_cuanti <- subset(LLL, select= c( 'Peso', 'Bateria', 'Precio'))



m <- lm(Precio ~ . - Modelo, data= LLL)
summary(m)


#---------------------------------------------------------------------------

library(car)
library(GGally)

m1 <- lm(Precio ~ . , data= var_cuanti)
summary(m1)
vif(m1)

ggpairs(var_cuanti)

#Del Modelo m1 de variables cuantitativas vemos que sus variables peso y bateria no son significativas 

#No hay casi correlacion entre estas variables cuantitativas
#lAS VARIABLES CUANTITATIVAS NO NOS GENERAN PROBLEMAS EN EL VIF, no son mayores a 5
#Podemos concluir que no hay MULTICOLINEALIDAD
#Ahora analizamos las variables cualitativas atraves de graficas para ver su coherencia


library(ggplot2)

G1 <- ggplot(LLL, aes(x = Marca, y = Precio)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")
G2 <- ggplot(LLL, aes(x = Procesador, y = Precio)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")
G3 <- ggplot(LLL, aes(x = RAM, y = Precio)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")
G4 <- ggplot(LLL, aes(x = Almacenamiento, y = Precio)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")
G5 <- ggplot(LLL, aes(x = TarjetaGrafica, y = Precio)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")
G6 <- ggplot(LLL, aes(x = SistemaOperativo, y = Precio)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")
G7 <- ggplot(LLL, aes(x = Garantia, y = Precio)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")
G8 <- ggplot(LLL, aes(x = Pantalla, y = Precio)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")


#Las variables que vamos a comparar con nuestra variable respuesta son:
G1 #Marca
G2 #Procesador
G3 #RAM
G4 #Storage
G5 #Graphics Card
G6 #Operating System
G7 #Warranty Years 
G8 #Screen size 

#Al analizar las graficas podemos decir que nos falta informacion para concluir algo debido a que
#Para que haya una variacion significativa se tienen que relacionar mas de las otras variables regresoras
#Con la respuesta

#"Comparación de Precio según la RAM y el Almacenamiento"
G9 <- ggplot(LLL, aes(x = RAM, y = Precio)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(fill = "dodgerblue1", colour = "black", alpha = 0.5, outlier.colour = "tomato2") +
  labs(title = "Comparación de Precio según la RAM y el Almacenamiento",
       x = "RAM (GB)",
       y = "Precio") +
  facet_wrap(~ Almacenamiento) +
  theme_minimal()
G9

#Comparación de Precio según la Pantalla y la Garantia"
G10 <- ggplot(LLL, aes(x = Pantalla, y = Precio)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(fill = "dodgerblue1", colour = "black", alpha = 0.5, outlier.colour = "tomato2") +
  labs(title = "Comparación de Precio según la Pantalla y la Garantia",
       x = "RAM (GB)",
       y = "Precio") +
  facet_wrap(~ Garantia) +
  theme_minimal()
G10


#---------------------------------------------------------------
#Puntos de influencia y outliers

#Aqui decidi tomar un Primer Modelo ajustado sobre el que voy a trabajar


modelo1<- lm(formula = Precio ~ Garantia, data = LLL)
summary(modelo1)

#Veo los puntos de influencia y outliers por medio de la distancia cook, DFITS y DFbeta

# Calcular la Distancia de Cook
cooks_distances <- cooks.distance(modelo1)

# Identificar puntos influyentes
puntos_influyentes <- which(cooks_distances > (4 / nrow(LLL)))

# Mostrar los puntos influyentes
print(puntos_influyentes)

# Eliminar los puntos influyentes del conjunto de datos
datos_sin_influencia <- LLL[-puntos_influyentes, ]

#Validacion

LLL$residuales <- rstandard(modelo1)


#NORMALIDAD
library(tseries)
library(nortest)
library(ggplot2)
library(car)

shapiro.test(modelo1$residuals)
ad.test(modelo1$residuals)
jarque.bera.test(modelo1$residuals)

ggplot(LLL, aes(x=rees))+
  geom_histogram(fill = "dodgerblue1")

qqPlot(modelo1$residuals, xlab = 'Cuantiles de distribucion normal',
       ylab = 'Cuantiles de residuales', pch = 16, col = "dodgerblue1",
       col.lines = "red")
# si esta entre banda de confianza, tiene normalidad 

#NO ES normal


#HOMOCEDASTICIDAD

library(lmtest)
bptest(modelo1)

LLL$Valores_Ajustados <- modelo1$fitted.values

ggplot(LLL, aes(x=Valores_Ajustados, y=rees)) + 
  geom_point(color= 'black', size= 2)+
  geom_hline(yintercept = c(-3.5,0,3.5), linetype= 'dotted',
             color="blue", size= 1)

# ES HOMOCEDASTICO

#INDEPENDENCIA

library(lmtest)
bgtest(modelo1)
dwtest(modelo1)

ggplot(LLL, aes(x=Precio, y=rees)) + 
  geom_point(color= 'black', size= 2)+
  geom_hline(yintercept = c(-3.5,0,3.5), linetype= 'dotted',
             color="blue", size= 1)

# NO ES independiente, Las pruebas estadisticas y graficas se contradicen.




#Residuales estandarizados
rees <- rstandard(m)

plot(LLL$Precio, LLL$rees) +
  abline(h=0)

ggplot(data = LLL, aes(Precio, rees)) + geom_point() +
  geom_smooth(method = lm, color= "blue", fill= "black", se= TRUE)


options(max.print=100000)
influence.measures(modelo1)




# 4. Seleccion de variables
#seleccion de variables 
modb <- step(modelo1, trace = T, direction = "backward")
modb

modf<-step(modelo1, trace = T, direction = "both")
modf


set.seed(123)

# 5. validación - trainig y test

#calcular muestra
sample <- sample.int(n= nrow(LLL), size = floor(0.8*nrow(LLL)), replace = F)

#muestra de entrenamiento 
train <- LLL[sample, ]

test <- LLL[-sample, ]

Modelotraining <- lm(formula= Precio ~ Garantia, data= train)
summary(Modelotraining)


Modelotesting <- lm(formula = Precio ~ Garantia, data = test)
summary(Modelotesting)

prediccion <- predict.lm(Modelotesting, data= test[,c('Garantia')])
summary(prediccion)

plot(test$Precio , prediccion)


library(Metrics)
metricas <- c(mae(test$Precio , prediccion),
              mape(test$Precio , prediccion),
              mse(test$Precio , prediccion),
              rmse(test$Precio , prediccion),
              AIC(Modelotesting),
              BIC(Modelotesting),
              summary(Modelotesting)$r.squared)
#R*2= mayor mejor
#Estos que acabamos de ver mientras mas pequeños mejor, y lo minimo que me puede dar es 0
#R*2, MSE, RMSE, MAE, MAPE

names(metricas) <- c('MAE', 'MAPE', 'MSE', 'RMSE', 'AIC', 'BIC', 'R2')
metricas



#-------------------------------------------------------------------------------
#INTENTO DE ESTE ULTIMO PASO CON OTRO Modelo


#Analisis de los otros modelos

#Modelo 2

modelo2<- lm(formula = Precio ~ Bateria + Peso + Garantia, data = LLL)
summary(modelo2)

#Validacion modelo 2

LLL$residuales <- rstandard(modelo2)


#NORMALIDAD
library(tseries)
library(nortest)
library(ggplot2)
library(car)

shapiro.test(modelo2$residuals)
ad.test(modelo2$residuals)
jarque.bera.test(modelo2$residuals)

qqPlot(modelo2$residuals, xlab = 'Cuantiles de distribucion normal',
       ylab = 'Cuantiles de residuales', pch = 16, col = "dodgerblue1",
       col.lines = "red")
# si esta entre banda de confianza, tiene normalidad 

#NO ES normal


#HOMOCEDASTICIDAD

library(lmtest)
bptest(modelo2)

LLL$residuos_estandarizados <- rstandard(modelo2)
LLL$valores_ajustados <- modelo2$fitted.values

# Crear el gráfico
ggplot(LLL, aes(x = valores_ajustados, y = residuos_estandarizados)) +
  geom_point(color = "dodgerblue") +      # Puntos de los residuos
  geom_hline(yintercept = c(-3.5,3.5), linetype = "dashed", color = "red") + # Línea en 0
  labs(x = "Valores Ajustados", y = "Residuos Estandarizados") +
  ggtitle("Gráfico de Residuos vs Valores Ajustados") +
  theme_minimal()

# NO ES HOMOCEDASTICO

#INDEPENDENCIA

library(lmtest)
bgtest(modelo2)
dwtest(modelo2)

ggplot(data = data.frame(index = seq_along(residuals(modelo2)), residuals = residuals(modelo2)), 
       aes(x = index, y = residuals)) +
  geom_point(color = "dodgerblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Índice de observación", y = "Residuos", 
       title = "Gráfico de dispersión de residuos vs índice de observación") +
  theme_minimal()

# NO ES independiente, Las pruebas estadisticas y graficas se contradicen.


#Despues analizar los outliers

cooks_distances <- cooks.distance(modelo2)
puntos_influyentes <- which(cooks_distances > (4 / nrow(LLL)))
print(puntos_influyentes)

LLL2 <- LLL[-puntos_influyentes, ]

#Volvamos a verificar si el modelo ya es valido

modelo22<- lm(formula = Precio ~ Bateria + Peso + Garantia, data = LLL2)
summary(modelo22)

#Validacion modelo 2 sin puntos de influencia

LLL2$residuales <- rstandard(modelo22)


#NORMALIDAD
library(tseries)
library(nortest)
library(ggplot2)
library(car)

shapiro.test(modelo22$residuals)
ad.test(modelo22$residuals)
jarque.bera.test(modelo22$residuals)

qqPlot(modelo22$residuals, xlab = 'Cuantiles de distribucion normal',
       ylab = 'Cuantiles de residuales', pch = 16, col = "dodgerblue1",
       col.lines = "red")
# si esta entre banda de confianza, tiene normalidad 

#NO ES normal


#HOMOCEDASTICIDAD

library(lmtest)
bptest(modelo22)

LLL2$residuos_estandarizados <- rstandard(modelo22)
LLL2$valores_ajustados <- modelo22$fitted.values

# Crear el gráfico
ggplot(LLL2, aes(x = valores_ajustados, y = residuos_estandarizados)) +
  geom_point(color = "dodgerblue") +      # Puntos de los residuos
  geom_hline(yintercept = c(-3.5,3.5), linetype = "dashed", color = "red") + # Línea en 0
  labs(x = "Valores Ajustados", y = "Residuos Estandarizados") +
  ggtitle("Gráfico de Residuos vs Valores Ajustados") +
  theme_minimal()

# NO ES HOMOCEDASTICO

#INDEPENDENCIA

library(lmtest)
bgtest(modelo22)
dwtest(modelo22)

ggplot(data = data.frame(index = seq_along(residuals(modelo22)), residuals = residuals(modelo22)), 
       aes(x = index, y = residuals)) +
  geom_point(color = "dodgerblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Índice de observación", y = "Residuos", 
       title = "Gráfico de dispersión de residuos vs índice de observación") +
  theme_minimal()








#Modelo 3


modelo3<- lm(formula = Precio ~ Bateria + Peso + Garantia, data = LLL)
summary(m3)


#Validacion modelo 3

LLL$residuales <- rstandard(modelo3)


#NORMALIDAD
library(tseries)
library(nortest)
library(ggplot2)
library(car)

shapiro.test(modelo3$residuals)
ad.test(modelo3$residuals)
jarque.bera.test(modelo3$residuals)

qqPlot(modelo3$residuals, xlab = 'Cuantiles de distribucion normal',
       ylab = 'Cuantiles de residuales', pch = 16, col = "dodgerblue1",
       col.lines = "red")
# si esta entre banda de confianza, tiene normalidad 

#NO ES normal


#HOMOCEDASTICIDAD

library(lmtest)
bptest(modelo3)

LLL$residuos_estandarizados <- rstandard(modelo3)
LLL$valores_ajustados <- modelo3$fitted.values

# Crear el gráfico
ggplot(LLL, aes(x = valores_ajustados, y = residuos_estandarizados)) +
  geom_point(color = "dodgerblue") +      # Puntos de los residuos
  geom_hline(yintercept = c(-3.5,3.5), linetype = "dashed", color = "red") + # Línea en 0
  labs(x = "Valores Ajustados", y = "Residuos Estandarizados") +
  ggtitle("Gráfico de Residuos vs Valores Ajustados") +
  theme_minimal()

# NO ES HOMOCEDASTICO

#INDEPENDENCIA

library(lmtest)
bgtest(modelo3)
dwtest(modelo3)

ggplot(data = data.frame(index = seq_along(residuals(modelo3)), residuals = residuals(modelo3)), 
       aes(x = index, y = residuals)) +
  geom_point(color = "dodgerblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Índice de observación", y = "Residuos", 
       title = "Gráfico de dispersión de residuos vs índice de observación") +
  theme_minimal()

# NO ES independiente, Las pruebas estadisticas y graficas se contradicen.


#Despues analizar los outliers

cooks_distances <- cooks.distance(modelo3)
puntos_influyentes <- which(cooks_distances > (4 / nrow(LLL)))
print(puntos_influyentes)

LLL3 <- LLL[-puntos_influyentes, ]

#Volvamos a verificar si el modelo ya es valido

modelo33<- lm(formula = Precio ~ Bateria + Peso + Garantia, data = LLL3)
summary(modelo33)

#Validacion modelo 2 sin puntos de influencia

LLL3$residuales <- rstandard(modelo33)


#NORMALIDAD
library(tseries)
library(nortest)
library(ggplot2)
library(car)

shapiro.test(modelo33$residuals)
ad.test(modelo33$residuals)
jarque.bera.test(modelo33$residuals)

qqPlot(modelo33$residuals, xlab = 'Cuantiles de distribucion normal',
       ylab = 'Cuantiles de residuales', pch = 16, col = "dodgerblue1",
       col.lines = "red")
# si esta entre banda de confianza, tiene normalidad 

#NO ES normal


#HOMOCEDASTICIDAD

library(lmtest)
bptest(modelo33)

LLL3$residuos_estandarizados <- rstandard(modelo33)
LLL3$valores_ajustados <- modelo33$fitted.values

# Crear el gráfico
ggplot(LLL3, aes(x = valores_ajustados, y = residuos_estandarizados)) +
  geom_point(color = "dodgerblue") +      # Puntos de los residuos
  geom_hline(yintercept = c(-3.5,3.5), linetype = "dashed", color = "red") + # Línea en 0
  labs(x = "Valores Ajustados", y = "Residuos Estandarizados") +
  ggtitle("Gráfico de Residuos vs Valores Ajustados") +
  theme_minimal()

# NO ES HOMOCEDASTICO

#INDEPENDENCIA

library(lmtest)
bgtest(modelo33)
dwtest(modelo33)

ggplot(data = data.frame(index = seq_along(residuals(modelo33)), residuals = residuals(modelo33)), 
       aes(x = index, y = residuals)) +
  geom_point(color = "dodgerblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Índice de observación", y = "Residuos", 
       title = "Gráfico de dispersión de residuos vs índice de observación") +
  theme_minimal()


