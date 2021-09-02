
# ENTREGA 2 _ ESTEBAN-LOPEZ-ALAMOS ----------------------------------------

#Fijar directorio
setwd("F:\Entretención\PUC\Diplomado PUC\EVALUACIONES\Evaluación 2 - Regresión logística")
library(skimr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(PerformanceAnalytics) # Analisis de Riesgo y Regresion
library(MASS)  # Para poder usar la funcion stepAIC
library(GGally)
library(nortest) #test estadisticos de regresion
library(lmtest) #para homocedasticidad
library(car) #para ejecutar influenceplot
library(ROCR)
library(pROC)
library(InformationValue)
#install.packages("ROCit")
library(ROCit)
library(DescTools)
#install.packages("MLmetrics")
library(MLmetrics)
library(broom)


# PREGUNTA A --------------------------------------------------------------
#Cargue los datos RRHH.csV

library(readr)
RRHH <- read.csv("F:/Entretención/PUC/Diplomado PUC/EVALUACIONES/Evaluación 2 - Regresión logística/rrhh.csv")

#Read.csv para que aparezcan las ? y el script en versi?n UTF-8

# PREGUNTA 1: VerifiCAR SINO codificar como factor las variables que corresponda

str(RRHH)
table(RRHH$Estado)
RRHH$Estado <- as.factor(RRHH$Estado) #Transformo a factor la variable (y)

table(RRHH$Estado)
table(RRHH$Estado.Civil)
RRHH$Estado.Civil <- factor(RRHH$Estado.Civil, 
                             labels = c("Divorciado", "Casado", "Separado",
                                        "Soltero", "Viuda"))

table(RRHH$Estado.Civil) #reviso si los valores corresponden a cuando no estaban codificados
str(RRHH)
library(DT)
DT::datatable(
  skim(RRHH) ,options = list(scrollX = TRUE))

# RESUMEN PREGUNTA A ------------------------------------------------------

#Se cargó la base, y se recodificaron Estado a factor (estaba en int) y Estado.Civil a factor porque estaba en int








# Pregunta B -----------------------------------------------------------------------

#Resumen exploratorio de los datos

skim(RRHH) #no existen missing
summary(RRHH) #no hay valores con incongruencias en los datos

#Búqueda de valores NA

dim(RRHH)
colMeans(is.na(RRHH))
sum(is.na(RRHH)) #no hay N.A
RRHH<-na.omit(RRHH) #no existen observaciones faltantes


#Outliers

#Utilizaremos la función quantile() para encontrar P25 P75 

Q <- quantile(RRHH$Edad, probs = c (.25, .75), na.rm = FALSE)

#la función IQR() que da la diferencia de los percentiles 75 y 25

iqr <- IQR(RRHH$Edad)

#Encontrar los rangos de corte más allá de los cuales todos los puntos de datos son valores atípicos.
sobre <- Q[2] + 1.5 * iqr # Rango superior
bajo <- Q[1] - 1.5 * iqr # Rango inferior
boxplot(RRHH$Edad)
eliminated <- subset(RRHH, RRHH$Edad > (Q[1] - 1.5*iqr) & RRHH$Edad < (Q[2]+1.5*iqr))
#La edad tiene 10 outliers

#------------------------------

#Utilizaremos la función quantile() para encontrar P25 P75 

Q1 <- quantile(RRHH$Ratio.Pago, probs = c (.25, .75), na.rm = FALSE)

#la función IQR() que da la diferencia de los percentiles 75 y 25

iqr2 <- IQR(RRHH$Ratio.Pago)

#Encontrar los rangos de corte más allá de los cuales todos los puntos de datos son valores atípicos.
sobre1 <- Q1[2] + 1.5 * iqr2 # Rango superior
bajo1 <- Q1[1] - 1.5 * iqr2 # Rango inferior
boxplot(RRHH$Ratio.Pago)
eliminated1 <- subset(RRHH, RRHH$Ratio.Pago > (Q1[1] - 1.5*iqr2) & RRHH$Ratio.Pago < (Q1[2]+1.5*iqr2))
#El ratio de pago no posee outliers 

#------------------------------

#Utilizaremos la función quantile() para encontrar P25 P75 

Q2 <- quantile(RRHH$Salario, probs = c (.25, .75), na.rm = FALSE)

#la función IQR() que da la diferencia de los percentiles 75 y 25

iqr3 <- IQR(RRHH$Salario)

#Encontrar los rangos de corte más allá de los cuales todos los puntos de datos son valores atípicos.
sobre2 <- Q2[2] + 1.5 * iqr3 # Rango superior
bajo2 <- Q2[1] - 1.5 * iqr3 # Rango inferior
boxplot(RRHH$Salario)
eliminated2 <- subset(RRHH, RRHH$Salario > (Q2[1] - 1.5*iqr3) & RRHH$Salario < (Q2[2]+1.5*iqr3))
#El salario posee 30 outliers 


#------------------------------

#Utilizaremos la función quantile() para encontrar P25 P75 

Q3 <- quantile(RRHH$Ausencias, probs = c (.25, .75), na.rm = FALSE)

#la función IQR() que da la diferencia de los percentiles 75 y 25

iqr4 <- IQR(RRHH$Ausencias)

#Encontrar los rangos de corte más allá de los cuales todos los puntos de datos son valores atípicos.
sobre3 <- Q3[2] + 1.5 * iqr4 # Rango superior
bajo3 <- Q3[1] - 1.5 * iqr4 # Rango inferior
boxplot(RRHH$Ausencias)
eliminated3 <- subset(RRHH, RRHH$Ausencias > (Q3[1] - 1.5*iqr4) & RRHH$Ausencias < (Q3[2]+1.5*iqr4))
#Las ausencias no poseen  


# RESUMEN PREGUNTA B ------------------------------------------------------

#No hay N.A ni tampoco incongruencias en los datos, si bien existen outliers estos no son propensos a sacarlos del estudio
#puesto que la base se reduciría en casi un 15% y se podría estar omitiendo información que, dado el contexto del estudio, puede ser relevante








# C.) ANOVA ---------------------------------------------------------------


#BOXPLOTS Y ANOVA

#¿como influyen las variables continuas con la variable de interes?

### Edad ###
# boxplot
RRHH %>%  ggplot(aes(x=Estado, y = Edad))+
  geom_boxplot()

anova(aov(RRHH$Edad ~ RRHH$Estado ))
# Con 10% de significancia, existen diferencias en la edad entre ser desvinculado o no

### Ratio.Pago ###
# boxplot
RRHH %>%  ggplot(aes(x=Estado, y = Ratio.Pago))+
  geom_boxplot()

anova(aov(RRHH$Ratio.Pago ~ RRHH$Estado ))
# Con 1% de significancia, existen diferencias en la medida de pago por hora entre ser desvinculado o no


### Salario ###
# boxplot
RRHH %>%  ggplot(aes(x=Estado, y = Salario))+
  geom_boxplot()

anova(aov(RRHH$Salario ~ RRHH$Estado ))
# Con 10% de significancia, no existen diferencias en el salario entre ser desvinculado o no


### Dias.trabajados ###
# boxplot
RRHH %>%  ggplot(aes(x=Estado, y = Dias.trabajados))+
  geom_boxplot()

anova(aov(RRHH$Dias.trabajados ~ RRHH$Estado ))
# Con 1% de significancia, existen diferencias en los dias trabajados entre ser desvinculado o no


### Ausencias ###
# boxplot
RRHH %>%  ggplot(aes(x=Estado, y = Ausencias))+
  geom_boxplot()

anova(aov(RRHH$Ausencias ~ RRHH$Estado ))
# Con 10% de significancia, no existen diferencias en el número de ausencias entre ser desvinculado o no

# RESUMEN PREGUNTA c ------------------------------------------------------

#Las variables cuantitativas que pueden ser significativas a la hora de evaluar la desvinculación de los empleados de la empresa son:
# 1% de error tipo I: Dias trabajados, Ratio de pago
# 10% de error tipo I: Edad
# y las variables que no impactan sobre el Estado son las ausencias y el salario







# PREGUNTA D) -------------------------------------------------------------

#Estado y Sexo
chisq.test(RRHH$Estado,RRHH$Sexo) # independientes con 10% de significancia
mosaicplot(~Estado + Sexo, data = RRHH, shade = T) 

#Estado y Estado Civil
chisq.test(RRHH$Estado,RRHH$Estado.Civil) # dependientes con 10% de significancia
mosaicplot(~Estado + Estado.Civil, data = RRHH, shade = T)

#Estado y Departamento
chisq.test(RRHH$Estado,RRHH$Departamento) # dependientes con 1% de significancia
mosaicplot(~Estado + Departamento, data = RRHH, shade = T)

#Estado y Posición
chisq.test(RRHH$Estado,RRHH$Posicion) # dependientes con 5% de significancia
mosaicplot(~Estado + Posicion, data = RRHH, shade = T)

#Estado y Desempeño
chisq.test(RRHH$Estado,RRHH$Desempeno) # dependientes con 5% de significancia
mosaicplot(~Estado + Desempeno, data = RRHH, shade = T)


# RESUMEN PREGUNTA D ------------------------------------------------------

#Las variables categóricas que pueden ser significativas a la hora de evaluar la desvinculación de los empleados de la empresa son:
# 1% de error tipo I: Departamento
# 5% de error tipo I: Desempeño, Posición
# 10% de error tipo I: Estado Civil
# y la variable que no impacta sobre el Estado es el sexo



# E TRATAMIENTO Y CONTROL ----------------------------------------------------------

set.seed(2021)
ind_train <- sample(1:nrow(RRHH), size = 0.75*nrow(RRHH), replace = FALSE)

RRHH_train <- RRHH %>% 
  slice(ind_train)

RRHH_test <- RRHH %>% 
  slice(-ind_train)



# F.- MODELO LOGÍSTICO ------------------------------------------------------

summary(RRHH$Estado)
#1 es desvinculado, sobre esa referencia se interpretarán los resultados
modelo1 <- glm(Estado ~ Edad + Desempeno, data = RRHH_train,
               family = binomial(link = "logit"))
summary(modelo1)


# G.- ODD RATIOS DE LETRA F) --------------------------------------------------

coef(modelo1)  #Betas

#obtener odd-ratios
library(broom)
tidy(modelo1) %>% mutate(OR = exp(estimate))

#El riesgo de quedar desempleado es un 5% (OR) mayor si aumenta la edad en un año. Factor de riesgo

#Si el desempeño es excedido, el riesgo de quedar desempleado baja un 75% (1-0,25). Factor protector

#Si el desempeño es excepcional, el riesgo de quedar desempleado baja en un 99,9%. Factor protector

#Si el desempeño es fully meats, el riesgo de quedar desempleado disminuye 54,4%. Factor protector

#El riesgo de quedar desempleado es un 49% mayor si su desempeño es N.A / too early to review. Factor de riesgo

#El riesgo de quedar desempleado es un 20,3% menor si su desempeño necesita mejorar. Factor protector 

#El riesgo de quedar desempleado es un 33,9% menor si su desempeño es PIP. Factor protector



# H. AKAIKE ---------------------------------------------------------------



todo_el_modelo<-glm(Estado ~ ., data = RRHH_train, family = binomial(link = "logit")) 
summary(todo_el_modelo)
#Ahora se eliminan las variables que no aporten con método backward

modelo_backward <- step(todo_el_modelo, birection = "backward")

# RESPUESTA: El modelo óptimo es Estado ~ Edad+Ratio.Pago+Dias.trabajados ya que entrega menor AIC



# I. Predicción -----------------------------------------------------------

prediccion <- data.frame(
  Edad = 34,
  Ratio.Pago = 34.95,
  Salario = 3345.2,
  Dias.trabajados = 3247,
  Ausencias = 16,
  Sexo = "Female",
  Estado.Civil = "Casado",
  Departamento = "Admin Offices",
  Posicion = "Sr. Accountant",
  Desempeno = "Fully Meets"
)

str(RRHH)
predict.glm(modelo_backward, prediccion, type ="response")

#La probabilidad de ser desvinculado es de un 0,42%






# J.) PROBABILIDADES ------------------------------------------------------

#dos formas para obtener las probabilidades: 

predictedscores<-plogis(predict(modelo_backward, RRHH_test))
summary(predictedscores)

predictedscores2<-predict(modelo_backward, RRHH_test, type="response")







# K.) MATRIZ CONFUSION ----------------------------------------------------
plotROC(actuals = RRHH_test$Estado, predictedScores = predictedscores,returnSensitivityMat = TRUE )


#Se definen los puntos de corte
values<-plotROC(actuals = RRHH_test$Estado, predictedScores = predictedscores,returnSensitivityMat = TRUE )

corte<-min(values[values$One_minus_specificity<0.25, 'Threshold'])

#Se obtiene la matriz de confusión:
confusionMatrix(RRHH_test$Estado, predictedscores, threshold = corte)

#.- Sensibilidad 
sensitivity(RRHH_test$Estado, predictedscores, threshold = corte) 
#Los casos positivos se representan en un 68,75%

#.- Especificidad 
specificity(RRHH_test$Estado, predictedscores, threshold = corte) 
#Los casos positivos se representan en un 78,26%

#.- Precisión
precision(RRHH_test$Estado, predictedscores, threshold = corte) 
#Los casos estan clasificados correctamente en un 68,75%






# L.) ÚLTIMA PREGUNTA -----------------------------------------------------

#.- Área bajo la Curva ROC
#InformationValue::plotROC(actuals = RRHH_test$Estado, predictedScores = predictedscores)

plotROC(actuals = RRHH_test$Estado, predictedScores = predictedscores) #ÁREA bajo la curva
#Bajor de área es 0,7785, Valores entre el 75 % y 90% son un buen ajuste, por ende el modelo discrimina de buena forma los datos

#.- Test de Kolmogorov - Smirnov

ks <- ksplot(rocit(score = predictedscores, class = RRHH_test$Estado))
ks$`KS stat` #Discrimina de forma regular
#En este caso tenemos un KS stat de 0,513 lo que significa que nuestro ajuste es bueno.

#.- Test de Hosmer - Lemeshow

DescTools::HosmerLemeshowTest(fit = predictedscores, obs = RRHH_test$Estado)

#Como tenemos un p-valor cercano a 0, se rechaza la hipótesis nula, por lo tanto tenemos diferencia entre
#los valores observados y predichos en al menos un grupo.


# FIN ---------------------------------------------------------------------

