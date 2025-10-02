#########################################################################
#########------- Scotiabank 3.0 desde cubo---#################
#########################################################################

# Autor: Yesin Camarena Pachas
# Correo: yesin.camarena@gmail.com
# Despliegue de Modelos
# version: 2.0
#################################################################
##### (1) DESARROLLO DE ALGORITMOS DE MACHINE LEARNING ##########
#################################################################


#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls()) ; dev.off()


#---------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

#####################################
####### 0. Librerias a utilizar #####
##################################### 


library(MASS)    # Modelos Logisticos, Intervalos confianza
library(pROC)    # Curva Roc-Auc
library(foreign) # Lectura de archivos en otros formatos: SPSS
library(gmodels) # Graficos y modelos
library(InformationValue) # Importancia de variables, WOESS
library(caTools) # Herramientas para indicadores de validacion de modelos
library(caret)   # Libreria de Machine Learning
library(ggplot2) # Graficas
library(MLmetrics) # Herramientas para indicadores de validacion de modelos
library(ISLR)      # Libreria del libro de los creadores de Rf
library(readxl)

library(mstrio)

##############################################################################
##### 1. Lectura de los datos (Training + testing) desde excel #######
##############################################################################

# Servidor 1

base_url <- "http://185.184.68.42:8080/MicroStrategyLibrary/api" 
username <- "administrator"
password <- "12345"
project_name <- "RRHH-Scotiabank"
conn <- connect_mstr(base_url=base_url, username=username, password=password, project_name=project_name)
my_cube <- Cube$new(connection=conn, cube_id="E48F52774B831AD2035DC3B6A059B83A", parallel=FALSE)
df2 <- my_cube$to_dataframe()
names(df2)



View(df2)

library(readxl)
# library(openxlsx)
# write.xlsx(df2, file = "RRHH-Scotiabank.xlsx", 
#            sheetName="MTCARS", append=TRUE)

write.csv(df2,"RRHH-Scotiabank.csv")
library(readr)
options(scipen=999)#deshabilitar notaciÃ³n cientÃ­fica para evitar errores de igv

df3 <- read_csv("RRHH-Scotiabank.csv")

# datos_n <-read.spss("Churn-nuevos-arboles.sav",
#                     use.value.labels=TRUE, 
#                     to.data.frame=TRUE)

#df2 <- read_excel("DataDummy - trabajada - v2.xlsx")
#Cargando los datos a trabajar 
#df2 <- read_excel("Churn_Modelling.xlsx")
#df2 <- read_excel("DataDummy5.xlsx")
#Convirtiendo a data.frame
datos <- as.data.frame(df2)
#dimension de los datos
dim(datos)
#longitud de los datos
length(datos)
#omitiendo na
datosr<-na.omit(datos)
#estructura de los datos
str(datos)
#nombre de las cabeceras
View(datos)





#eliminando la columnas


datos$'Persona_DESC'<-NULL
datos$`Lima-Provincia`<- NULL
datos$`Ubicaciión Trabajo`<- NULL
datos$`Departamento Trabajo`<- NULL
datos$`Ubicación Casa`<- NULL
datos$`Departmento Casa`<- NULL
datos$'Código Líder'<-NULL
datos$`Fecha Contratación`<-NULL
datos$`Fecha Nacimiento Líder`<-NULL
datos$FY<-NULL
datos$`Fecha Retiro`<-NULL
datos$'Persona_Synonym 1'<-NULL
datos$Líder_ID<-NULL
datos$Líder_DESC<-NULL 


str(datos)
names(datos)
# datos[,14:16]<- NULL

#####################################
# 2. Pre-procesamiento de los datos #
#####################################

# Tablas resumen - Analisis univariado de la informacion
# Opcion 01
library(mlr)#modelos de clasificacion regresion , cv
summarizeColumns(datos.r) # tabla mas completa

Resumen_Datos<-summarizeColumns(datos.r) # Guardo en un objeto
write.csv(Resumen_Datos,"Resumen_1.csv") # Escribir o sacar un objeto de R

# Opcion 02
library(funModeling)
#Data Cleaning
#Variable importance analysis
#Assessing model performance


write.csv(df_status(datos), "Resumen_2.csv", row.names = F)


#Comentarios de la data


#----------------------------------------------------------
# Verificacion de datos perdidos
library(DataExplorer)
plot_missing(datos1)


# Graficar la cantidad de valores perdidos
library(VIM)
graf_perdidos1 <- aggr(datos.r,prop = F, 
                       numbers = TRUE,
                       sortVars=T,
                       cex.axis=0.5)

summary(graf_perdidos1)

# Matriz de datos x variables perdidas
matrixplot(datos.r,
           main="Matrix Plot con Valores Perdidos",
           cex.axis = 0.6,
           ylab = "registro")


#datos numericos
library(psych)
multi.hist(x = datos[,4:11], dcol = c("blue", "red"), dlty = c("dotted", "solid"), 
           main = "")

######################################
#2.1  Recodificacion de Variables
######################################
#recodificando a training y test
# convert <- c(2:5, 7:9,11,16:17)
# read_file[,convert] <- data.frame(apply(read_file[convert], 2, as.factor))


datos.r<-datos

# usando el paquete dummiesa Geography
#  library(dummies)
#  datos.r <- dummy.data.frame(datos,names=c("Location_Job"))
#  names(datos.r)
# datos.r$Location_JobABANCAY <- as.factor(datos.r$Location_JobABANCAY)
# datos.r$Location_JobATE VITARTE <- as.factor(datos.r$Location_JobATE VITARTE)
# datos.r$(Location_JobATE VITARTE)
# datos.r$GeographySpain <- as.factor(datos.r$GeographySpain)



#  Recodificando Geography 
# datos$Geography=ifelse(datos$Geography=="France",0,
#                          ifelse(datos$Geography=="Germany",1,  
#                                 ifelse(datos$Geography=="Spain",2,datos$Geography)))      
# datos$Geography <- as.factor(datos$Geography)

#Recodificando Region
datos.r$`Región Trabajo`=ifelse(datos.r$`Región Trabajo`=="CENTRO",0,
                      ifelse(datos.r$`Región Trabajo`=="LIMA",1,
                             ifelse(datos.r$`Región Trabajo`=="NORTE",2,
                                    ifelse(datos.r$`Región Trabajo`=="SUR",3,datos.r$`Región Trabajo`))))
datos.r$`Región Trabajo`<-as.factor(datos.r$`Región Trabajo`)


#Recodificando Gender
datos.r$Género=ifelse(datos.r$Género=="M",1,0)
datos.r$Género <- as.factor(datos.r$Género)


#Recodificando Civil  Status
datos.r$`Estado Civil`=ifelse(datos.r$`Estado Civil`=='CASADO (A)',0,
                              ifelse(datos.r$`Estado Civil`=='CONVIVIENTE',1,  
                                     ifelse(datos.r$`Estado Civil`=='DIVORCIADO (A)',2,
                                            ifelse(datos.r$`Estado Civil`=='SEPARADO',3 ,   
                                                   ifelse(datos.r$`Estado Civil`=='SOLTERO (A)',4,datos.r$`Estado Civil`))))) 
datos.r$`Estado Civil` <- as.factor(datos.r$`Estado Civil`)




#Recodificando Generation
datos.r$Generación=ifelse(datos.r$Generación=='GENERACION X',0,
                          ifelse(datos.r$Generación=='MILLENNIALS JR',1,  
                                 ifelse(datos.r$Generación=='MILLENNIALS SR',2,datos.r$Generación)))
datos.r$Generación <- as.factor(datos.r$Generación)


#Recodificando EDD_Category ¿tiene EDD?
datos.r$'¿Tiene EDD?'=ifelse(datos.r$'¿Tiene EDD?'=="No",0,1)
datos.r$'¿Tiene EDD?' <- as.factor(datos.r$'¿Tiene EDD?')


#Recodificando Children colaborador ¿tiene hijos?

datos.r$`¿Tiene hijos?`=ifelse(datos.r$`¿Tiene hijos?`=="No",0,1)
datos.r$`¿Tiene hijos?` <- as.factor(datos.r$`¿Tiene hijos?`)

#Recodificando Leader_Gender
datos.r$`Género Líder`=ifelse(datos.r$`Género Líder`=="M",1,0)
datos.r$`Género Líder` <- as.factor(datos.r$`Género Líder`)

#Recodificando Leader_union

datos.r$`Líder Union` <- ifelse(datos.r$`Líder Union`=="No",0,1)
datos.r$`Líder Union`<- as.factor(datos.r$`Líder Union`)

#Recodificando Leader_hijo ¿Lider tiene hijos?
datos.r$'¿Líder tiene hijos?'<- ifelse(datos.r$'¿Líder tiene hijos?'=="No",0,1)
datos.r$'¿Líder tiene hijos?'<- as.factor(datos.r$'¿Líder tiene hijos?')



#Recodificando como factor

datos.r$FTE<-as.factor(datos.r$FTE)
datos.r$`Situación Laboral`<-as.factor(datos.r$`Situación Laboral`)
datos.r$EDD<-as.factor(datos.r$EDD)
datos.r$`Líder EDD`<-as.factor(datos.r$`Líder EDD`)
datos.r$`Satisfacción Laboral`<-as.factor(datos.r$`Satisfacción Laboral`)
datos.r$`Tasa Ausentismo`<-as.factor(datos.r$`Tasa Ausentismo`)
#Recodificando  Como numerico E integer

datos.r$`Año Fiscal Canadiense`<-as.integer(datos.r$`Año Fiscal Canadiense`)
datos.r$Capacitaciones<-as.integer(datos$Capacitaciones)
datos.r$Edad<-as.integer(datos.r$Edad)
datos.r$`Edad Líder`<-as.integer(datos.r$`Edad Líder`)




table(datos.r$`Líder EDD`)


datos.r$`Antigüedad Cargo Años`<-as.numeric(datos$`Antigüedad Cargo Años`)


table(datos.r$Leader_Union)


summary(datos.r)
#Targets para 2020 ,por definir como numerico (regresion o factor)

# datos.r$Retiro <- as.factor(datos.r$Retiro)
# datos.r$Retirocod <- as.factor(datos.r$Retirocod)



# 2.Satisfaccion_Laboral
# 3.Tasa_Ausentismo
# 4.Productividad_x_objetivos
# 5.Productividad_x_transacciones
# 6.Productividad_x_ventas
# 7.Eficiencia_horas_trabajadas
# 8.Eficiencia_errores_operativos


names(datos.r)


#creando dataset para el 1er target Satisfaccion laboral,y eliminando los demas target:
# 1.Situacion_laboral
datos1<-datos.r
# datos1$`Satisfacción Laboral`<-NULL
# datos1$`Tasa Ausentismo`<-NULL
# datos1$`Productividad Promedio`<-NULL
# datos1$`Eficiencia Promedio`<-NULL
# View(datos1)


#cambiando las cabeceras al datos1



library(psych)
pairs.panels(datos.r[1:10])
pairs.panels(datos.r[10:20])
pairs.panels(datos.r[40:56])


#datos numericos
library(psych)
multi.hist(x = datos.r[,50:56], dcol = c("blue", "red"), dlty = c("dotted", "solid"), 
           main = "")

View(datos)
str(datos.r)
names(datos)
##########################################
# 2.2 Valores Nulos, Missings o Faltantes #
##########################################
#Datos de entrenamiento 2018,2019
#Datos de testing 2020
datos1[datos1$`Año Fiscal Canadiense`<2020, ] 

#datos1[datos1$x>2]
dim(datos1)
str(datos1)

write.csv(datos1,"datos_1er_target_satisfaccion_laboral.csv")
datos_1er <- read_csv("datos_1er_target_satisfaccion_laboral.csv")

View(datos_1er)




utils::View(datos1)
datos1$X1<-NULL
#----------------------------------------------------------------
# Opcion 1 - Parametrica , Univariada o por Criterio de Negocio
# Utilizamos la libreria Mlr para esto.
library(mlr)
options(warn=-1)
datosD_Imp <- mlr::impute(datos_1er,classes = list(factor = imputeMode(),  # Cualitativas por moda
                                                  integer = imputeMedian(), # Cuantitativas por media
                                                  numeric = imputeMedian()),
                                 dummy.classes = c("integer","factor"), 
                                              dummy.type = "numeric")


class(datos1)


data_parametrica <- impute(datos1, classes = list(factor = imputeMode(), 
                                              integer = imputeMedian(),
                                              numeric = imputeMedian()),
                           dummy.classes = c("integer","factor"), dummy.type = "numeric")

bd1=data_parametrica$data[,1:min(dim(datos1))]

datosD_Imp=datosD_Imp$data[,1:min(dim(datos1))]

summary(datosD_Imp)

plot_missing(datosD_Imp)
View(datosD_Imp)

# Opcion 2 - Sofisticada o por Machine Learning
# Imputando los valores perdidos cuantitativos usando k-nn
# y estandarizando las variables numericas
# 
library(caret)
preProcValues1 <- caret::preProcess(datosD_Imp,
                                    method=c("knnImpute","center","scale"))


preProcValues1 <- caret::preProcess(datos1,
                                    method=c("knnImpute","center","scale"))

preProcValues1
datosD_Imp2 <- predict(preProcValues1, datosD_Imp)
View(datosD_Imp2)
# Otras opciones: range , bagImpute, medianImpute

#####################################
# 3. Seleccion de Drivers ###########
#####################################
library("scorecard")
#sea más fácil y eficiente al proporcionar funciones 
#para algunas tareas comunes, como la partición de datos, 
#la selección de variables, la distribución de problemas, 
#la escala del cuadro de mando, la evaluación del rendimiento 
#y la generación de informes


###### Utilizando Boruta ########

#El método realiza una búsqueda de arriba hacia abajo 
#para las características relevantes comparando la importancia 
#de los atributos originales con la importancia que se puede 
#obtener al azar, estimada

#olvide eliminar la columna Retiro
datosD_Imp2$Retiro<-NULL 
datosD_Imp2$SUELDO_BASE<-NULL
datosD_Imp2$Prom.Incent.6m<-NULL
datosD_Imp2$INCENTIVOS<-NULL

set.seed(123)
Bor.hvo<-Boruta(Retirocod~.,
                data=datosD_Imp2,
                doTrace=2);

names(Bor.hvo)
boruta.bank <- TentativeRoughFix(Bor.hvo)
print(boruta.bank)

# Boruta performed 28 iterations in 11.05488 secs.
# 15 attributes confirmed important: Age, Age_Leader, Civil_status, EDD, EDD_Category and 10
# more;
# 4 attributes confirmed unimportant: Children, Gender, Leader_Union, Q_Children_Number;

library(Amelia)
missmap(datosD_Imp2)
plot(boruta.bank, xlab = "", xaxt = "n")
getSelectedAttributes(boruta.bank, withTentative = F)
bank_df <- attStats(boruta.bank)
print(bank_df)
#windows()
plot(Bor.hvo,las=3)
# Direccionar a r, al dataset .r

names(datosD_Imp2)
# 7 variables mas importantes

#Tenure_colab +Factor_Prom..Incent.6m+FTE+FACTOR_INCENTIVOS+EDD_Category+Age+FACTOR_SUELDO_BASICO
attach(datos.r)

View(datos.r)


str(datos.r$SEXO)




#####################################
### 4. Particion Muestral ###########
#####################################
#-------------------------------------------------------------------
# Seleccion de muestra de entrenamiento (70%) y de prueba (30%)
library(caret)
set.seed(123) # Establecemos una 'semilla' aleatoria fija, 
# para que los resultados de la simulacion sean los mismos para todos
#opcion 1
index <- createDataPartition(datosD_Imp2$Retirocod, p=0.7, list=FALSE)
training <- datosD_Imp2[ index, ] # Entrenamiento del modelo
testing <-  datosD_Imp2[-index, ] # Validacion o test del modelo



View(testing)

clientes_nuevos<-testing 
#clientes_nuevos[13:15]<-NULL

View(clientes_nuevos)

# write.csv(cbind(testing,proba.pred,clase.pred),
write.csv(clientes_nuevos,"Clientes_nuevos.csv")


# train <-datos.r
# test <-datos_n

View(testing)
# Verificando la estructura de los datos particionados
prop.table(table(datos.r$Exited)) # Distribucion de y en el total
prop.table(table(training$Exited))
prop.table(table(testing$Exited))

#verificando 

#############################################################################
#  Modelamiento Predictivo Mediante Algoritmos de machine Learning ########
##############################################################################

#####################################
###### 5. Regresion Logistica ######
#####################################

options(scipen=999)#deshabilitar notacion cientifica para evitar errores de igv

#----------------------------------------------------------------------------
# Modelo Logit con las variables mas importantes (Ver Boruta)
modelo_logit <- glm (Exited ~ Age + Gender+
                       +GeographyGermany+GeographyFrance+Balance
                     +HasCrCard+IsActiveMember+NumOfProducts+CreditScore,
                     data = training, family = binomial ("logit"), maxit = 100)


summary(modelo_logit)
coef(modelo_logit)
View(modelo_logit)



# Como tenemos un modelo parsimonioso creado , lo guardamos.

saveRDS(modelo_logit,"modelo_logit.rds")

#write.csv(modelo_logit,"modelo_logit.csv")



#------------------------------------------------------------------------------
# Cociente de ventajas (Odd Ratio)
exp(coef(modelo_logit))
cbind(Coeficientes=modelo_logit$coef,ExpB=exp(modelo_logit$coef))

#------------------------------------------------------------------------------
# Cociente de ventajas e Intervalo de Confianza al 95% 
library(MASS) #Funciones de soporte y conjuntos de datos.
exp(cbind(OR = coef(modelo_logit),confint.default(modelo_logit)))


#----------------------------------------------------------
# Importancia de las variables, Feature Selection*
varImp(modelo_logit)
#error
#plot(varImp(modelo_logit))
#----------------------------------------------------------
# Seleccion de Variables  
library(MASS)
step <- stepAIC(modelo_logit,direction="backward", trace=FALSE)
step$anova



###################################################
#5.1 Indicadores para la evaluacion del modelo Rg #
###################################################


#Evaluacion del modelo_logit sobre la data testing de 3000 registros por predecir
#---------------------------------------------------------------------------------
# Para la evaluacion se usara el modelo_logit obtenido con la 
# muestra training y se validara en la muestra testing

# Prediciendo la probabilidad sobre la data testing
probareg <- predict(modelo_logit,testing,type="response")
head(probareg)


# Prediciendo la clase (con punto de corte = 0.5)
reg <- ifelse(probareg >= 0.5, 1, 0)
clasereg <- ifelse(probareg >= 0.5, 1, 0)
claseregpromo<- ifelse(probareg >= 0.5, 1, 0)


# Convirtiendo a factor
clasereg <- as.factor(clasereg)
claseregpromo <- as.factor(claseregpromo)

#significa que no fuga =0 ,fuga=1
levels(clasereg) <- c("No","Si")
levels(claseregpromo) <- c("Cliente estable","Ofrecer paquete promocional ORO al cliente para conservarlo")


str(claseregpromo)
View(claseregpromo)
View(clasereg)
str(clasereg)
str(testing$Exited)

#previzualizacion de testing antes de exportar

head(cbind(testing,probareg,clasereg,claseregpromo,reg),8) #8 registros

# EstimatedSalary Exited Exited 1        Exited 2   probareg clasereg   claseregpromo reg
# 2          4777.23      0       No Cliente estable 0.03833646       No Cliente estable   0
# 3         59172.42      0       No Cliente estable 0.04098911       No Cliente estable   0
# 11       126172.11      0       No Cliente estable 0.13987592       No Cliente estable   0
# 17       127014.32      0       No Cliente estable 0.02020806       No Cliente estable   0
# 18       145936.28      0       No Cliente estable 0.05040566       No Cliente estable   0
# 23         4491.77      0       No Cliente estable 0.02146271       No Cliente estable   0
# 33         2766.63      0       No Cliente estable 0.01968555       No Cliente estable   0
# 44        24857.25      0       No Cliente estable 0.15161205       No Cliente estable   0

#Como tenemos un modelo parsimonioso creado , lo guardamos.
#saveRDS(modelo_logit,"Rg_Logistica_toda_la_data 3.rds")


write.csv(cbind(testing,probareg,clasereg,claseregpromo,reg),
          "Testing con prob clase modelo_logit.csv")


View(cbind(testing,probareg,clasereg,claseregpromo,reg),
     "Testing con prob clase modelo_logit.csv")


# Graficando la probabilidad predicha y la clase real
ggplot(testing, aes(x = probareg, fill = Exited)) + 
  geom_histogram(alpha = 0.25)


###########################################################
# 5.2 Tabla de clasificacion de la testing con la columna reg 
###########################################################

#reg = 0 o 1
#---------------------------------------------
# Calcular el % de acierto (accuracy)
accuracy <- mean(reg==testing$Exited)
accuracy

#---------------------------------------------
# Calcular el error de mala clasificacion
error <- mean(reg!=testing$Exited)
error

#Usando tabla de proporciones
library(gmodels)
CrossTable(testing$Exited,reg,
           prop.t=FALSE, prop.c=FALSE,prop.chisq=FALSE)

# Usando el paquete caret

library(caret)
caret::confusionMatrix(reg,testing$Exited ,positive="1")

# caret::confusionMatrix(reg,testing$Exited ,positive="1")
# Error: `data` and `reference` should be factors with the same levels.
#solucion,ponerlo como factor
str(reg)
str(training$Exited)
reg <- as.factor(reg)


############################
# 5.3. Estadistico de Kappa  #
############################

# Tabla de Clasificacion
addmargins(table(Real=testing$Exited,Clase_Predicha=reg))

# Clase_Predicha
# Real     0    1  Sum
# 0   2294   94 2388
# 1    473  138  611
# Sum 2767  232 2999



#####################################
# 5.4. Curva ROC y area bajo la curva reg #
#####################################
#reg=0 o 1
#----------------------------------------------
# Usando el paquete pROC
library(pROC)

# Area bajo la curva
roc <- roc(testing$Exited,probareg)
roc$auc

#---------------------------------------------------
# Curva ROC usando el paquete caTools
library(caTools)
AUC <- colAUC(probareg,testing$Exited, plotROC = TRUE)
abline(0, 1,col="red") 

AUC  # Devuelve el area bajo la curva


puntos.corte <- data.frame(prob=roc$thresholds,
                           sen=roc$sensitivities,
                           esp=roc$specificities)
head(puntos.corte)


plot(roc,print.thres=T)



###############################
###### 6. Random Forest ######
###############################
#Tenure_colab +Factor_Prom..Incent.6m+FTE+FACTOR_INCENTIVOS+EDD_Category+Age+FACTOR_SUELDO_BASICO

set.seed(123)
library(randomForest)
modelo_rf <- randomForest(Retirocod ~ Tenure_colab +Factor_Prom..Incent.6m+FTE+FACTOR_INCENTIVOS+EDD_Category+Age+FACTOR_SUELDO_BASICO, # Y ~ X
                          data = training,   # Datos a entrenar 
                          ntree=200,           # Numero de arboles
                          mtry = 3,            # Cantidad de variables
                          # Raiz2 Total de variables o 40%-60% total variables.
                          importance = TRUE,   # Determina la importancia de las variables
                          replace=T) 

names(datosD_Imp2)
#----------------------------------
# OOB error (out of bag error)
#
print(modelo_rf)

# Graficar Error del Modelo
#
# En este grafico se muestra un modelo que intenta predecir 
# la variable churn={FUGA,ACTUAL}. 
# La linea negra representa el OOB, 
# la linea roja es el error al intentar predecir churn={ACTUAL}, 
# la linea verde es el error en la prediccion churn={FUGA}. 
# La linea negra siempre ser? el OOB, y las siguientes lineas
# se pueden identificar con la matriz de confusi?n 
# usando print(MODELO.RF) 

plot(modelo_rf)

#-----------------------------------------------------------------------------
# Importancia de las variables
# La tabla MeanDecreaseAccuracy representa en cu?nto removiendo 
# dicha variable se reduce la precision del modelo.
# Un valor mas alto de MeanDecreaseAccuracy o 
# del MeanDecreaseGiniScore, implica una mayor importancia 
# de la variable en el modelo.

varImpPlot(modelo_rf)
modelo_rf$importance



###################################################
#6.1 Indicadores para la evaluacion del modelo Rf #
###################################################



#Evaluacion del modelo_rf sobre la data testing de 3000 registros por predecir
#---------------------------------------------------------------------------------
# Para la evaluacion se usara el modelo_logit obtenido con la 
# muestra training y se validara en la muestra testing

# Prediciendo la probabilidad sobre la data testing

probarf <- predict(modelo_rf,testing,type="prob")

head(probarf)
probarf <- probarf[,2]
head(probarf)
str(probarf)

View(probarf)


# Prediciendo la clase (con punto de corte = 0.5)
rf <- ifelse(probarf >= 0.5, 1, 0)
claserf <- ifelse(probarf >= 0.5, 1, 0)
claserfpromo<- ifelse(probarf >= 0.5, 1, 0)


# Convirtiendo a factor

rf <- as.factor(rf)
claserf <- as.factor(claserf)
claserfpromo <- as.factor(claserfpromo)

#rf por defecto tiene 2 niveles , 0 o 1

#significa que no fuga =0 ,fuga=1
#levels(rf)<- c("No","Si")
levels(claserf) <- c("No","Si")
levels(claserfpromo) <- c("Cliente estable","Ofrecer paquete promocional ORO al cliente para conservarlo")


str(claserfpromo)
View(claserfpromo)
View(claserf)
str(claserf)
str(testing$Exited)

# Prediciendo la probabilidad
#ya esta incluido ,y sale 0 o 1 y ya esta 
#ya esta convertido de numeric a factor 
#proba.pred <- predict(modelo_rf,data.prueba,type="prob")
#previzualizacion de testing antes de exportar

head(cbind(testing,probarf,claserf,claserfpromo,rf),8)
# 
# EstimatedSalary Exited Exited 1        Exited 2 probarf claserf    claserfpromo rf
# 2          4777.23      0       No Cliente estable   0.135      No Cliente estable  0
# 3         59172.42      0       No Cliente estable   0.050      No Cliente estable  0
# 11       126172.11      0       No Cliente estable   0.065      No Cliente estable  0
# 17       127014.32      0       No Cliente estable   0.040      No Cliente estable  0
# 18       145936.28      0       No Cliente estable   0.215      No Cliente estable  0
# 23         4491.77      0       No Cliente estable   0.030      No Cliente estable  0
# 33         2766.63      0       No Cliente estable   0.025      No Cliente estable  0
# 44        24857.25      0       No Cliente estable   0.195      No Cliente estable  0

#guardando el modelo y la comparacion con data testing
saveRDS(modelo_rf,"modelo_rf.rds")


# # write.csv(cbind(testing,proba.pred,clase.pred),
# write.csv(cbind(testing,probarf,probapred),
#           "Testing con clase y proba predicha-modelo_rf_5.csv")
# View(cbind(testing,probarf,probapred),
#      "Testing con clase y proba predicha-modelo_rf_5.csv")
# ##guaradmos los dos modelos rf y glm en una misma tabla 
# 


write.csv(cbind(testing,probarf,claserf,claserfpromo,rf),
          "Testing con prob clase modelo_rf.csv")


View(cbind(testing,probarf,claserf,claserfpromo,rf),
     "Testing con prob clase modelo_rf.csv")


#error en la longitud de las filas 


#########################################################################################
#Desde Testing con prob clase modelo_rf.csv,eliminando Exited ,Exited 1 ,Exited 2 [14:16]

final<- read.csv("~/Machine learning Beginner Intermediate/Proyecto abandono de clientes mstr/Modelo 3.0/Testing con prob clase modelo_rf.csv")

Testing_customer_support <- as.data.frame(final)
View(Testing_customer_support)
Testing_customer_support[14:16] <- NULL

#write.csv(Testing_customer_support,"Testing_customer_support.csv")



# Exportando a un cubo data testing para construir hypercard 

ds3 = Dataset$new(connection=conn, name="Cubo_data_tresting_y_prob_predicha")
ds3$add_table(name="tabla_data_testing_prob_predicha", data_frame= datos.f, update_policy="add")
ds3$create()





###########################################################
# 6.2 Tabla de clasificacion de la testing con la columna rf
###########################################################

#rf=0 o 1 
library(gmodels)
CrossTable(testing$Retirocod,rf,
           prop.t=FALSE, prop.c=FALSE,prop.chisq=FALSE)

# Usando el paquete caret

library(caret)

#caret::confusionMatrix(clase.pred,data.prueba$Loan_Status,positive="Fuga")
caret::confusionMatrix(rf,testing$Retirocod,positive="1")

# caret::confusionMatrix(reg,testing$Exited ,positive="1")
# Error: `data` and `reference` should be factors with the same levels.
#solucion,ponerlo como factor
str(rf)
str(training$Exited)
rf <- as.factor(rf)

############################
# 6.3. Estadistico de Kappa  #
############################

# Tabla de Clasificacion
addmargins(table(Real=testing$Exited,Clase_Predicha=reg))

#           Clase_Predicha
# Real     Actual Fuga Sum
# Actual      168   81 249
# Fuga         25  128 153
# Sum         193  209 402



#####################################
# 5.4. Curva ROC y area bajo la curva reg #
#####################################
#reg=0 o 1
#----------------------------------------------
# Usando el paquete pROC
library(pROC)

# Area bajo la curva
roc <- roc(testing$Exited,probareg)
roc$auc

#---------------------------------------------------
# Curva ROC usando el paquete caTools
library(caTools)
AUC <- colAUC(probareg,testing$Exited, plotROC = TRUE)
abline(0, 1,col="red") 

AUC  # Devuelve el area bajo la curva


puntos.corte <- data.frame(prob=roc$thresholds,
                           sen=roc$sensitivities,
                           esp=roc$specificities)
head(puntos.corte)



plot(roc,print.thres=T)

# Graficando la Sensibilidad y Especificidad
ggplot(puntos.corte, aes(x=prob)) + 
  geom_line(aes(y=sen, colour="Sensibilidad")) +
  geom_line(aes(y=esp, colour="Especificidad")) + 
  labs(title ="Sensibilidad vs Especificidad", 
       x="Probabilidad") +
  scale_color_discrete(name="Indicador") +
  geom_vline(aes(xintercept=0.5507937),
             color="black", linetype="dashed", size=0.5) + 
  theme_replace() 



###############################
###### 7. XGBOOST ######
###############################

library(xgboost)
View(training)
# Separar covariables y target

#no considerar el Target retirocod
#Mtrain_XGB <- model.matrix(~ ., data=training[,c(1:20)]) # X
Mtrain_XGB <- model.matrix(~ ., data=training[-19])
View(Mtrain_XGB)

dim(Mtrain_XGB)

Ytrain <- as.vector(training$Retirocod)                # Y
View(Ytrain)



# Construimos una matriz Xgboost
dtrain <- xgb.DMatrix(Mtrain_XGB, label = Ytrain)


#dtrain = xgb.DMatrix(as.matrix(sapply(Mtrain_XGB, as.numeric)), label=label_train)

# Lo mismo que le hago al train, le hago al test
# Mtest_XGB <- model.matrix(~ ., data=testing[,c(1:12)])


Mtest_XGB <- model.matrix(~ ., data=testing[-19])
Ytest <- as.vector(testing$Retirocod)
dtest <- xgb.DMatrix(Mtest_XGB, label = Ytest)

#Hacemos nuestra lista de particiones de datos
watchlist <- list(train = dtrain, test = dtest)

# Grilla NÂ° 01
param <- list(max.depth = 1, # Profundidad arboles
              eta=0.00001,     # Ratio de aprendizaje
              silent = 0,    # Mostrar resultados
              alpha=0.8,       # L1
              #gamma=1,       # L2  
              objective="reg:logistic", # El tipo de modelo
              eval_metric="auc")

# Grilla NÂ° 02
param <- list(booster = "gbtree", 
              objective = "binary:logistic", 
              eta=0.00001,
              alpha=0.8,
              gamma=0.7, 
              max_depth=1, 
              #min_child_weight=1, 
              subsample=0.5, 
              colsample_bytree=0.5,
              eval_metric = "auc")
# Entrenamiento-----------------------------------
xgb_fit <- xgb.train(param, 
                     dtrain, 
                     nround = 1000, 
                     watchlist,
                     verbose = 1,
                     early_stopping_rounds = 15)

# Importancia de las variables
importance_xgb <- xgb.importance(model = xgb_fit)
importance_xgb


# Prediccion
proba_xgb <- predict(xgb_fit,dtest,type="response")
head(proba_xgb)
str(proba_xgb)

# Convertimos la clase a probabilidad

clas_Xgb <- ifelse(pred_xgb_t<0.50,'0','1')
clas_Xgb <- as.factor(clas_Xgb)

#############################
# 7.2.1. Tabla de clasificacion #
#############################
# Vemos la matriz de confusion

matrizXGB <- caret::confusionMatrix(clas_Xgb,testing$Retirocod,positive='1')
matrizXGB

caret::confusionMatrix(rf,testing$Retirocod,positive="1")


#####################################
# 7.2.2 Curva ROC y area bajo la curva XGBOOST #
#####################################
#reg=0 o 1
#----------------------------------------------
# Usando el paquete pROC
library(pROC)

# Area bajo la curva
roc <- roc(testing$Retirocod,proba_xgb)
roc$auc

#---------------------------------------------------
# Curva ROC usando el paquete caTools
library(caTools)
AUC <- colAUC(proba_xgb,testing$Retirocod, plotROC = TRUE)
abline(0, 1,col="red") 

AUC  # Devuelve el area bajo la curva


puntos.corte <- data.frame(prob=roc$thresholds,
                           sen=roc$sensitivities,
                           esp=roc$specificities)
head(puntos.corte)



plot(roc,print.thres=T)

# Graficando la Sensibilidad y Especificidad
ggplot(puntos.corte, aes(x=prob)) + 
  geom_line(aes(y=sen, colour="Sensibilidad")) +
  geom_line(aes(y=esp, colour="Especificidad")) + 
  labs(title ="Sensibilidad vs Especificidad", 
       x="Probabilidad") +
  scale_color_discrete(name="Indicador") +
  geom_vline(aes(xintercept=0.5507937),
             color="black", linetype="dashed", size=0.5) + 
  theme_replace() 





#######################################################
###### 8 .Estabilidad de Algoritmos : Validacion Cruzada #
#-----------------------------------------------------------------------------
# Podemos gracias a cv, asegurarnos de tener un modelo bajo en bias y variance
xgbcv <- xgb.cv( params = param, 
                 data = dtrain, 
                 nrounds = 10000, 
                 nfold = 10, 
                 showsd = T, 
                 stratified = T, 
                 print_every_n = 10, 
                 early_stopping_rounds = 100, 
                 maximize = T)

xgbcv
summary(xgbcv)


##########################
# 5. Coeficiente de Gini #
##########################

gini <-  2*AUC -1 ; gini

################
# 6. Log Loss  #
################

# Transformar la variable CHURN a numerica
real <- as.numeric(testing$CHURN)
head(real)
# [1] 1 1 2 1 1 2

# Recodificar los 1 y 2 como 0 y 1 respectivamente
real <- ifelse(real==2,1,0)

library(MLmetrics)
LogLoss(proba.pred,real)







############################################
# 8. Union de modelos logit + randon forest
############################################
##reg
write.csv(cbind(testing,probareg,clasereg),
          "Testing con clase y proba predicha-Logistica_toda_data 3.csv")


View(cbind(testing,probareg,clasereg),
     "Testing con clase y proba predicha-Logistica_toda_data 3.csv")

###rf 
#guardando el modelo y la comparacion con data testing
saveRDS(modelo_rf,"modelo_rf_2.3.rds")


# write.csv(cbind(testing,proba.pred,clase.pred),
write.csv(cbind(testing,probarf,claserf),
          "Testing con clase y proba predicha-modelo_rf_2.3.csv")



View(cbind(testing,probarf,claserf),
     "Testing con clase y proba predicha-modelo_rf_2.3.csv")
##guaradmos los dos modelos rf y glm en una misma tabla 

######

write.csv(cbind(testing,probareg,clasereg,probarf,claserf),
          "Testing con clase y proba predicha-Logit_y_rf3.csv")


View(cbind(testing,probareg,clasereg,probarf,claserf),
     "Testing con clase y proba predicha-Logit_y_rf.csv")



###################################################
###9.Modelo Regresion logistica con Caret y CV ###
###################################################

# Relacion de parametros a ajustar de un modelo
modelLookup(model='glm')

# Aplicando el modelo con Validacion Cruzada "cv"
ctrl <- trainControl(method="cv",number=10)

######################################################
ctrl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


set.seed(825)
gbmFit1 <- training(Exited ~ Age + Gender+
                      +Geography+Balance
                    +HasCrCard+IsActiveMember, data = training, 
                    method = "gbm", 
                    trControl = ctrl,
                    ## This last option is actually one
                    ## for gbm() that passes through
                    verbose = FALSE)
gbmFit1

summary(gbmFit1)

plot(gbmFit1)


#####################################################
# Prediciendo la probabilidad
proba.pred_gbm <- predict(gbmFit1,testing,type="prob") # "raw" "response"
head(proba.pred_gbm)

# Prediciendo la clase (con punto de corte = 0.5)
clase.pred <- ifelse(proba.pred_gbm >= 0.5, 1, 0)


head(clase.pred)
View(clase.pred)
str(clase.pred)

# Convirtiendo a factor
clase.pred <- as.factor(clase.pred)          
#significa que no fuga =0 ,fuga=1
levels(clase.pred) <- c("0","1")
######################################################

set.seed(123)
modelo_log <- train(Exited ~ ., 
                    data = training, 
                    method = "glm", family="binomial", 
                    trControl = ctrl, 
                    tuneLength = 5,
                    metric="Accuracy")
modelo_log

summary(modelo_log)

#si no hay parametros de ajuste,se produce un error
plot(modelo_log)



varImp(modelo_log)

########################################################
##### 10. Despliegue en produccion de los modelos #######
########################################################

# Deseamos replicar o implementar el modelo.
# Leemos el modelo predictivo.
RegresionLog <- readRDS("modelo_logit.rds")


# Leemos el dataset de nuevos leads clientes
library(foreign)



######Importando desde excel

datos_n <- read_excel("Churn_nuevos.xlsx")




######Importando desde cubo mstr
library(mstrio)
base_url <- "http://hp:8080/MicroStrategyLibrary/api"
username <- "administrator"
password <- ""
project_name <- "MicroStrategy Tutorial"
conn <- connect_mstr(base_url=base_url, username=username, password=password, project_name=project_name)
my_cube <- Cube$new(connection=conn, cube_id="77E6B3C54F61B62916A26DA68D2DDB02", parallel=FALSE)
df <- my_cube$to_dataframe()

View(df)
names(df)
df

#eliminando la columna por defecto de mstr nrow
df[8] <- NULL 
datos_n<-df

str(datos_n)
dim(datos_n)
length(datos.r)
datos.r<-na.omit(datos_n)

str(datos_n)

# Verificacion de datos perdidos
library(DataExplorer)

plot_missing(datos_n)

# Graficar la cantidad de valores perdidos
library(VIM)
graf_perdidos1 <- aggr(datos_n,prop = F, 
                       numbers = TRUE,
                       sortVars=T,
                       cex.axis=0.5)

summary(graf_perdidos1)

View(datos_n)
####################################################################

# Para los datos de test "arboles nuevos"
datos_n$AUTO <- as.factor(datos_n$AUTO)

datos_n$CIVIL <- as.factor(datos_n$CIVIL)
datos_n$SEXO <- as.factor(datos_n$SEXO)
datos_n$EDAD <- as.integer(datos_n$EDAD)
datos_n$HIJOS <- as.integer(datos_n$HIJOS)
datos_n$INGRESO <- as.numeric(datos_n$INGRESO)



# No considerar la variable de identificacion ID
#datos.n$CustomerId <- NULL
datos_n$Age <- as.integer(datos_n$Age)
datos_n$Gender <- as.factor(datos_n$Gender)
datos_n$Geography <- as.factor(datos_n$Geography)
datos_n$HasCrCard <- as.factor(datos_n$HasCrCard)
datos_n$IsActiveMember <- as.factor(datos_n$IsActiveMember)
datos_n$NumOfProducts <- as.integer(datos_n$NumOfProducts)
datos_n$Surname <- as.factor(datos_n$Surname)
datos_n$Tenure <- as.integer(datos_n$Tenure)
datos_n$Balance <- as.numeric(datos_n$Balance)
datos_n$CreditScore <- as.integer(datos_n$CreditScore)
#datos_n$Exited <- as.factor(datos_n$Exited)


str(datos.r)

# Etiquetando las opciones de las variables categoricas

levels(datos.r$SEXO)  <- c("Fem","Masc")
levels(datos.r$CIVIL) <- c("Casado","Soltero")
levels(datos.r$AUTO)  <- c("Si","No")

str(datos.r)
###################################################################


# Scorear o puntuar nuevos registros
base_5 <- predict(RegresionLog,datos_n,type = "response")
# Convertir a prediccion
Score_5 <- ifelse(base_5<=0.50,"0","1")
#Score_5 <- ifelse(base_5<=0.50,"No Fuga","Fuga")
# Lo mandamos a gestionar a distintas areas
Base_5 <- data.frame(
  DNI=datos_n$CustomerId,
  Score_Predict=Score_5)

Base_5

# Exportar el objeto
write.csv(Base_5,"Base_5.1mstr.csv",row.names = F)
#####################################################################


######Exportar a mstr al proyecto abierto 

datos_n

ds = Dataset$new(connection=conn, name="Store Analysis")

#agregando una tabla
datos_n$add_table(name="tablayesin", data_frame=BaseGestNov19, update_policy="add")



datos_n$create()
names(datos_n)

datos_n$add_table(name="tablayesin", data_frame=BaseGestNov19, update_policy="add",
                  to_attribute=list("AUTO"))

