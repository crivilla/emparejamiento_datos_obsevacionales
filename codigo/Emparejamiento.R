######## EMPAREJAMIENTO GRUPO CONTROL Y GRUPO INTERVENCIÓN COTRACO ######

#CARGA DE LOS DATOS

install.packages("readxl")
library("readxl")

#Carga del dataset del grupo intervención
sujetos_interv_raw <- read_excel("~/Desktop/TFG/TFG-R dir/dataset_interv.xlsx")
colnames(sujetos_interv_raw)
#Selección de columnas 
sujetos_interv_raw = subset(sujetos_interv_raw, select = c(fecha_subida, bcomun, edad, sexo, bhta, bacv, bangina, biam, bdiabe, bfuma, bpro_pa_bpro_pa_s, bpeso, btalla, bimc, hfe_re, hcolest))

#Carga del dataset del grupo control
sujetos_ctrl_raw<- read_excel("~/Desktop/TFG/TFG-R dir/dataset_control.xlsx")
str(sujetos_ctrl_raw)
colnames(sujetos_ctrl_raw)

##Selección de columnas (se elimina la columna índice) 
sujetos_ctrl_raw = subset(sujetos_ctrl_raw, select = -CODIGO_PAC)
str(sujetos_ctrl_raw)

sujetos_ctrl_raw[rowSums(is.na(sujetos_ctrl_raw)) != ncol(sujetos_ctrl_raw), ]       # Se eliminan líneas que estén completamente vacías



#PREPARACIÓN DE LOS DATASETS


##################

##################

#Exploración para detectar qué variables deben de ser procesadas previo emparejamiento
# ....

#Unificación de las variables y selección de columnas de interés para el emparejamiento 

##GRUPO INTERVENCIÓN
## Transformación de variables de texto (chr) en variables numéricas
table(sujetos_interv_raw$bcomun,useNA = 'ifany') #Comprobación. Todos los sujetos son de la comunidad 2 (Colombia)

unique(sujetos_interv_raw$sexo)
sujetos_interv_raw$sexo_fem_as_num_interv<-as.numeric(sujetos_interv_raw$sexo=='Femenino') # 1 si cumple la condición (sexo femenino), 0 si no la cumple
str(sujetos_interv_raw)

unique(sujetos_interv_raw$bhta)
#El conjunto de datos de control solo contiene datos de hipertensos no controlados (los hipertensos de novo del grupo intervención son eliminados)
table(sujetos_interv_raw$bhta, useNA = 'ifany')
sujetos_interv_raw<-sujetos_interv_raw[sujetos_interv_raw$bhta ==1, ]  

unique(sujetos_interv_raw$bacv)
unique(sujetos_interv_raw$bangina)
unique(sujetos_interv_raw$biam)
unique(sujetos_interv_raw$bdiabe)
unique(sujetos_interv_raw$bfuma)

## Transformación de la variable bfuma en binaria: valores 1 y 2 son no fumadores (0), valores 3-7 son fumadores (1)
##Atendiendo a la documentación de los cuadernos de recogida de datos y las equivalencias definidas
sujetos_interv_raw <- mutate(
  sujetos_interv_raw, bfuma = case_when(
    bfuma == 1 | bfuma == 2 ~ 0, 
    TRUE   ~ 1 
  ))

#Creación de dataset con estas variables procesadas
sujetos_interv_df = subset(sujetos_interv_raw, select = c(edad, sexo_fem_as_num_interv, bacv, bangina, biam, bdiabe, bfuma, bpro_pa_bpro_pa_s, bpeso, btalla, bimc, hcolest))
str(sujetos_interv_df)


##GRUPO CONTROL
str(sujetos_ctrl_raw)

sujetos_ctrl_raw$sexo_fem_as_num_ctrl<-as.numeric(sujetos_ctrl_raw$SEXO=='F')

#Cambio de nombre de las columnas para facilitar el proceso de preparación de los datos
sujetos_ctrl_raw <-
  rename(sujetos_ctrl_raw,
    HTA_K86_FECHA = HTA_K86,
    HTA_COM_K87_FECHA = HTA_COM_K87,
    DM1_T89_FECHA=DM1_T89,
    DM2_T90_FECHA=DM2_T90,
    TABACO_DGP_FECHA=FECHA_TABACO,
    PAS_FECHA=FECHA_PAS,
    PESO_FECHA=FECHA_PESO,
    TALLA_FECHA=FECHA_TALLA,
    IMC_FECHA=FECHA_IMC,
    COLESTEROL_FECHA=FECHA_COLESTEROL,
    FUMADOR_P17_FECHA=FUMADOR_P17,
    ACVA_K90_K91_FECHA=ACVA_K90_K91,
    IAM_K75_K76_FECHA=IAM_K75_K76,
    ANGINA_K74_FECHA=ANGINA_K74
  )

sujetos_ctrl_raw$HTA_K86 <- ifelse(is.na(sujetos_ctrl_raw$HTA_K86_FECHA), 0, 1)
sujetos_ctrl_raw$HTA_COM_K87 <- ifelse(is.na(sujetos_ctrl_raw$HTA_COM_K87_FECHA), 0, 1)
sujetos_ctrl_raw$DM1_T89 <- ifelse(is.na(sujetos_ctrl_raw$DM1_T89_FECHA), 0, 1)
sujetos_ctrl_raw$DM2_T90 <- ifelse(is.na(sujetos_ctrl_raw$DM2_T90_FECHA), 0, 1)
sujetos_ctrl_raw$FUMADOR_P17 <- ifelse(is.na(sujetos_ctrl_raw$FUMADOR_P17_FECHA), 0, 1)
sujetos_ctrl_raw$ACVA_K90_K91 <- ifelse(is.na(sujetos_ctrl_raw$ACVA_K90_K91_FECHA), 0, 1)
sujetos_ctrl_raw$IAM_K75_K76 <- ifelse(is.na(sujetos_ctrl_raw$IAM_K75_K76_FECHA), 0, 1)
sujetos_ctrl_raw$ANGINA_K74 <- ifelse(is.na(sujetos_ctrl_raw$ANGINA_K74_FECHA), 0, 1)

str(sujetos_ctrl_raw)

#Unificar las variables HTA_K86 y HTA_COM_K87
#La variable HTA previa se determina como 1 si el paciente tiene un episodio de HTA_K86 o un episodio de HTA_COM_K87
table(sujetos_ctrl_raw$HTA_K86)
table(sujetos_ctrl_raw$HTA_COM_K87)
sujetos_ctrl_raw$hta_previa_ctrl<-ifelse(sujetos_ctrl_raw$HTA_K86==1|sujetos_ctrl_raw$HTA_COM_K87==1, 1, 0)
table(sujetos_ctrl_raw$hta_previa_ctrl)

#Crear nueva variable FECHA_HTA (fecha del primer diagnóstico de HTA), la menor entre HTA_K86_FECHA y HTA_COM_K87_FECHA
vector_hta_fecha <- ifelse(!is.na(sujetos_ctrl_raw$HTA_K86_FECHA),ifelse(!is.na(sujetos_ctrl_raw$HTA_COM_K87_FECHA),ifelse(sujetos_ctrl_raw$HTA_K86_FECHA<sujetos_ctrl_raw$HTA_COM_K87_FECHA,sujetos_ctrl_raw$HTA_K86_FECHA,
                                                                                                                                     as.POSIXct(sujetos_ctrl_raw$HTA_COM_K87_FECHA)),
                                                                                   as.POSIXct(sujetos_ctrl_raw$HTA_K86_FECHA)),
                                     as.POSIXct(sujetos_ctrl_raw$HTA_COM_K87_FECHA))
class(vector_hta_fecha) <- c("POSIXct", "POSIXt")
sujetos_ctrl_raw$HTA_FECHA <- format(as.POSIXct(vector_hta_fecha,format = '%Y%m%d %H:%M:%S'),format = '%Y-%m-%d')

str(sujetos_ctrl_raw)

#Unificar la variable tabaco. Se determina como 1 si el paciente tiene el DGP de TABAQUISMO (TABACO_DGP) o un episodio de tabaquismo abierto (FUMADOR_P17)
table(sujetos_ctrl_raw$TABACO_DGP, useNA = 'ifany')
sujetos_ctrl_raw["TABACO_DGP"][is.na(sujetos_ctrl_raw["TABACO_DGP"])] <- 0 #Asumimos que si no está recogida el DGP 'fumador' para el paciente, es no fumador
table(sujetos_ctrl_raw$FUMADOR_P17)

sujetos_ctrl_raw$tabaquismo_ctrl<-ifelse(sujetos_ctrl_raw$TABACO_DGP==1|sujetos_ctrl_raw$FUMADOR_P17==1, 1, 0)
table(sujetos_ctrl_raw$tabaquismo_ctrl)

#El conjunto de datos de intervención no recoge datos de DM1 (los pacientes con DM1 del grupo control son eliminados)
table(sujetos_ctrl_raw$DM1_T89)
sujetos_ctrl_raw<-sujetos_ctrl_raw[sujetos_ctrl_raw$DM1_T89!=1, ]  

#Subset país de nacimiento Colombia
table(sujetos_ctrl_raw$PAIS)
sujetos_ctrl_pais_nac_Esp<-sujetos_ctrl_raw[sujetos_ctrl_raw$PAIS=='COLOMBIA'& !is.na(sujetos_ctrl_raw$PAIS), ]  
###
str(sujetos_ctrl_raw)

sujetos_ctrl_df = subset(sujetos_ctrl_raw, select = c(EDAD, sexo_fem_as_num_ctrl, ACVA_K90_K91, ANGINA_K74, IAM_K75_K76, DM2_T90, tabaquismo_ctrl, PAS, PESO, TALLA, IMC, COLESTEROL))


#UNIÓN DE LOS DATASETS

#Añadimos los valores de tratamiento recibido
sujetos_interv_df$INTERV <- 1
sujetos_ctrl_df$INTERV <- 0

str(sujetos_interv_df)
str(sujetos_ctrl_df)

#Unión de los datasets del grupo control e intervención en un solo conjunto de datos

df <- rbind(sujetos_ctrl_df,         # Rename columns & rbind
                  setNames(sujetos_interv_df, names(sujetos_ctrl_df)))
df

df <-
  rename(df,
         TABAQ = tabaquismo_ctrl,
         SEXO = sexo_fem_as_num_ctrl
  )



########## EXPLORACIÓN INICIAL DEL DATASET INICIAL A EMPAREJAR ############

library(DataExplorer)
DataExplorer::create_report(df)

##Valoración de posibles variables por las cuales emparejar según disponibilidad:
##Las variables TALLA, PESO, IMC y especialmente COLESTEROL tienen un alto porcentaje de missing values (8%, >35% y >95% respectivamente)



########## EVALUACIÓN DEL EQUILIBRIO INICIAL #############

#Variables por las cuales se empareja
xvars<-c("EDAD", "SEXO", "ACVA_K90_K91", "ANGINA_K74", "IAM_K75_K76", "DM2_T90", "TABAQ", "PAS")

#table 1: resumen estadístico de las variables en xvars
#Estratificado por intervención para comparar, poder evaluar el equilibrio entre el grupo control e intervención (entre sus sujetos)
table1_prematch<- CreateTableOne(vars=xvars,strata="INTERV", data=df, test=FALSE)
print(table1_prematch,smd=TRUE) 
#Consideramos como no aceptables los smd>0.1, evidencian desequilibrio y dan una idea de la magnitud del fenómeno de confusión que podría suponer

# Exportación de la Tabla 1 
library(readr)
tab1Mat_prematch <- print(table1_prematch, quote = FALSE, noSpaces = TRUE,smd=TRUE)
## Fichero csv
write.csv(tab1Mat_prematch, file = "table1_prematch.csv")
table1_prematch_df <- read_csv("table1_prematch.csv")
table1_prematch_df
#Fichero Excel
library("writexl")
write_xlsx(table1_prematch_df,"~/Desktop/TFG/TFG-R dir/tableones-Matching.xlsx", col_names = TRUE, format_headers = TRUE)

##############   EMPAREJAMIENTO  (DIFERENTES EJECUCIONES) #############
library(MatchIt)

# CONJUNTO #1 DE VARIABLES
##Se considera incluir solo las variables con 0% de NA 
#Covariables incluidas: EDAD, SEXO, ACVA_K90_K91, ANGINA_K74, IAM_K75_K76, DM2_T90, TABAQ, PAS

# PRUEBA 1 - CONJUNTO #1 DE VARIABLES - DISTANCIA: MAHALANOBIS - MÉTODO: NN
#Parámetros:
# Sin reemplazo
# Ratio: 1:1

m.outMah <- matchit(INTERV~EDAD+SEXO+ACVA_K90_K91+ANGINA_K74+IAM_K75_K76+DM2_T90+TABAQ+PAS, 
                    data = df, 
                    method = "nearest",
                    distance = "mahalanobis",
                    replace=FALSE)
m.outMah
summary(m.outMah)

#Visualización del equilibrio entre los grupos según sus covariables
plot(summary(m.outMah))

plot(m.outMah, type = "qq", interactive = FALSE,
     which.xs = c("EDAD", "SEXO"))

plot(m.outMah, type = "histogram")
#Gráfico de densidad
plot(m.outMah, type = "density", interactive = FALSE,
     which.xs = ~EDAD+SEXO)

#Gráfico de densidad (variables continuas)
library('cobalt')
bal.plot(m.outMah, var.name = "EDAD", which = "both",
         type = "histogram",mirror = TRUE)

#Exportación del conjunto final emparejado
m.data1 <- match.data(m.outMah, data = df,
                      distance = "mahalanobis")
write_xlsx(m.data1,"~/Desktop/TFG/TFG-R dir/conjunto_emparejado_1.xlsx", col_names = TRUE, format_headers = TRUE)


# PRUEBA 2 - CONJUNTO #1 DE VARIABLES - DISTANCIA: MAHALANOBIS - MÉTODO: NN
#Parámetros:
# Sin reemplazo
# Ratio: 3:1

m.outMah2 <- matchit(INTERV~EDAD+SEXO+ACVA_K90_K91+ANGINA_K74+IAM_K75_K76+DM2_T90+TABAQ+PAS, 
                    data = df, 
                    method = "nearest",
                    distance = "mahalanobis",
                    replace=FALSE,
                    ratio = 3)
m.outMah2
summary(m.outMah2)

# PRUEBA 3 - CONJUNTO #1 DE VARIABLES - DISTANCIA: Mahalanobis robusta - MÉTODO: NN 
#Parámetros:
# Sin reemplazo
# Ratio: 3:1

m.out_robustMah <- matchit(INTERV~EDAD+SEXO+ACVA_K90_K91+ANGINA_K74+IAM_K75_K76+DM2_T90+TABAQ+PAS, 
                           data = df, 
                           method = "nearest",
                           distance = "robust_mahalanobis",
                           replace=FALSE,
                           ratio = 3)
m.out_robustMah
summary(m.outPS)

# PRUEBA 4 - CONJUNTO #1 DE VARIABLES - DISTANCIA: MAHALANOBIS - MÉTODO: OPTIMAL
#Parámetros:
# Reemplazo: no se especifica en optimal
# Ratio: 1:1

m.outMah <- matchit(INTERV~EDAD+SEXO+ACVA_K90_K91+ANGINA_K74+IAM_K75_K76+DM2_T90+TABAQ+PAS, 
                    data = df, 
                    method = "optimal",
                    distance = "mahalanobis",
                    ratio = 3)
m.outMah
summary(m.outMah)

  #Debido al tamaño del dataset se produce un error: Error: vector memory exhausted (limit reached?)

# PRUEBA 5 - CONJUNTO #1 DE VARIABLES - DISTANCIA: Propensity Score (Propensity Score Matching) - MÉTODO: NN 
#Parámetros:
# Sin reemplazo
# Ratio: 3:1

m.outPS <- matchit(INTERV~EDAD+SEXO+ACVA_K90_K91+ANGINA_K74+IAM_K75_K76+DM2_T90+TABAQ+PAS, 
                    data = df, 
                    method = "nearest",
                    replace=FALSE,
                    ratio = 3)
m.outPS
summary(m.outPS)

#Gráficos de PS 
plot(m.outPS,type = "jitter",interactive = FALSE)
  #Las dristribuciones tras el emparejamiento son más similares
plot(m.outPS,type = "hist")




############ AJUSTAR UN MODELO DE PROPENSITY SCORE #############

#Utilizando las funciones de MatchIt no es necesario implementar explícitamente un modelo de PS (MatchIt estima el PS y empareja directamente)
#Pero realizar estos pasos permite estudiar mejor las características del conjunto de datos y especificar parámetros del modelo de estimación del PS

#glm (Generalized Linear Models)

#ajustar un modelo de PS (regresión logística)
psmodel<-glm(INTERV~EDAD+SEXO+ACVA_K90_K91+ANGINA_K74+IAM_K75_K76+DM2_T90+TABAQ+PAS, #'INTERV' sería el outcome para la regresión logística, el resto las variables que se quieren controlar
             family=binomial(), #el 'outcome' es binario, por defecto se utiliza logit link (escala)
             data=df) 
summary(psmodel)

#da una idea de qué variables son predictivas de recibir el tratamiento (intervención)
#podría ser interesante para ver quien sería más propenso a recibir la intervención (pertenecer al grupo intervención), qué tienen en común esos sujetos (los valores de sus variables)

pscore<-psmodel$fitted.values

#check overlap
h1 = hist(pscore[df$INTERV==1], plot=FALSE,breaks=20)
h2 = hist(pscore[df$INTERV==0], plot=FALSE,breaks=20)
h2$counts = - h2$counts
hmax = max(h1$counts)
hmin = min(h2$counts)
X = c(h1$breaks, h2$breaks)
xmax = max(X)
xmin = min(X)
plot(h1, ylim=c(hmin, hmax), col="Red", xlim=c(xmin, xmax),
     xlab='propensity score',main="",yaxt='n',ylab='')
lines(h2, col="blue")

##### EMPAREJAMIENTO CON LA LIBRERÍA 'Matching' #####

library(Matching)

logit <- function(p) {log(p)-log(1-p)}

psmatch<-Match(Tr=df$INTERV,M=1,X=logit(pscore),replace=FALSE,
               caliper=.2) 

matched<-df[unlist(psmatch[c("index.treated","index.control")]), ] 

table1_matched<-CreateTableOne(vars=xvars, strata ="INTERV", 
                               data=matched, test = FALSE)
print(table1_matched, smd = TRUE)
print(table1_prematch, smd = TRUE)

#check overlap, tras el emparejamiento
h1 = hist(pscore[matched$INTERV==1], plot=FALSE,breaks=20)
h2 = hist(pscore[matched$INTERV==0], plot=FALSE,breaks=20)
h2$counts = - h2$counts
hmax = max(h1$counts)
hmin = min(h2$counts)
X = c(h1$breaks, h2$breaks)
xmax = max(X)
xmin = min(X)
plot(h1, ylim=c(hmin, hmax), col="Red", xlim=c(xmin, xmax),
     xlab='propensity score',main="",yaxt='n',ylab='')
lines(h2, col="blue")
########## export TableOne
tab1Mat_matched <- print(table1_matched, quote = FALSE, noSpaces = TRUE,smd=TRUE)
write.csv(tab1Mat_matched, file = "table1_matched.csv")
table1_matched_df <- read_csv("table1_matched.csv")

#CALCULAR VARIACIÓN SMD TOTAL
table1_matched_df=table1_matched_df[-1,]

#hay un campo char "<0.001", transformar
table1_matched_df$SMD <- sapply(table1_matched_df$SMD, function(x) {
  y <- as.numeric(x)
  if (is.na(y)) {
    0
  } else {
    y
  }
})

variación_smd_porVariable=-(table1_prematch_df$SMD-as.numeric(table1_matched_df$SMD))
cat(variación_smd_porVariable, sep = "\n") #print vertical
sum(variación_smd_porVariable)

#write in a new sheet, xlsx   #########REVISAR
install.packages("XLConnect")
library(XLConnect)
# Load workbook 
wb <- loadWorkbook("~/Desktop/pruebasR-PSM/tableones-PSM.xlsx", create = TRUE) #created if it does not exist yet
# Create a worksheet called 'CO2'
createSheet(wb, name = "CO2")
# Save workbook (this actually writes the file to disk)
saveWorkbook(wb)
# clean up 
file.remove("createSheet.xlsx")
## End(Not run)
write_xlsx(table1_matched_df,"~/Desktop/table1.xlsx", col_names = TRUE, format_headers = TRUE)
###############################

# CONJUNTO #2 DE VARIABLES
##Se considera incluir las variables con 0% de NA y el IMC

#Variables por las cuales se empareja
xvars<-c("EDAD", "SEXO", "ACVA_K90_K91", "ANGINA_K74", "IAM_K75_K76", "DM2_T90", "TABAQ", "PAS", "IMC")

#table 1: resumen estadístico de las variables en xvars
#Estratificado por intervención para comparar, poder evaluar el equilibrio entre el grupo control e intervención (entre sus sujetos)
table1_prematch<- CreateTableOne(vars=xvars,strata="INTERV", data=df, test=FALSE)
print(table1_prematch,smd=TRUE) 





