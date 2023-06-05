#######  PSM DATOS SINTÉTICOS n700000 #####


#CARGA Y PREPARACIÓN DE LOS DATOS

install.packages("readxl")
library("readxl")

fakePatients_interv <- read_excel("~/Desktop/Datasets pruebas PSM/fakePatients_interv.xlsx")
View(fakePatients_interv)
str(fakePatients_interv)

fakePatients_ctrl_n <- read_excel("~/Desktop/Datasets pruebas PSM/fakePatients_ctrl_n700000.xlsx")
View(fakePatients_ctrl_n)
str(fakePatients_ctrl_n)

#Eliminar columna índice
fakePatients_interv_df = subset(fakePatients_interv, select = -...1)
View(fakePatients_interv_df)
fakePatients_ctrl_n_df = subset(fakePatients_ctrl_n, select = -...1)
str(fakePatients_ctrl_n_df)

#Merge datasets
fakePatients_both_n <- rbind(fakePatients_interv_df, fakePatients_ctrl_n_df)


#EVALUACIÓN DEL EQUILIBRIO

install.packages("tableone") #create the tableones
install.packages("Matching")

library(tableone)
colnames(fakePatients_both_n)

#lista de covariables a utilizar
xvars<-c("edad","sexo","ic","acv","sca","iam_k75","dm","ir","pas","pad")

#table 1
table1_prematch<- CreateTableOne(vars=xvars,strata="interv", data=fakePatients_both_n, test=FALSE)
print(table1_prematch,smd=TRUE) #include standardized mean difference (SMD)

########## export TableOne
library(readr)
tab1Mat_prematch <- print(table1_prematch, quote = FALSE, noSpaces = TRUE,smd=TRUE)
## Save to a CSV file
write.csv(tab1Mat_prematch, file = "table1_prematch.csv")
table1_prematch_df <- read_csv("table1_prematch.csv")

install.packages("writexl")
library("writexl")
write_xlsx(table1_prematch_df,"~/Desktop/pruebasR-PSM/tableones-PSM.xlsx", col_names = TRUE, format_headers = TRUE)
##############


#AJUSTAR EL MODELO DE PSM

#glm
psmodel<-glm(interv~edad+sexo+ic+acv+sca+iam_k75+dm+ir+pas+pad,
             family=binomial(), 
             data=fakePatients_both_n) 
summary(psmodel)

pscore<-psmodel$fitted.values

h1 = hist(pscore[fakePatients_both_n$interv==1], plot=FALSE,breaks=20)
h2 = hist(pscore[fakePatients_both_n$interv==0], plot=FALSE,breaks=20)
h2$counts = - h2$counts
hmax = max(h1$counts)
hmin = min(h2$counts)
X = c(h1$breaks, h2$breaks)
xmax = max(X)
xmin = min(X)
plot(h1, ylim=c(hmin, hmax), col="Red", xlim=c(xmin, xmax),
     xlab='propensity score',main="",yaxt='n',ylab='')
lines(h2, col="blue")


#MATCHING

#using MatchIt for PSM
install.packages("MatchIt")
library(MatchIt)

#MatchIt PSM, optimal matching (only if reasonably sized dataset --> NOT THE CASE!)
m.out<- matchit(interv~edad+sexo+ic+acv+sca+iam_k75+dm+ir+pas+pad,
                data=fakePatients_both_n, 
                method = "optimal")

#Error: vector memory exhausted (limit reached?)

# 1:1 nearest neighbor matching with replacement on 
# the Mahalanobis distance
m.outMah <- matchit(interv~edad+sexo+ic+acv+sca+iam_k75+dm+ir+pas+pad, 
                    data = fakePatients_both_n, 
                    distance = "robust_mahalanobis")
R.version()
help(matchit)
packageVersion("MatchIt")
m.outMah
summary(m.outMah)

#Plot balance
plot(summary(m.outMah))

plot(m.outMah, type = "qq", interactive = FALSE,
     which.xs = c("edad", "sexo", "ic","acv", "pas","pad"))
plot(m.outMah, type = "histogram")
#density plot
plot(m.outMah, type = "density", interactive = FALSE,
     which.xs = ~edad + sexo + icv)


#MatchIt PSM, nearest neighbor matching
m.out<- matchit(interv~edad+sexo+ic+acv+sca+iam_k75+dm+ir+pas+pad,
                data=fakePatients_both_n, 
                method = "nearest") #nearest neighbor=greedy matching // optimal:only if reasonably sized dataset
summary(m.out)

#PS plots
plot(m.out,type = "jitter",interactive = FALSE)
plot(m.out,type = "hist")


######OPCIONAL
install.packages('cobalt')
library('cobalt')
#Density plots for continuous variables
bal.plot(m.out, var.name = "pas", which = "both")
bal.plot(m.out, var.name = "pad", which = "both",
         type = "histogram",mirror = TRUE)
#Mirrored histogram, for distance
bal.plot(m.out, var.name = "distance", which = "both",
         type = "histogram", mirror = TRUE)

#########

###### BENCHMARKING ####
startTime <- Sys.time()
### (función a medir) 
endTime <- Sys.time()
print(endTime - startTime)
########################

#greedy matching on logit(PS) CON CALIBRE
library(Matching)
help(Matching)

logit <- function(p) {log(p)-log(1-p)}

psmatch<-Match(Tr=fakePatients_both_n$interv,M=1,X=logit(pscore),replace=FALSE,
               caliper=.2) 
matched<-fakePatients_both_n[unlist(psmatch[c("index.treated","index.control")]), ] 

#visualizar SMD
table1_matched<-CreateTableOne(vars=xvars, strata ="interv", 
                               data=matched, test = FALSE)
print(table1_matched, smd = TRUE)

print(table1_prematch, smd = TRUE)

########## export TableOne
tab1Mat_matched <- print(table1_matched, quote = FALSE, noSpaces = TRUE,smd=TRUE)
## Save to a CSV file
write.csv(tab1Mat_matched, file = "table1_matched.csv")
table1_matched_df <- read_csv("table1_matched.csv")
write_xlsx(table1_matched_df,"~/Desktop/pruebasR-PSM/tableones2-PSM.xlsx", col_names = TRUE, format_headers = TRUE)

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


