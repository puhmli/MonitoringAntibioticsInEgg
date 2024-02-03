#Basic package
library("ggplot2")
library(tidyverse)
library(caret)
library(caretEnsemble)
library(dplyr)
###################################################
#Data reading
####################################################
install.packages("readxl")
library(readxl)

#####################################################################################################################################################################################
#Choose data For BN
eggDATA<-read_xlsx("EggDataForBN.xlsx", sheet="EGGBNFINAL0906" )
str(eggDATA)
colnames(eggDATA)
eggDATA_TAN<-cbind(eggDATA$"ProductClass" ,eggDATA$"AnimalSource",eggDATA$"HazardCategory",eggDATA$"Contaminant",
                  eggDATA$"ProdMonth",eggDATA$"ProdYear",eggDATA$"ProdPlace",eggDATA$"FinalMonth",eggDATA$"FinalYear",eggDATA$"SamplingPlace",eggDATA$"SamplingLevel",eggDATA$"ChainPoint")
str(eggDATA_TAN)
is.na(eggDATA_TAN)
eggDATA_TAN<-as.data.frame(eggDATA_TAN)
eggDATA_TAN[is.na(eggDATA_TAN)] <- "ss"
colnames(eggDATA_TAN)<-c("ProductClass","AnimalSource","HazardCategory","Contaminant","ProdM","ProdY","ProdP","FinalM","FinalY","SamplingP","SamplingL","ChainPoint")

eggDATA_TAN <- as.data.frame(unclass(eggDATA_TAN), stringsAsFactors = TRUE)
sapply(eggDATA_TAN, class)

#####################################################################################################################################################################################
#Data clean
##############
#clean variable ProdP
levels(eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('安徽', '1', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('北京', '2', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('不详', '3', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('福建', '4', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('甘肃', '5', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('广东', '6', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('广西', '7', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('贵州', '8', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('海南', '9', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('河北', '10', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('河南', '11', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('黑龙江', '12', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('湖北', '13', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('湖南', '14', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('吉林', '15', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('江苏', '16', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('进口', '17', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('辽宁', '18', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('内蒙古', '19', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('宁夏', '20', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('青海', '21', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('山东', '22', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('山西', '23', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('陕西', '24', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('上海', '25', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('四川', '26', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('天津', '27', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('西藏', '28', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('新疆', '29', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('云南', '30', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('浙江', '31', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('重庆', '32', eggDATA_TAN$ProdP)
eggDATA_TAN$ProdP<- gsub('江西', '33', eggDATA_TAN$ProdP)

#clean variable contaminant
levels(eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Amantadine', '1', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Chloramphenicol', '2', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Ciprofloxacin', '3', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Dimetridazole', '4', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Doxycycline', '5', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Enrofloxacin', '6', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Florfenicol', '7', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('fluoroquinolones', '8', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Furazolidone', '9', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Levofloxacin', '10', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Metronidazole', '11', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Natamycin', '12', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Ofloxacin', '13', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Oxytetracycline', '14', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Pefloxacin', '15', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Sarafloxacin', '16', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Sulfamethoxazole', '17', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Thiamphenicol', '18', eggDATA_TAN$Contaminant)
eggDATA_TAN$Contaminant<- gsub('Tetracyclines', '19', eggDATA_TAN$Contaminant)

#####################################################################
#Set as factor
eggDATA_TAN <- as.data.frame(unclass(eggDATA_TAN), stringsAsFactors = TRUE)
sapply(eggDATA_TAN, class)
##########################################################
#####Select 80% datasets for TAN training
nrow(eggDATA_TAN)
ns<-round(nrow(eggDATA_TAN)*0.8)
set.seed(0)
samp <- sample(nrow(eggDATA_TAN),ns)
egg80 <- eggDATA_TAN[samp,]
egg20 <- eggDATA_TAN[-samp,]
str(egg80 )
str(egg80$antibio)
str(egg80$ProductClass)
str(egg20 )

#####################################################################################################################################################################################
#Building Bayesian network
#####################################################
#bnlearn library
#########################################################
install.packages("bnlearn")
library(bnlearn)
install.packages("BiocManager")
BiocManager::install("Rgraphviz")
library("Rgraphviz")
install.packages("gRbase")
install.packages("gRain")
install.packages("bnclassify")
library("bnclassify")
library("gRbase")
library("gRain")
###########################################################


################################################
#Bayesian structure learning and validation
########################################################################
#
tan = tree.bayes(egg80, "HazardCategory")
fitted = bn.fit(tan, egg80, method = "bayes")
fitted$antibio
str(fitted)
pred = predict(fitted, egg20)
bnclassify::accuracy(pred, egg20$HazardCategory)
table(pred, egg20[, "HazardCategory"]) 
par(mfrow = c(1,1))
graphviz.plot(tan)
cv.nb = bn.cv(data = egg80,tan, runs = 10, method = "k-fold", folds = 10)
##################################################################################
#Check BN performance
confMat <- confusionMatrix(as.factor(pred), as.factor(egg20$HazardCategory))
print(confMat)
##################################################################################
#check structure and variable
str(egg80)
levels(egg80$SamplingP)
levels(egg80$HazardCategory)
levels(egg80$Contaminant)
#################################################################################

##################################################################################
#marginal distribution of each varaible
#####marginal distribution
junction = compile(as.grain(fitted))
ProductClass<-querygrain(junction , nodes = "ProductClass", type = "marginal",
                           evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                           details = 0)
AnimalSource<-querygrain(junction , nodes = "AnimalSource", type = "marginal",
                           evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                           details = 0)
HazardCategory<-querygrain(junction , nodes = "HazardCategory", type = "marginal",
                            evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                            details = 0)
Contaminant<-querygrain(junction , nodes = "Contaminant", type = "marginal",
                          evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                          details = 0)
ProdM<- querygrain(junction , nodes = "ProdM", type = "marginal",
                            evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                            details = 0)
ProdY <- querygrain(junction , nodes = "ProdY", type = "marginal",
                         evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                         details = 0)
FinalM  <- querygrain(junction , nodes = "FinalM", type = "marginal",
                       evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                       details = 0)
FinalY  <- querygrain(junction , nodes = "FinalY", type = "marginal",
                      evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                      details = 0)
ChainPoint  <- querygrain(junction , nodes = "ChainPoint", type = "marginal",
                      evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                      details = 0)
SamplingP  <- querygrain(junction , nodes = "SamplingP", type = "marginal",
                      evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                      details = 0)
SamplingL  <- querygrain(junction , nodes = "SamplingL", type = "marginal",
                      evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                      details = 0)
ProdP <- querygrain(junction , nodes = "ProdP", type = "marginal",
                    evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                    details = 0)
################################################################
#########################################################################################################################################
#monitoring at beijing and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="北京")), method = "ls", n= 10^6)
#for antibiotic contaminant

for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="北京"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#from individual source for all antibiotic 
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="北京"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}

#from individual source for all antibiotic for ('Enrofloxacin', '6')/('Levofloxacin', '10')/('Metronidazole', '11')

#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="北京"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="北京"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="北京"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="北京"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="北京"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}

#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="北京"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="北京"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="北京"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at AnHui and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="安徽")), method = "ls", n= 10^6)
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="安徽"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#from individual source for all antibiotic 
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="安徽"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}

#from individual source for all antibiotic for ('Enrofloxacin', '6')/('Levofloxacin', '10')/('Metronidazole', '11')

#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="安徽"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="安徽"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="安徽"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="安徽"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="安徽"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="安徽"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="安徽"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="安徽"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at FuJian and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="福建")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="福建"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="福建"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="福建"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="福建"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="福建"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="福建"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="福建"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="福建"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="福建"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="福建"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at GanSu and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="甘肃")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="甘肃"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="甘肃"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="甘肃"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="甘肃"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="甘肃"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="甘肃"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="甘肃"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="甘肃"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="甘肃"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="甘肃"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at GuangDong and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="广东")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="广东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="广东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="广东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="广东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="广东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="广东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="广东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="广东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="广东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="广东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at GuangXi and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="广西")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(Contaminant== as.character(i))),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}


set.seed(0)
cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(Contaminant== "1")),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 





#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="广西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at GuiZhou and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="贵州")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="贵州"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="贵州"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="贵州"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="贵州"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((ProdP == as.character(i)&(Contaminant == "11"))),evidence=((SamplingP=="贵州")),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="贵州"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="贵州"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="贵州"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="贵州"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="贵州"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at HaiNan and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="海南")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="海南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="海南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="海南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="海南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="海南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="海南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="海南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="海南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="海南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="海南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at HeBei and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="河北")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="河北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="河北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="河北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="河北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="河北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="河北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="河北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="河北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="河北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="河北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at HeNan and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="河南")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="河南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="河南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="河南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="河南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="河南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="河南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="河南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="河南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="河南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="河南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at HeiLongJiang and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="黑龙江")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="黑龙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="黑龙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="黑龙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="黑龙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="黑龙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="黑龙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="黑龙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="黑龙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="黑龙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="黑龙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at HuBei and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="湖北")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="湖北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="湖北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="湖北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="湖北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="湖北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="湖北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="湖北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="湖北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="湖北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="湖北"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at HuNan and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="湖南")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="湖南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="湖南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="湖南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="湖南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="湖南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="湖南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="湖南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="湖南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="湖南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="湖南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at JiLin and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="吉林")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="吉林"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="吉林"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="吉林"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="吉林"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="吉林"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="吉林"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="吉林"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="吉林"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="吉林"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="吉林"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at JangSu and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="江苏")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="江苏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="江苏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="江苏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="江苏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="江苏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="江苏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="江苏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="江苏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="江苏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="江苏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at JangXi and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="江西")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="江西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="江西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="江西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="江西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="江西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="江西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="江西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="江西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="江西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="江西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at LiaoNing and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="辽宁")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="辽宁"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="辽宁"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="辽宁"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="辽宁"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="辽宁"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="辽宁"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="辽宁"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="辽宁"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="辽宁"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="辽宁"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at NeimengGu and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="内蒙古")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="内蒙古"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="内蒙古"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="内蒙古"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="内蒙古"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="内蒙古"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="内蒙古"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="内蒙古"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="内蒙古"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="内蒙古"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="内蒙古"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at NingXia and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="宁夏")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="宁夏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="宁夏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="宁夏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="宁夏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="宁夏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="宁夏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="宁夏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="宁夏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="宁夏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="宁夏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at QingHai and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="青海")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="青海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="青海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="青海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="青海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="青海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="青海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="青海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="青海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="青海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="青海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at ShanDong and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="山东")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="山东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="山东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="山东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="山东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="山东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="山东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="山东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="山东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="山东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="山东"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at ShanXi and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="山西")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="山西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="山西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="山西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="山西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="山西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="山西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="山西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="山西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="山西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="山西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at ShanXi2 and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="陕西")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="陕西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="陕西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="陕西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="陕西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="陕西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="陕西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="陕西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="陕西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="陕西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="陕西"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at ShangHai and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="上海")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="上海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="上海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="上海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="上海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="上海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="上海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="上海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="上海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="上海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="上海"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at SiChuan and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="四川")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="四川"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="四川"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="四川"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="四川"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="四川"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}

#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="四川"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="四川"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="四川"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="四川"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="四川"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at XiZang and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="西藏")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="西藏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="西藏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="西藏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="西藏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="西藏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="西藏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="西藏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="西藏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="西藏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="西藏"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at XinJiang and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="新疆")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="新疆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="新疆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="新疆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="新疆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="新疆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="新疆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="新疆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="新疆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="新疆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="新疆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at yUNnan and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="云南")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="云南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="云南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="云南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="云南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="云南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="云南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="云南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="云南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="云南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="云南"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at ZheJiang and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="浙江")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="浙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="浙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="浙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="浙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="浙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="浙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="浙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="浙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="浙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="浙江"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
########################################################################################################################################
#monitoring at ChongQing and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="重庆")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="重庆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="重庆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}


for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((ProdP == as.character(i))),evidence=((Contaminant == "6")&(SamplingP=="重庆")),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="重庆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="重庆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="重庆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Sulfamethoxazole', '17')
for (i in 1:33) { 
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="重庆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="重庆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="重庆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="重庆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="重庆"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
##########################################################################################
########################################################################################################################################
#monitoring at TianJin and check all contamination source places#########################################################################
set.seed(0)
cpquery(fitted, event =((HazardCategory == "Antibiotic")),evidence=((SamplingP=="天津")), method = "ls", n= 10^6)
#from individual source
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((HazardCategory == "Antibiotic")&(ProdP == as.character(i))),evidence=(SamplingP=="天津"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#for antibiotic contaminant
for (i in 1:19) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant== as.character(i))),evidence=(SamplingP=="天津"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}



#('Enrofloxacin', '6')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "6")&(ProdP == as.character(i))),evidence=(SamplingP=="天津"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Levofloxacin', '10')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "10")&(ProdP == as.character(i))),evidence=(SamplingP=="天津"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}
#('Metronidazole', '11')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "11")&(ProdP == as.character(i))),evidence=(SamplingP=="天津"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}

#('Sulfamethoxazole', '17')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "17")&(ProdP == as.character(i))),evidence=(SamplingP=="天津"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}

#（'Ofloxacin', '13')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "13")&(ProdP == as.character(i))),evidence=(SamplingP=="天津"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}

#('Ciprofloxacin', '3')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "3")&(ProdP == as.character(i))),evidence=(SamplingP=="天津"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}


#('Doxycycline', '5')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "5")&(ProdP == as.character(i))),evidence=(SamplingP=="天津"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}

#('Florfenicol', '7')
for (i in 1:33) {
 as.factor(i)
 set.seed(0)
 prob= cpquery(fitted, event = ((Contaminant == "7")&(ProdP == as.character(i))),evidence=(SamplingP=="天津"),method = "ls", n= 10^6) 
 result<- c(prob, paste("monitoring at beijing contamination from", i))
 print(result)
}



#####################################################################################################################################
#conditional probability of antibio agents#########################################################################
levels(egg80$Contaminant)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Amantadine")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Dimetridazole")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Doxycycline")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Enrofloxacin")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Frontline")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Furazolidone")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Levofloxacin")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Metronidazole")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Ofloxacin")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Sulfamethoxazole")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
set.seed(0)
cpquery(fitted, event =((Contaminant== "Sulfonamides")),evidence=((antibio == "不合格")), method = "ls", n= 10^6)
##################################################################################################################

