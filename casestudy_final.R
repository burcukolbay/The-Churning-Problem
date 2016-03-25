library(ggplot2)
library("FactoMineR")
churn<-read.table("C:/Users/ASUS/Desktop/csbarna/churn2.txt",header = TRUE)
churn$antig <- ifelse(churn$antig > 90, NaN, churn$antig)
churn$total_vista<-NA
churn$total_vista<-churn$Total_pasivo-(churn$Total_Plazo+churn$Total_Inversion+churn$Total_Seguros)

table(churn$antig)
sum(is.na(churn))
summary(churn)
new_DF <- churn[rowSums(is.na(churn)) > 0,]

library(mice)
addit1<-(md.pattern(churn))
churn2 <- mice(churn)
churn2 <- complete(churn2)
summary(churn2)
sum(is.na(churn2))
new_DF2 <- churn2[rownames(churn2) %in% rownames(new_DF),]
cat <- catdes(churn2,num.var=1)


##########CATEGORICAL VARIABLES#################

library(grid)
install.packages("gridExtra")
library(gridExtra)
install.packages("cowplot")
library(cowplot)
install.packages("Rmisc")
library(Rmisc)
#baja
ggp1<-ggplot(data.frame(churn2[,1]),aes(x=churn2[,1]))
ggp1<-ggp1+geom_histogram(fill="lightgreen")

#edatcat
ggp2<-ggplot(data.frame(churn2[,2]),aes(x=churn2[,2]))
ggp2<-ggp2+geom_histogram(fill="lightgreen")

#sexo
ggp3<-ggplot(data.frame(churn2[,3]),aes(x=churn2[,3]))
ggp3<-ggp3+geom_histogram(fill="lightgreen")

#nomina
ggp4<-ggplot(data.frame(churn2[,5]),aes(x=churn2[,5]))
ggp4<-ggp4+geom_histogram(fill="lightgreen")

#pension
ggp5<-ggplot(data.frame(churn2[,6]),aes(x=churn2[,6]))
ggp5<-ggp5+geom_histogram(fill="lightgreen")

#debito normal
ggp6<-ggplot(data.frame(churn2[,7]),aes(x=churn2[,7]))
ggp6<-ggp6+geom_histogram(fill="lightgreen")

##Debito_aff
ggp7<-ggplot(data.frame(churn2[,8]),aes(x=churn2[,8]))
ggp7<-ggp7+geom_histogram(fill="lightgreen")

##VISA
ggp8<-ggplot(data.frame(churn2[,9]),aes(x=churn2[,9]))
ggp8<-ggp8+geom_histogram(fill="lightgreen")

##VISA_aff
ggp9<-ggplot(data.frame(churn2[,10]),aes(x=churn2[,10]))
ggp9<-ggp9+geom_histogram(fill="lightgreen")

##MCard
ggp10<-ggplot(data.frame(churn2[,11]),aes(x=churn2[,11]))
ggp10<-ggp10+geom_histogram(fill="lightgreen")

##Amex
ggp11<-ggplot(data.frame(churn2[,12]),aes(x=churn2[,12]))
ggp11<-ggp11+geom_histogram(fill="lightgreen")

##ind_resid
ggp12<-ggplot(data.frame(churn2[,30]),aes(x=churn2[,30]))
ggp12<-ggp12+geom_histogram(fill="lightgreen")

##############QUANTITATIVE VARIABLES########################## 

#antig
table(churn2$antig)
quantile(churn2$antig)
# xr_antig<-cut(churn2$antig,breaks=c(-0.0001,0.0001,3,13,18,20,69))
xr_antig<-cut(churn2$antig,breaks=c(-0.0001,13,18,20,69))
table(xr_antig)
tab <- table(churn2[,1],xr_antig)
barplot(tab[2,]/colSums(tab),main="antig")
prop.table(table(churn2[,1],xr_antig),2)

#imp_caj_int
table(churn2$Imp_caj_int)
#all of the values are 0. it is not important.

#num_caj_int
table(churn2$Num_caj_int)
#all of the values are 0. it is not important

#imp_caj_nac
table(churn2$Imp_caj_nac)
#1993 0

#num_caj_nac
table(churn2$Num_caj_nac)
#all of the values are zero. not important

#imp_caj_pro
table(churn2$Imp_caj_pro)
quantile(churn2$Imp_caj_pro)
xr_Imp_caj_pro<-cut(churn2$Imp_caj_pro,breaks=c(-4,-0.0001,0.0001,80.5))
table(xr_Imp_caj_pro)
tab <- table(churn2[,1],xr_Imp_caj_pro)
barplot(tab[2,]/colSums(tab),main="imp_caj_pro")
prop.table(table(churn2[,1],xr_Imp_caj_pro),2)

#num_caj_pro
table(churn2$Num_caj_pro)
#1996 0

#Imp_ven_int
table(churn2$Imp_ven_int)
#1991 0

#num_ven_int
table(churn2$Num_ven_int)
#all of the variables are zero. not important.

#imp_ven_nac
table(churn2$Imp_ven_nac)
#all of the variables are zero. not important.

#num_ven_nac
table(churn2$Num_ven_nac)
#all of the variables are zero. not important.

#imp_ven_pro
table(churn2$Imp_ven_pro)
#1991 0

#num_ven_pro
table(churn2$Num_ven_pro)
#all of the variables are zero. not important.

#Total_pasivo
table(churn2$Total_pasivo)
quantile(churn2$Total_pasivo)
xr_Total_pasivo<-cut(churn2$Total_pasivo,breaks=c(-0.0001,0.0001,106,499.5,2984.75,71483))
table(xr_Total_pasivo)
tab <- table(churn2[,1],xr_Total_pasivo)
barplot(tab[2,]/colSums(tab),main="Total_pasivo")
prop.table(table(churn2[,1],xr_Total_pasivo),2)

#Total_activo
table(churn2$Total_activo)
quantile(churn2$Total_activo)
xr_Total_activo<-cut(churn2$Total_activo,breaks=c(-0.0001,0.0001,32772))
table(xr_Total_activo)
tab <- table(churn2[,1],xr_Total_activo)
barplot(tab[2,]/colSums(tab),main="Total_activo")
prop.table(table(churn2[,1],xr_Total_activo),2)

#Total_Plazo
table(churn2$Total_Plazo)
quantile(churn2$Total_Plazo)
xr_Total_Plazo<-cut(churn2$Total_Plazo,breaks=c(-0.0001,0.0001,472.25,43400))
table(xr_Total_Plazo)
tab <- table(churn2[,1],xr_Total_Plazo)
barplot(tab[2,]/colSums(tab),main="Total_Plazo")
prop.table(table(churn2[,1],xr_Total_Plazo),2)

#Total_Inversion
table(churn2$Total_Inversion)
quantile(churn2$Total_Inversion)
xr_Total_Inversion<-cut(churn2$Total_Inversion,breaks=c(-0.0001,0.0001,62017))
table(xr_Total_Inversion)
tab <- table(churn2[,1],xr_Total_Inversion)
barplot(tab[2,]/colSums(tab),main="Total_inversion")
prop.table(table(churn2[,1],xr_Total_Inversion),2)

#Total_Seguros
table(churn2$Total_Seguros)
quantile(churn2$Total_Seguros)
xr_Total_Seguros<-cut(churn2$Total_Seguros,breaks=c(-0.0001,0.0001,45445))
table(xr_Total_Seguros)
tab <- table(churn2[,1],xr_Total_Seguros)
barplot(tab[2,]/colSums(tab),main="Total_Seguros")
prop.table(table(churn2[,1],xr_Total_Seguros),2)

#ind_Libreta_caj
table(churn2$ind_Libreta_caj)
quantile(churn2$ind_Libreta_caj)
# xr <- cut(churn2$ind_Libreta_caj,breaks=c(-774.75,-55,-4.5,-0.0001,0.0001,62.5,1157.50))
xr <- cut(churn2$ind_Libreta_caj,breaks=c(-774.75,-50,-0.0001,0.0001,1157.50))
table(xr)
x_ind_Libreta_caj <- xr
tab <- table(churn2[,1],xr)
barplot(tab[2,]/colSums(tab),main="ind_Libreta_caj")
prop.table(table(churn2[,1],xr),2) #to check



#ind_Libreta_ven
table(churn2$ind_Libreta_ven)
quantile(churn2$ind_Libreta_ven)
# x_ind_Libreta_ven <- cut(churn2$ind_Libreta_ven,breaks=c(-5038.6700,-51.5625,-0.0001,0.0001,6.75,6378.2600))
xr <- cut(churn2$ind_Libreta_ven,breaks=c(-5038.6700,-0.0001,0.0001,40,6378.2600))
table(xr)
x_ind_Libreta_ven <- xr
tab <- table(churn2[,1],xr)
barplot(tab[2,]/colSums(tab),main="ind_Libreta_ven")
prop.table(table(churn2[,1],xr),2)

#ind_CC
table(churn2$ind_CC)
quantile(churn2$ind_CC)
x_ind_CC <- cut(churn2$ind_CC,breaks=c(-3312.540,-0.0001,0.0001,9715.28))
table(x_ind_CC)
tab <- table(churn2[,1],x_ind_CC)
barplot(tab[2,]/colSums(tab),main="ind_CC")
prop.table(table(churn2[,1],x_ind_CC),2)

#ind_Vista
table(churn2$ind_Vista)
quantile(churn2$ind_Vista)
x_ind_Vista <- cut(churn2$ind_Vista,breaks=c(-11811.9,-150,-0.0001,0.0001,12737)) #with pattern
table(x_ind_Vista)
tab <- table(churn2[,1],x_ind_Vista)
barplot(tab[2,]/colSums(tab),main="ind_Vista")
prop.table(table(churn2[,1],x_ind_Vista),2)

#ind_Plazo
table(churn2$ind_Plazo)
x_ind_Plazo <- cut(churn2$ind_Plazo,breaks=c(-15000,-0.0001,0.0001,27000)) 
table(x_ind_Plazo)
tab <- table(churn2[,1],x_ind_Plazo)
barplot(tab[2,]/colSums(tab),main="ind_Plazo")
prop.table(table(churn2[,1],x_ind_Plazo),2)

#ind_Ahorro
table(churn2$ind_Ahorro)
x_ind_Ahorro <- cut(churn2$ind_Ahorro,breaks=c(-24208,-0.0001,0.0001,4008))
table(x_ind_Ahorro)
tab <- table(churn2[,1],x_ind_Ahorro)
barplot(tab[2,]/colSums(tab),main="ind_Ahorro")
prop.table(table(churn2[,1],x_ind_Ahorro),2)

#ind_Largo_plazo
table(churn2$ind_Largo_plazo)
quantile(churn2$ind_Largo_plazo)
#x_ind_Largo_plazo <- cut(churn2$ind_Largo_plazo,breaks=c(-15913.04,11,-0.0001,0.0001,503.75,10071))
x_ind_Largo_plazo <- cut(churn2$ind_Largo_plazo,breaks=c(-15913.04,-0.0001,0.0001,10071))
tab <- table(churn2[,1],x_ind_Largo_plazo)
barplot(tab[2,]/colSums(tab),main="ind_Largo_plazo")
prop.table(table(churn2[,1],x_ind_Largo_plazo),2)

#ind_Fondos_inv
table(churn2$ind_Fondos_inv)
quantile(churn2$ind_Fondos_inv)
x_ind_Fondos_inv <- cut(churn2$ind_Fondos_inv,breaks=c(-7746.06,-0.0001,0.0001,62017))
tab <- table(churn2[,1],x_ind_Fondos_inv)
barplot(tab[2,]/colSums(tab),main="ind_Fondos_inv")
prop.table(table(churn2[,1],x_ind_Fondos_inv),2)

#ind_Seguros
table(churn2$ind_Seguros)
quantile(churn2$ind_Seguros)
x_ind_Seguros <- cut(churn2$ind_Seguros,breaks=c(-3905.05,-0.0001,0.0001,19461))
tab <- table(churn2[,1],x_ind_Seguros)
barplot(tab[2,]/colSums(tab),main="ind_Seguros")
prop.table(table(churn2[,1],x_ind_Seguros),2)

#ind_Planes_pension
table(churn2$ind_Planes_pension)
quantile(churn2$ind_Planes_pension)
x_ind_Planes_pension <- cut(churn2$ind_Planes_pension,breaks=c(-8246.55,-0.0001,0.0001))
table(x_ind_Planes_pension)
tab <- table(churn2[,1],x_ind_Planes_pension)
barplot(tab[2,]/colSums(tab),main="ind_Planes_pension")
prop.table(table(churn2[,1],x_ind_Planes_pension),2)

#ind_Hipoteca
table(churn2$ind_Hipoteca)
quantile(churn2$ind_Hipoteca)
x_ind_Hipoteca <- cut(churn2$ind_Hipoteca,breaks=c(-26654,-0.0001,0.0001,32772))
table(x_ind_Hipoteca)
tab <- table(churn2[,1],x_ind_Hipoteca)
barplot(tab[2,]/colSums(tab),main="ind_Hipoteca")
prop.table(table(churn2[,1],x_ind_Hipoteca),2)

#ind_Prest_personales
table(churn2$ind_Prest_personales)
quantile(churn2$ind_Prest_personales)
x_ind_Prest_personales <- cut(churn2$ind_Prest_personales,breaks=c(-8676,-0.0001,0.0001,6741))
table(x_ind_Prest_personales)
tab <- table(churn2[,1],x_ind_Prest_personales)
barplot(tab[2,]/colSums(tab),main="ind_Prest_personales")
prop.table(table(churn2[,1],x_ind_Prest_personales),2)

#modification
#to get the normal pasivo
quantile(churn2$total_vista)
xr_total_vista<-cut(churn2$total_vista,breaks=c(-0.0001,0.0001,51.75,206,657,12738))
table(xr_total_vista)
tab<-table(churn2[,1],xr_total_vista)
barplot(tab[2,]/colSums(tab),main="total_vista")
prop.table(table(churn2[,1],xr_total_vista),2)
#removing columns

delete <- c("Imp_caj_int", "Num_caj_int","Imp_caj_nac","Num_caj_nac","Num_caj_pro","Num_ven_int","Imp_ven_nac","Num_ven_nac","Imp_ven_pro","Num_ven_pro","Imp_ven_int")
churn2 <- churn2[, !(colnames(churn2) %in% delete), drop=FALSE] 

#new data frame
churn3<-data.frame(churn2$Baja,
          churn2$edatcat,
          churn2$sexo,
          churn2$antig,
          churn2$Nomina,
          churn2$Pension,
          churn2$Debito_normal,
          churn2$Debito_aff,
          churn2$VISA,
          churn2$VISA_aff,
          churn2$MCard,
          churn2$Amex,
          xr_Imp_caj_pro,
          xr_Total_pasivo,
          xr_Total_activo,
          xr_Total_Plazo,
          xr_Total_Inversion,
          xr_Total_Seguros,
          xr_total_vista,        
          churn2$ind_resid,
          x_ind_Libreta_caj,
          x_ind_Libreta_ven,
          x_ind_CC,
          x_ind_Vista,
          x_ind_Plazo,
          x_ind_Ahorro,
          x_ind_Largo_plazo,
          x_ind_Fondos_inv,
          x_ind_Seguros,
          x_ind_Planes_pension,
          x_ind_Hipoteca,
          x_ind_Prest_personales
          )
smp_size <- floor(0.8 * nrow(churn3))
set.seed(123)
train_ind <- sample(seq_len(nrow(churn3)), size = smp_size)
train <- churn3[train_ind, ]
test <- churn3[-train_ind, ]
library(caret)
library(rpart)
install.packages("e1071")
library(e1071)
########################DECISION TREE#####################################
fit.b<-rpart(train$churn2.Baja~.,method="class", data=train,control=rpart.control(minbucket=20, cp=0.0001, xval=10))
fit.b$cp
fit.b1<-rpart(train$churn2.Baja~.,method="class", data=train,control=rpart.control(minbucket=20, cp=0.002750000))

pred <- predict(fit.b1, test, type="class")
confusionMatrix(pred, test[,1])

pred1<-predict(fit.b,test,type="class")
confusionMatrix(pred1,test[,1])

#######################LOGISTIC REGRESSION###############################
model <- glm(churn2.Baja ~.,family=binomial(link='logit'),data=train)
AIC(model)
model<-step(model) 
summary(model)



# Interpreting results 
anova(model, test="Chisq")

library(pscl)
pR2(model)

n<-ncol(churn3)
# Testing 


fitted.results.pred <- predict(model,newdata=subset(test,select=c(2:n)),type='response')
fitted.results <- ifelse(fitted.results.pred > 0.6, 1 ,0) # Check threshold 0.6 and 0.5 conf.matrix
count(fitted.results)
test.churn2.baja<- as.numeric(test$churn2.Baja)
test.churn2.baja[test.churn2.baja == 1] <- 0 #Baja No
test.churn2.baja[test.churn2.baja == 2] <- 1 #Baja SI

library(caret)
#install.packages("caret")
confusionMatrix(fitted.results, test.churn2.baja)


##################### RForest ####################################
library(randomForest)

summary(train)

rf_model <- randomForest(churn2.Baja ~ .,   data=train,na.action = na.omit) # Omit NA cases 
attributes(rf_model) # view results 
tt<-rf_model$confusion[,1:2]
error_rate.test <- 100*(1-sum(diag(tt))/sum(tt))
error_rate.test


rf_pred <- predict(rf_model, newdata=subset(test,select=c(2:n))) # Testing 
tt <- table(test$churn2.Baja,rf_pred)
error_rate.test <- 100*(1-sum(diag(tt))/sum(tt))
error_rate.test

confusionMatrix(rf_pred,test$churn2.Baja)


#### Categorical Variable Analysis with Interaction ####
source("Xcatdes.R")
colnames(churn3) <- gsub("churn2.","",colnames(churn3))

summary2 <- Xcatdes(targettable=churn3,targetcolnum=1,degree=2)
top_6NO2 <- head(summary2[summary2$target=="Baja NO",])
top_6SI2 <- head(summary2[summary2$target=="Baja SI",])

summary3 <- Xcatdes(targettable=churn3,targetcolnum=1,degree=3)
top_6NO3 <- head(summary3[summary3$target=="Baja NO",])
top_6SI3 <- head(summary3[summary3$target=="Baja SI",])
