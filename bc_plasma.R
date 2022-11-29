
plasma = read.table("/Users/anab/Documents/MS_UCDavis/STA206/final_proj/Plasma.txt", header = TRUE)
head(plasma)
str(plasma)
plasma$SEX = as.factor(plasma$SEX)
plasma$SMOKSTAT = as.factor(plasma$SMOKSTAT)
plasma$VITUSE = as.factor(plasma$VITUSE)
str(plasma)
names(plasma)[4] = "BMI"
beta_c_plasma = plasma[,1:13]

#change the order of variables, betaplasma 1st and categorical next then numeric
beta_c_plasma = beta_c_plasma[c("BETAPLASMA", "SEX", "SMOKSTAT", "VITUSE", "AGE", "BMI",
                                "CALORIES","FAT", "FIBER","ALCOHOL","CHOLESTEROL" ,
                                "BETADIET", "RETDIET")] 


head(beta_c_plasma)
str(beta_c_plasma)

summary(beta_c_plasma[1:4])
summary(beta_c_plasma[5:10])
summary(beta_c_plasma[11:13])

names(beta_c_plasma) =tolower(names(beta_c_plasma))
head(beta_c_plasma)
bplasma = subset(beta_c_plasma, betaplasma!=0)
#-------------------------------diagnostics--------------------------------------------
initial_model = lm(betaplasma~.^2, data = bplasma)
summary(model)
length(model$residuals)

par(mfrow = c(1,2))
#check to see if data is normally distributed
qqnorm(initial_model$residuals)
qqline(initial_model$residuals)

#hist of errors
hist(initial_model$residuals, main = "Residuals", xlab = "ei",
     pch = 16,cex = 1.25, col = "white")

ei= initial_model$residuals
the.SWtest = shapiro.test(ei)
the.SWtest

plot(initial_model$fitted.values, initial_model$residuals, 
     main = "Errors vs. Fitted Values",xlab = "Fitted Values",
     ylab = "Errors") 
abline(h = 0,col = "purple")

Group = rep("Lower",nrow(bplasma)) #Creates a vector that repeats "Lower" n times
Group[bplasma$betaplasma > median(bplasma$betaplasma)] = "Upper" #Changing the appropriate values to "Upper"
Group = as.factor(Group) #Changes it to a factor, which R recognizes as a grouping variable.
bplasma$Group = Group
the.FKtest= fligner.test(initial_model$residuals, bplasma$Group)
the.FKtest
bplasma$Group = NULL


boxCox1 = boxcox(initial_model, plotit = TRUE)
lambdaBC = boxCox1$x[which.max(boxCox1$y)]
lambdaBC

#### --------------------------check assumptions are met on new transformed--------------------

newY1 = data.frame(log(bplasma$betaplasma))
names(newY1) = "Y" # this is really just lnY
dataTrans = cbind(newY1,bplasma)
head(dataTrans)
dataTrans$betaplasma = NULL

model_after_transformation = lm(Y~.^2 , data = dataTrans)

#sw and fk test
ei= model_after_transformation$residuals
the.SWtest = shapiro.test(ei)
the.SWtest

Group = rep("Lower",nrow(dataTrans)) #Creates a vector that repeats "Lower" n times
Group[dataTrans$Y > median(dataTrans$Y)] = "Upper" #Changing the appropriate values to "Upper"
Group = as.factor(Group) #Changes it to a factor, which R recognizes as a grouping variable.
dataTrans$Group = Group
the.FKtest= fligner.test(model_after_transformation$residuals, dataTrans$Group)
the.FKtest
dataTrans$Group = NULL

#------ plots 

par(mfrow=  c(2,2)) 
qqnorm(model_after_transformation$residuals, pch = 19,cex = 1.25)
qqline(model_after_transformation$residuals,lwd = 2, col = "purple")
hist(model_after_transformation$residuals, main = "Residuals", xlab = "ei",pch = 16,cex = 1.25,col="white")
plot(model_after_transformation$fitted.values, model_after_transformation$residuals, 
     main = "Errors vs. Fitted Values", xlab = "Fitted Values",ylab = "Errors")
abline(h = 0,col = "purple")

#-------------------------------check for outliers and leverage  ----------------------------#######
library(leaps)
library(MPV)

ei.s = model_after_transformation$residuals/sqrt(sum(model_after_transformation$residuals^2)/
                                                   (nrow(dataTrans) - length(model_after_transformation$coefficients)))

ri = rstandard(model_after_transformation)
ti = rstudent(model_after_transformation)

alpha = 0.1
n= nrow(dataTrans)
p = length(model_after_transformation$coefficients)
cutoff = qt(1-alpha/(2*n), n-p )
cutoff.deleted = qt(1-alpha/(2*n), n -p -1 )

outliers = which(abs(ei.s) > cutoff | abs(ri) > cutoff | abs(ti) > cutoff.deleted)

dataTrans[outliers,] #remove 39

dataTrans = dataTrans[!(row.names(dataTrans) %in% c('39')), ]
rownames(dataTrans) = seq(length=nrow(dataTrans))

#refit the original model with outliers removed 
model_after_transformation = lm(Y~.^2 , data = dataTrans)

###------------------------- leverage pts cooks dist -----------------------------------

h = influence(model_after_transformation)$hat
sort(h[which(h>2*p/n)], decreasing = TRUE)

lev.hat = which(all.values[,"hat"] >2*p/n)
dataTrans[lev.hat,]

res = model_after_transformation$residuals
mse = anova(model_after_transformation)["Residuals", 3]
cook.d = res^2*h/(p*mse*(1-h)^2)
sort(cook.d[which(cook.d>1)], decreasing = T)

#using cooks distance
all.values = influence.measures(model_after_transformation)$infmat
lev.DI = which(all.values[,"cook.d"] >1 )
dataTrans[lev.DI,]

par(mfrow = c(1,1))
plot(model_after_transformation, which=4)

which(rownames(dataTrans)=="61")

fit=lm(Y ~.^2, data=dataTrans, subset=setdiff(rownames(dataTrans), "61")) 
rbind(model_after_transformation$coefficients,fit$coefficients)

##compare fitted regression coefficients
#since there is no difference beween the  there is little difference in these two fits, so case 62 can be retained.
plot(model_after_transformation$fitted.value, predict(fit, dataTrans), xlab="Fitted values all cases", ylab="Fitted values w/o case 61") ## compare fitted values
abline(0,1)

dataTrans = dataTrans[!(row.names(dataTrans) %in% c('61')), ]
rownames(dataTrans) = seq(length=nrow(dataTrans))

# ---------------------------------- final model ----------------
#---------------------------prediction intervals
final_model = lm(formula = Y ~ bmi + vituse, data = dataTrans)
summary(final_model)

mult.fun = function(n,p,g,alpha){
  bon = qt(1-alpha/(2*g), n-p)
  WH = sqrt(p*qf(1-alpha,p,n-p))
  Sch = sqrt(g*qf(1-alpha,g,n-p))
  all.mul = c(bon,WH,Sch)
  all.mul = round(all.mul,3)
  names(all.mul) = c("Bon","WH","Sch")
  return(all.mul)
}

mult.CI = function(C.star,x.stars,the.model,alpha,the.type = "confidence"){
  all.preds = predict(the.model,x.stars)
  if(the.type == "confidence"){
    all.se = predict(the.model,x.stars,interval = the.type,se.fit = TRUE)$se.fit
  } else if(the.type == "prediction"){
    all.se = predict(the.model,x.stars,interval = the.type,se.fit = TRUE)$se.fit
    MSE = sum(the.model$residuals^2)/(length(the.model$residuals) - length(the.model$coefficients))
    all.se = sqrt(all.se^2 + MSE)
  }
  LB = all.preds - C.star*all.se
  UB = all.preds + C.star*all.se
  all.CIs = cbind(LB,UB)
  colnames(all.CIs) = paste((1-alpha)*100, "%",c(" Lower"," Upper"), sep = "")
  results = cbind(all.preds,all.CIs)
  colnames(results)[1] = "Estimate"
  return(results)
}

all.of.them = mult.fun(nrow(dataTrans), length(final_model$coefficients), 3, 0.05)
all.of.them

xs = data.frame(bmi=c(27,19,31),vituse = c("NO","OFTEN","NOT OFTEN"))
all.the.CIs = mult.CI(all.of.them[1], xs, final_model,0.05,"prediction")
all.the.CIs
cbind(xs,all.the.CIs)











