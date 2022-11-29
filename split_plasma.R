
## 55%split
##########################60% train 
set.seed(100)
n = nrow(dataTrans) #312
ind = sample(1:n, n*0.55, replace=FALSE)
train = dataTrans[ind, ] #training set
valid = dataTrans[-ind, ] #validation/test set

full.model = lm(Y~.^2, data = train)
empty.model = lm(Y ~ 1, data = train)

FB.model.BIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), k = log(n),trace=FALSE,direction = "both")
FB.model.BIC


#FB.model.AIC = stepAIC(empty.model, scope = list(lower = empty.model, upper= full.model), k = 2,direction = "both",trace = FALSE)
#BF.model.BIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), k = log(n),trace=FALSE,direction = "both")


train1 = lm(formula = Y ~ bmi + vituse, data = train)
valid1 = lm(formula = Y ~ bmi + vituse, data = valid)

train_est = summary(train1)$coefficients[,1]
valid_estim = summary(valid1)$coefficients[,1]
train_se = summary(train1)$coefficients[,2]
valid_se = summary(valid1)$coefficients[,2]
mod_sum = cbind(train_est, valid_estim, train_se, valid_se )
colnames(mod_sum) = c("Train Est","Valid Est","Train s.e.","Valid s.e.")
mod_sum
mod_sum[,1] - mod_sum[,2] #diff train est - test est
mod_sum[,3] - mod_sum[,4] #train se - test se

sse_t = sum(train1$residuals^2)
sse_v = sum(valid1$residuals^2)
Radj_t = summary(train1)$adj.r.squared
Radj_v = summary(valid1)$adj.r.squared
train_sum = c(sse_t,Radj_t)
valid_sum = c(sse_v,Radj_v)
criteria = rbind(train_sum,valid_sum)
colnames(criteria) = c("SSE","R2_adj")
criteria
criteria[1,] - criteria[2,]

newdata = valid[, -1]
y.hat = predict(train1, newdata)
MSPE = mean((valid$Y - y.hat)^2)
MSPE
MSPE - sse_t/n







