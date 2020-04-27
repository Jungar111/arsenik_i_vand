library(mgcv)
library(ggplot2)
# Frederik wd
# setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\ArsenikGit\\Data")
# Asger wd
#setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i GIT/Data")
# Joachim wd
setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")
#Oskar wd 
# setwd("C:\\Users\\User\\OneDrive - Danmarks Tekniske Universitet\\SAS_030919\\4. Semester\\42584_Fagprojekt\\Arsenik i drikkevand\\42584_Data\\arsenik_i_vand\\Data")
set.seed(69)

############### INDLÆSNING AF LUNGE DATA #################
flun <- read.table("flun.txt", header=TRUE)
mlun <- read.table("mlun.txt", header=TRUE)
flun$gender <- "Female"
mlun$gender <- "Male"
flun$female <- 1
mlun$female <- 0
lun <- rbind(flun,mlun)
lun$village1 <- 1*(lun$group == 1)
# Antal observationer
N <- length(lun$events)
# p.hat (empirisk sandsynlighed)
p.hat <- sum(lun$events/N)


################ General Additive Model / GLM / Model-definering ###################
# analysis <- glm(events~ age + I(age^2) + conc + gender*village1, family=poisson(link= "log"), data=lun, offset=log(at.risk))
analysis <- gam(events~s(age) + s(age,by=female) + s(conc) + gender*village1
                + offset(I(log(at.risk))),
                family=poisson(link = "log"),
                data=lun)
summary(analysis)
AIC(analysis)

# RESIDUAL PLOT ---> Først indbygget funktion, herefter manuel!
par(mfrow=c(2,1))
plot(analysis$residuals, col=lun$group, main = "Indbygget R funktion")
plot(analysis$fitted.values, (lun$events - analysis$fitted.values) / sqrt(analysis$fitted.values), col=lun$group, main = "Manuel beregning")
par(mfrow=c(1,1))
## Kan det forsvares dette plot? Og hvor mange er ude for vores konfidensinterval ud af 1118?
length(((lun$events - analysis$fitted.values) / sqrt(analysis$fitted.values))[(lun$events - analysis$fitted.values)/sqrt(analysis$fitted.values) > 4])
1 - pnorm(4)^(1118 - 12)

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis, se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

# Transformerer dataen tilbage til original tilstand og sorterer data efter pred
prediction.data.original <- exp(prediction.data)
prediction.data.original <- prediction.data.original[order(lun$events, decreasing = TRUE), ]


############### PLOT LUN #################

maxr <- 2

# PLOTTETID
plot(round(prediction.data.original$pred, digits=2), lun$events[order(lun$events, decreasing = TRUE)], xlim=c(0, maxr), ylim=c(0, maxr), xlab="Predicted events", ylab="Actual events")
lines(0:maxr, 0:maxr, type="l")
sd.Pred <- sd(prediction.data.original$pred[0:maxr])
lines(0:maxr+sd.Pred, 0:maxr, type="l", col = "red")
lines(0:maxr-sd.Pred, 0:maxr, type="l", col = "red")

v = vector()
x = vector()

# lun$events <- lun$events[order(lun$events, decreasing = TRUE)]

for (i in 1:length(prediction.data.original$pred)){
  res <- mean(lun$events[0.02*(i-1) <= prediction.data.original$pred & prediction.data.original$pred < 0.02*i])
  v = c(v, res)
  x = c(x, 0.02*i)}

plot(x, v, xlim=c(0, maxr), ylim=c(0, maxr), xlab="Average predicted events", ylab="Average actual events")
lines(0:maxr,0:maxr, type="l")


###################### RISK VS AGE PLOT - DEN HELLIGE GRAL #########################
to <- 82.5
lun.pred <- data.frame(conc = rep(0, to+1),
                       age = 0:to,
                       at.risk=100,
                       gender = rep("Male", to+1),
                       female = rep(0, to+1),
                       village1 = rep(1, to+1))
predict <- predict(analysis, newdata = lun.pred, se.fit=TRUE)

lun.pred2 <- data.frame(conc = rep(0, to+1),
                        age = 0:to,
                        at.risk=100,
                        gender = rep("Female", to+1),
                        female = rep(1, to+1),
                        village1 = rep(1, to+1))
predict2 <- predict(analysis, newdata = lun.pred2, se.fit=TRUE)

##### Concentration = 448 ppb
lun.pred3 <- data.frame(conc = rep(448, to+1),
                        age = 0:to,
                        at.risk=100,
                        gender = rep("Male", to+1),
                        female = rep(0, to+1),
                        village1 = rep(0, to+1))
predict3 <- predict(analysis, newdata = lun.pred3, se.fit=TRUE)

lun.pred4 <- data.frame(conc = rep(448, to+1),
                        age = 0:to,
                        at.risk=100,
                        gender = rep("Female", to+1),
                        female = rep(1, to+1),
                        village1 = rep(0, to+1))
predict4 <- predict(analysis, newdata = lun.pred4, se.fit=TRUE)

####### Concentration = 934 ppb
lun.pred5 <- data.frame(conc = rep(934, to+1),
                        age = 0:to,
                        at.risk=100,
                        gender = rep("Male", to+1),
                        female = rep(0, to+1),
                        village1 = rep(0, to+1))
predict5 <- predict(analysis, newdata = lun.pred5, se.fit=TRUE)

lun.pred6 <- data.frame(conc = rep(934, to+1),
                        age = 0:to,
                        at.risk=100,
                        gender = rep("Female", to+1),
                        female = rep(1, to+1),
                        village1 = rep(0, to+1))
predict6 <- predict(analysis, newdata = lun.pred6, se.fit=TRUE)

plot(exp(predict$fit), xlim=c(0,82.5), ylim=c(0,1), type = "l", col="blue", xlab = "Age in years", ylab = "Risk of dying from lung cancer in %", main="Risks at different conc. and ages")
lines(exp(predict2$fit), lty=1, col="red")
lines(exp(predict3$fit), lty=2, col="blue")
lines(exp(predict4$fit), lty=2, col="red")
lines(exp(predict5$fit), lty=3, col="blue")
lines(exp(predict6$fit), lty=3, col="red")
legend(5, 0.90, legend=c("Male", "Female", "0 ppb", "448 ppb", "934 ppb"),
       col=c("blue", "red", 1, 1, 1), lty=c(1,1,1,2,3), cex=0.8)

########### Conc plot og age plot ###############
lun.pred2 <- data.frame(conc = lun$conc,
                        age = lun$age,
                        pop = lun$at.risk,
                        cases = lun$events,
                        pred.cases = prediction.data.original$pred)

ggplot(lun.pred2, aes(x = conc)) + geom_point(aes(y = cases/pop*100), size = 1) + geom_point(aes(y = pred.cases/pop*100), colour = "red", size = 1, alpha = 0.5)

ggplot(lun.pred2, aes(x = age)) + geom_point(aes(y = cases/pop*100), size = 1) + geom_point(aes(y = pred.cases/pop*100), colour = "red", size = 1, alpha = 0.5)


################## USA ANALYSE #####################


