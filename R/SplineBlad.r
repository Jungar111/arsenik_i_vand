library("mgcv")
library("ggplot2")

# Frederik wd
# setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\ArsenikGit\\Data")

# Asger wd
setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i GIT/Data")

# Joachim wd
#setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")

#Oskar wd 
# setwd("C:\\Users\\User\\OneDrive - Danmarks Tekniske Universitet\\SAS_030919\\4. Semester\\42584_Fagprojekt\\Arsenik i drikkevand\\42584_Data\\arsenik_i_vand\\Data")


set.seed(69)

fblad <- read.table("fblad.sw.dat", header=TRUE)
mblad <- read.table("mblad.sw.dat", header=TRUE)
fblad$gender <- "Female"
mblad$gender <- "Male"
fblad$female <- 1
mblad$female <- 0
blad <- rbind(fblad,mblad) 
blad$village1 <- 1*(blad$group == 1)

N <- length(blad$events)

analysis <- gam(events~s(age)+s(log(1+conc))+s(age,by=female) + gender*village1 + s(I(age^2))+ age:gender+
                  offset(I(log(at.risk))),
                family=poisson(link = "log"),
                data=blad)

# AIC
AIC(analysis)

summary(analysis)

prediction.temp$fit

# ggplot(Sample_data, aes(x, y)) + geom_point() + geom_smooth(method = lm)

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis,se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

prediction.data.original <- exp(prediction.data)
prediction.data.original <- prediction.data.original[order(blad$events, decreasing = TRUE),]

par(mfrow = c(1,1))


plot(analysis$fitted.values, ((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values)),col=blad$group)

length(((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))[((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))<0])


plot(analysis$fitted.values, ((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values)),col=blad$group)


# plots i original data transformation 
#plot(prediction.data.original$pred, blad$events, col="blue")
#lines(prediction.data.original$lower, blad$events, col="red")
#lines(prediction.data.original$upper, blad$events, col="red")

maxr <- 150

# Laver foreløbig test, tror det her er den rigtige måde at plotte det på
plot(round(prediction.data.original$pred,2),blad$events[order(blad$events, decreasing = TRUE)], xlim=c(0, maxr), ylim=c(0, maxr))
lines(0:maxr,0:maxr, type="l")

v = vector()
x = vector()

#blad$events<- blad$events[order(blad$events, decreasing = TRUE)]

for (i in 1:length(prediction.data.original$pred)){
  res <- mean(blad$events[0.1*(i-1) <= prediction.data.original$pred & prediction.data.original$pred < 0.1*i])
  v = c(v,res)
  x = c(x, 0.1*i)
}

sd.Pred <- sd(prediction.data.original$pred)

length(prediction.data.original$upper)
plot(x, v, xlim=c(0, maxr), ylim = c(0,maxr))blad$
lines(0:maxr,0:maxr, type="l")
lines(0:maxr+sd.Pred,0:maxr, type="l", col = "red")
lines(0:maxr-sd.Pred,0:maxr, type="l", col = "red")
plot(analysis$residuals)


plot(analysis$residuals, ylim=c(-10, 20))
plot(analysis$fitted.values, analysis$residuals)

ggplot(blad, aes(events,conc)) + geom_point() + geom_smooth(method = gam, formula = blad$events~s(blad$age)+s(log(1+blad$conc))+s(blad$age,by=blad$female) + blad$gender*blad$village1 + offset(I(log(blad$at.risk))))

par(mfrow = c(2,2))
gam.check(analysis)

blad.pred <- data.frame(conc = blad$conc,
                        age = blad$age,
                        pop = blad$at.risk,
                        cases = blad$events,
                        pred.cases = prediction.data.original$pred)

ggplot(blad.pred, aes(x = conc)) + geom_point(aes(y = cases/pop*100), size = 1) + geom_point(aes(y = pred.cases/pop*100), colour = "red", size = 1, alpha = 0.5, pch = 3)

ggplot(blad.pred, aes(x = age)) + geom_point(aes(y = cases/pop*100), size = 1) + geom_point(aes(y = pred.cases/pop*100), colour = "red", size = 1, alpha = 0.5, pch = 3)

length(analysis$fitted.values[analysis$fitted.values<3])

1-pnorm(4)^1098

length(((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))[(blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values)>4])

to <- 90

blad.pred2 <- data.frame(conc = rep(0,to+1),
                        age = 0:to,
                        at.risk = 100,
                        gender = rep('Male',to+1),
                        female = rep(0,to+1),
                        village1 = rep(1,to+1))

predict2 <- predict(analysis,newdata = blad.pred2, se.fit=TRUE)

exp(predict2$fit)[80]*35232/5822763*100
plot(exp(predict2$fit))

s = 0

for (i in 80:length(exp(predict2$fit))){
  s = s + exp(predict2$fit)[i]
}

s*201783/5822763*100


eventPlot = function(){
  
  # Plot diff conc/ages
  par(mfrow=c(1,1))
  to <- 90
  #Concentration = 0 ppb
  blad.pred <- data.frame(conc = rep(0, to+1),
                          age = 0:to,
                          at.risk=100,
                          gender = rep("Male", to+1),
                          female = rep(0, to+1),
                          village1 = rep(1, to+1))
  predict <- predict(analysis, newdata = blad.pred, se.fit=TRUE)
  
  blad.pred2 <- data.frame(conc = rep(0, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Female", to+1),
                           female = rep(1, to+1),
                           village1 = rep(1, to+1))
  predict2 <- predict(analysis, newdata = blad.pred2, se.fit=TRUE)
  
  #Contration = 50 ppb
  blad.pred3 <- data.frame(conc = rep(50, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Male", to+1),
                           female = rep(0, to+1),
                           village1 = rep(0, to+1))
  predict3 <- predict(analysis, newdata = blad.pred3, se.fit=TRUE)
  
  blad.pred4 <- data.frame(conc = rep(50, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Female", to+1),
                           female = rep(1, to+1),
                           village1 = rep(0, to+1))
  predict4 <- predict(analysis, newdata = blad.pred4, se.fit=TRUE)
  
  
  #Concentration = 300 ppb
  blad.pred5 <- data.frame(conc = rep(300, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Male", to+1),
                           female = rep(0, to+1),
                           village1 = rep(0, to+1))
  predict5 <- predict(analysis, newdata = blad.pred5, se.fit=TRUE)
  
  blad.pred6 <- data.frame(conc = rep(300, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Female", to+1),
                           female = rep(1, to+1),
                           village1 = rep(0, to+1))
  predict6 <- predict(analysis, newdata = blad.pred6, se.fit=TRUE)
  
  
  #Concentration = 934 ppb
  blad.pred7 <- data.frame(conc = rep(934, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Male", to+1),
                           female = rep(0, to+1),
                           village1 = rep(0, to+1))
  predict7 <- predict(analysis, newdata = blad.pred7, se.fit=TRUE)
  
  blad.pred8 <- data.frame(conc = rep(934, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Female", to+1),
                           female = rep(1, to+1),
                           village1 = rep(0, to+1))
  predict8 <- predict(analysis, newdata = blad.pred8, se.fit=TRUE)
  
  plot(exp(predict$fit), xlim=c(0,to), ylim=c(0,2), type = "l", col="blue", xlab = "Age in years", ylab = "Risk of dying from lung cancer in %", main="Events at different conc. and ages")
  lines(exp(predict2$fit), lty=1, col="red")
  lines(exp(predict3$fit), lty=2, col="blue")
  lines(exp(predict4$fit), lty=2, col="red")
  lines(exp(predict5$fit), lty=3, col="blue")
  lines(exp(predict6$fit), lty=3, col="red")
  lines(exp(predict7$fit), lty=4, col="blue")
  lines(exp(predict8$fit), lty=4, col="red")
  legend(5, 2, legend=c("Male", "Female", "0 ppb","50 ppb", "300 ppb", "934 ppb"),col=c("blue", "red", 1, 1, 1,1), lty=c(1,1,1,2,3,4), cex=0.8)
}
eventPlot()

