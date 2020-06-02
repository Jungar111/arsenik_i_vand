library("mgcv")
library("ggplot2")
library("tibble")

# Frederik wd
setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\ArsenikGit\\Data")

# Asger wd
#setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i GIT/Data")

# Joachim wd
#setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")

#Oskar wd 
# setwd("C:\\Users\\User\\OneDrive - Danmarks Tekniske Universitet\\SAS_030919\\4. Semester\\42584_Fagprojekt\\Arsenik i drikkevand\\42584_Data\\arsenik_i_vand\\Data")


set.seed(69)
usmblad <- (read.table("usdth_mblad.txt", skip = 1, header = FALSE))
usfblad <- (read.table("usdth_fblad.txt", skip = 1, header = FALSE))
usBlad <- (rbind(usfblad,usmblad))
age <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
usBlad <- t(rbind(usBlad,age))
colnames(usBlad) <- c("Female", "Male", "age")
usBlad <- as.data.frame(usBlad)


length(usmblad)

usmtot <- read.table("usdth_mtot.txt", skip = 1, header = FALSE)
usftot <- read.table("usdth_ftot.txt", skip = 1, header = FALSE)
usTotDeath <- rbind(usftot,usmtot)
age <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
usTotDeath <- t(rbind(usTotDeath,age))
colnames(usTotDeath) <- c("Female", "Male", "age")
usTotDeath <- as.data.frame(usTotDeath)

usmtotPop <- read.table("usmalepyr.txt", skip = 1, header = FALSE)
usftotPop <- read.table("usfemalepyr.txt", skip = 1, header = FALSE)
usTotPop <- rbind(usftotPop,usmtotPop)
age <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
usTotPop <- t(rbind(usTotPop,age))
colnames(usTotPop) <- c("Female", "Male", "age")
usTotPop <- as.data.frame(usTotPop)

plot(usTotDeath$age, usTotDeath$Male, type="l")

fblad <- read.table("fblad.sw.dat", header=TRUE)
mblad <- read.table("mblad.sw.dat", header=TRUE)
fblad$gender <- "Female"
mblad$gender <- "Male"
fblad$female <- 1
mblad$female <- 0
blad <- rbind(fblad,mblad) 
blad$village1 <- 1*(blad$group == 1)
blad$nrWell[blad$group == 1] <- 0
blad$at.risk1 <- blad$at.risk
blad$at.risk1[blad$village1 == 1] <- 0


for (i in 1:42){
  blad$nrWell[blad$group == i + 1] <- unique(well$nwell[well$village == i])
}

#fjerner outliers 

#blad <- blad[-c(126,160,636,761,764,1069),]

N <- length(blad$events)

#analysis <- gam(events~s(age) + s(conc)+ s(conc,age) +age+I(age^2) + gender:village1+
#                  s(age,by=female) + s(conc,by=village1) + ti(conc) + te(age,by=female) + t2(age) + s(I(age^2)) +
#                  offset(I(log(at.risk))),
#                family=poisson(link = "log"),
#                data=blad)

#analysis <- gam(events~s(age) + I(conc^2)+s(conc,age)+ s(age,by=female) + gender + gender:village1 + village1 +
#                  offset(I(log(at.risk))),
#                family=poisson(link = "log"),
#                data=blad)

#analysis <- gam(events~(s(log(conc + 1))) + s(log(conc + 1), (age)) + gender:age + gender:village1 + village1 + offset(I(log(at.risk))),
#                family=poisson(link = "log"),
#                data=blad)


analysis <- gam(events ~ s(age) + I(sqrt(conc))  + village1  + s(age, by = female) + offset(I(log(at.risk))), 
                family = poisson(link = "log"), data = blad, select = TRUE)

#s(age) + s(conc)+ s(conc,age) +age+I(age^2) + gender:village1+
#  s(age,by=female) + s(conc,by=village1)+ ti(conc)+te(age,by=female)+t2(age)+s(I(age^2))+

# AIC
AIC(analysis)

summary(analysis)

prediction.temp$fit


# ggplot(Sample_data, aes(x, y)) + geom_point() + geom_smooth(method = lm)

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis,se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

prediction.data.original <- exp(prediction.data)
# prediction.data.original <- prediction.data.original[order(blad$events, decreasing = TRUE),]

par(mfrow = c(1,1))


plot(analysis$fitted.values, ((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values)),col=blad$group, ylab = "Standard Error", xlab="Fitted values")

length(((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))[((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))<0])


plot(analysis$fitted.values, ((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values)),col=blad$group)


# plots i original data transformation 
#plot(prediction.data.original$pred, blad$events, col="blue")
#lines(prediction.data.original$lower, blad$events, col="red")
#lines(prediction.data.original$upper, blad$events, col="red")

maxr <- 5


blad[analysis$fitted.values>20,]


blad[((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))>6,]
((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))[((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))>6]
analysis$fitted.values[((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))>6]
blad[((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))>6,]

# Laver foreløbig test, tror det her er den rigtige måde at plotte det på
plot(round(prediction.data.original$pred,2),blad$events, xlim=c(0, maxr), ylim=c(0, maxr))
lines(0:maxr,0:maxr, type="l")

v = vector()
v.upper = vector()
v.lower = vector()
x = vector()


#blad$events<- blad$events[order(blad$events, decreasing = TRUE)]

for (i in 1:length(prediction.data.original$pred)){
  res <- mean(blad$events[0.1*(i-1) <= prediction.data.original$pred & prediction.data.original$pred < 0.1*i])
  res.upper <- mean(blad$events[0.1*(i-1) <= prediction.data.original$upper & prediction.data.original$upper < 0.1*i])
  res.lower <- mean(blad$events[0.1*(i-1) <= prediction.data.original$lower & prediction.data.original$lower < 0.1*i])
  v = c(v,res)
  v.upper = c(v.upper,res.upper)
  v.lower = c(v.lower,res.lower)
  x = c(x, 0.1*i-0.05)
}

plot(x, v, xlim=c(0, maxr), ylim = c(0,maxr),xlab="Index", ylab="Predictions")
plot(x,v.lower)
lines(x,v.lower, col = "red")
lines(prediction.data$upper, type = 'l', col = "red")
lines(x, v.lower, xlim=c(0, maxr), ylim = c(0,maxr))


# Rigtige plot her!!!! 
datDat <- as.data.frame(cbind(v, x))

ggplot(datDat, aes(x = v, y = x)) + geom_point() + geom_smooth(method = gam) + xlim(0,120) + ylim(-5,120) + xlab("Prediction") + ylab("Events")


lines(0:maxr,0:maxr, type="l")
lines(0:maxr+sd.Pred,0:maxr, type="l", col = "red")
lines(0:maxr-sd.Pred,0:maxr, type="l", col = "red")
plot(analysis$residuals)


plot(analysis$residuals, ylim=c(-10, 20))
plot(analysis$fitted.values, blad$events)

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

colours <- c("blue" = "22.5", "red" = "27.5", "orange"= "32.5", "yellow" = "37.5", "cyan" = "42.5", "grey" = "47.5", "black" = "52.5", "white" = "57.5", "pink" = "62.5", "navy" = "67.5", "green" = "72.5", "brown" = "77.5", "purple" = "82.5")



ggplot(blad.pred[blad.pred$conc > 0 ,], aes(x = conc[conc > 0], colour = rep(colours,84))) + geom_point(aes(y = cases), size = 1) + labs(colour = "Age") + geom_point(aes(y = pred.cases[conc > 0]), colour = "red", size = 1, alpha = 0.5, pch = 3)


ggplot(blad.pred[blad.pred$conc > 0 ,], aes(x = age, colour = rep(colours,84))) + geom_point(aes(y = cases[conc>0]), size = 1) +  labs(colour = "Age") + geom_point(aes(y = pred.cases[conc>0]), colour = "red", size = 1, alpha = 0.5, pch = 3)

blad.pred[blad.pred$conc > 0 ,]

length(rep(colours,86))

length(blad.pred$age)

tail(blad.pred$age)

1-pnorm(4)^1098

length(((blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values))[(blad$events - analysis$fitted.values)/sqrt(analysis$fitted.values)>4])

to <- 90

blad.pred2 <- data.frame(conc = rep(50,11),
                        age = 80:to,
                        at.risk = 100,
                        gender = rep('Male',11),
                        female = rep(0,11),
                        village1 = rep(0,11))

predict2 <- predict(analysis,newdata = blad.pred2, se.fit=TRUE)


exp(predict2$fit)[80]*35232/5822763*100
plot(exp(predict2$fit))

s = 0

for (i in 80:length(exp(predict2$fit))){
  s = s + exp(predict2$fit)[i]
}

s*201783/5822763*100


par(mfrow = c(2,1))
x.temp <- blad$age[blad$group==1 & blad$female==0]

y.temp <- blad$at.risk[blad$group==1 & blad$female==0]

y.temp2 <- numeric(0)

y.temp2[1] <- y.temp[1]
y.temp2[length(y.temp2)] <- y.temp[length(y.temp)]
k <- 1

length(y.temp)

for (i in 1:12){
  for (j in 1:5){
    k <- k + 1
    y.temp2[k] <- (j/5)*y.temp[i] + (5-j/5) * y.temp[i + 1]
  }
}

plot(y.temp2, type = "l")


LinearInterpolation <- function(y1, y2, lambda = 0.5){
  y3 <- numeric(length(y2))
  y3[1] <- y1[1]
  y3[length(y3)] <- y1[length(y1)]
  for (i in 2:(length(y2)-1)){
    y3[i] <- y2[i]*lambda + y2[i-1] * (1 - lambda)/2 + y2[i + 1] * (1 - lambda)/2
  }
  return(y3)
}

for (i in 1:10){
  y.temp2 <- LinearInterpolation(y.temp, y.temp2, lambda = 0.5)
}


plot(0:60+22.5,y.temp2, type = "l")

z.temp <- (y.temp2 / sum(y.temp2)) * sum(y.temp)


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
                          village1 = rep(1, to+1),
                          nrWell = rep(0, to+1))
  predict1 <- predict(analysis, newdata = blad.pred, se.fit=TRUE)
  
  blad.pred2 <- data.frame(conc = rep(0, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Female", to+1),
                           female = rep(1, to+1),
                           village1 = rep(1, to+1),
                           nrWell = rep(0, to+1))
  predict2 <- predict(analysis, newdata = blad.pred2, se.fit=TRUE)
  
  #Contration = 50 ppb
  blad.pred3 <- data.frame(conc = rep(10, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Male", to+1),
                           female = rep(0, to+1),
                           village1 = rep(0, to+1),
                           nrWell = rep(10, to+1))
  predict3 <- predict(analysis, newdata = blad.pred3, se.fit=TRUE)
  
  blad.pred4 <- data.frame(conc = rep(10, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Female", to+1),
                           female = rep(1, to+1),
                           village1 = rep(0, to+1),
                           nrWell = rep(10, to+1))
  predict4 <- predict(analysis, newdata = blad.pred4, se.fit=TRUE)
  
  
  #Concentration = 300 ppb
  blad.pred5 <- data.frame(conc = rep(50, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Male", to+1),
                           female = rep(0, to+1),
                           village1 = rep(0, to+1),
                           nrWell = rep(10, to+1))
  predict5 <- predict(analysis, newdata = blad.pred5, se.fit=TRUE)
  
  blad.pred6 <- data.frame(conc = rep(50, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Female", to+1),
                           female = rep(1, to+1),
                           village1 = rep(0, to+1),
                           nrWell = rep(10, to+1))
  predict6 <- predict(analysis, newdata = blad.pred6, se.fit=TRUE)
  
  
  #Concentration = 934 ppb
  blad.pred7 <- data.frame(conc = rep(100, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Male", to+1),
                           female = rep(0, to+1),
                           village1 = rep(0, to+1),
                           nrWell = rep(10, to+1))
  predict7 <- predict(analysis, newdata = blad.pred7, se.fit=TRUE)
  
  blad.pred8 <- data.frame(conc = rep(100, to+1),
                           age = 0:to,
                           at.risk=100,
                           gender = rep("Female", to+1),
                           female = rep(1, to+1),
                           village1 = rep(0, to+1),
                           nrWell = rep(10, to+1))
  predict8 <- predict(analysis, newdata = blad.pred8, se.fit=TRUE)
  
  plot(exp(predict1$fit), xlim=c(0,to), ylim=c(0,1), type = "l", col="blue", xlab = "Age in years", ylab = "Risk of dying from lung cancer in %", main="Events at different conc. and ages")
  lines(exp(predict2$fit), lty=1, col="red")
  lines(exp(predict3$fit), lty=2, col="blue")
  lines(exp(predict4$fit), lty=2, col="red")
  lines(exp(predict5$fit), lty=3, col="blue")
  lines(exp(predict6$fit), lty=3, col="red")
  lines(exp(predict7$fit), lty=4, col="blue")
  lines(exp(predict8$fit), lty=4, col="red")
  legend(5, 1, legend=c("Male", "Female", "0 ppb","10 ppb", "50 ppb", "100 ppb"),col=c("blue", "red", 1, 1, 1,1), lty=c(1,1,1,2,3,4), cex=0.8)
}
eventPlot()

par(mfrow = c(2,1))

x.temp <- usTotPop$age

y.temp <- usTotPop$Male

y.temp2 <- numeric(0)

y.temp2[1] <- y.temp[1]
y.temp2[length(y.temp2)] <- y.temp[length(y.temp)]
k <- 1

for (i in 1:20){
  for (j in 1:5){
    k <- k + 1
    y.temp2[k] <- (j/5)*y.temp[i] + (5-j/5) * y.temp[i + 1]
  }
}

plot(y.temp2, type = "l")


for (i in 1:10){
  y.temp2 <- AndersSmooth(y.temp, y.temp2, lambda = 0.5)
}

plot(y.temp2, type = "l")
z.temp <- (y.temp2 / sum(y.temp2)) * sum(y.temp)
plot(z.temp, type="l")

