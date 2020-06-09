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

maxr <- 125

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
  res <- mean(lun$events[0.007*(i-1) <= prediction.data.original$pred & prediction.data.original$pred < 0.007*i])
  v = c(v, res)
  x = c(x, 0.007*i)}

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
predict1 <- predict(analysis, newdata = lun.pred, se.fit=TRUE)

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

plot(exp(predict1$fit), xlim=c(0,82.5), ylim=c(0,1), type = "l", col="blue", xlab = "Age in years", ylab = "Risk of dying from lung cancer in %", main="Risks at different conc. and ages")
lines(exp(predict2$fit), lty=1, col="red")
lines(exp(predict3$fit), lty=2, col="blue")
lines(exp(predict4$fit), lty=2, col="red")
lines(exp(predict5$fit), lty=3, col="blue")
lines(exp(predict6$fit), lty=3, col="red")
legend(5, 0.9, legend=c("Male", "Female", "0 ppb", "448 ppb", "934 ppb"),
       col=c("blue", "red", 1, 1, 1), lty=c(1,1,1,2,3), cex=0.8)

########### Conc plot og age plot ###############
lun.pred100 <- data.frame(conc = lun$conc,
                        age = lun$age,
                        pop = lun$at.risk,
                        cases = lun$events,
                        pred.cases = prediction.data.original$pred)

ggplot(lun.pred100, aes(x = conc)) + geom_point(aes(y = cases/pop*100), size = 1) + geom_point(aes(y = pred.cases/pop*100), colour = "red", size = 1, alpha = 0.5)

ggplot(lun.pred100, aes(x = age)) + geom_point(aes(y = cases/pop*100), size = 1) + geom_point(aes(y = pred.cases/pop*100), colour = "red", size = 1, alpha = 0.5)


################## USA ANALYSE --> DATA INDLÆSNING #####################
## Lung deaths data
USAflun <- read.table("usdth_flun.txt", skip=1, header=FALSE)
USAmlun <- read.table("usdth_mlun.txt", skip=1, header=FALSE)
USAlun <- rbind(USAmlun, USAflun)
age <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
USAlun <- t(rbind(USAlun, age))
colnames(USAlun) <- c("Male", "Female", "age")
USAlun <- as.data.frame(USAlun)

## Total deaths data
USAftot <- read.table("usdth_ftot.txt", skip=1, header=FALSE)
USAmtot <- read.table("usdth_mtot.txt", skip=1, header=FALSE)
USAtot <- rbind(USAmtot, USAftot)
age <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
USAtot <- t(rbind(USAtot, age))
colnames(USAtot) <- c("Male", "Female", "age")
USAtotdeaths <- as.data.frame(USAtot)

## Total population data
USAfpop <- read.table("usfemalepyr.txt", skip=1, header=FALSE)
USAmpop <- read.table("usmalepyr.txt", skip=1, header=FALSE)
USApop <- rbind(USAmpop, USAfpop)
age <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
USApop <- t(rbind(USApop, age))
colnames(USApop) <- c("Male", "Female", "age")
USApop <- as.data.frame(USApop)

####################### USA PLOT-TID #########################
## Herunder er plots af hver af de 3 inddelinger med tilhørende overskrifter.
# Lung cancer deaths
plot(USAlun$age, (USAlun$Male+USAlun$Female), main="Number of deaths: Lung cancer", xlab="Age in years", ylab="Number of deaths", type="l", col="black")
lines(USAlun$age, USAlun$Female, col="red")
lines(USAlun$age, USAlun$Male, col="blue")
legend("topleft", legend=c("Both", "Male", "Female"),
       col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)

# All causes death-plot
plot(USAtotdeaths$age, (USAtotdeaths$Male+USAtotdeaths$Female), main="Number of deaths: All causes", xlab="Age in years", ylab="Number of deaths", type="l")
lines(USAtotdeaths$age, USAtotdeaths$Female, col="red")
lines(USAtotdeaths$age, USAtotdeaths$Male, col="blue")
legend("topleft", legend=c("Both", "Male", "Female"),
       col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)

#Population plot
# Læg mærke til, at y-aksen er divideret med 5 for at gøre op for, at aldersinddelingen er hvert 5. år.
# Ellers havde der været 5 gange så mange mennesker i USA, som der er.
plot(USApop$age, (USApop$Male+USApop$Female)/5, main="Total population of USA", xlab="Age in years", ylab="Number of people", type="l")
lines(USApop$age, USApop$Female/5, col="red")
lines(USApop$age, USApop$Male/5, col="blue")
legend("topright", legend=c("Both", "Male", "Female"),
       col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)


### Mortality rates etc.
qlist <- numeric(0)
hslist <- numeric(0)
hlist <- numeric(0)

## hslist er hstjernelist
for (i in 1:21){
  his <- USAtotdeaths[i,1]/USApop[i,1]
  hi <- USAlun[i,1]/USApop[i,1]
  qi <- exp(-5*his)
  qlist[i] = qi
  hslist[i] = his
  hlist[i] = hi
  }
plot(0:20*5, hslist*100, main="Risk of not surviving through age i", xlab="Age", ylab="Probability in %", type="l")

## Slist er det som i teksten henvises som 2A-18
Slist <- numeric(0)
Slist[1] <- 1  ## Vi begynder at tælle alder i 1 år, derfor er sandsynligheden for at blive 1 år gammel = 1.
for (i in 2:21){
  Slist[i] <- prod(qlist[1:(i-1)])
}
plot(0:20*5, Slist*100, main="Chance of surviving to age i", xlab="Age", ylab="Probability in %", type="l")
#### TESTER MED DATAEN FRA RADON --> GOD TENDENS (Y) !
bibop <- c(1,0.982,0.980,0.978,0.972,0.962,0.952,0.943,0.931,0.915,0.888,0.847,0.788,0.705,0.595,0.461,0.317,0.181,0.071,0.028,0.011,0.004)
bibop2 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
# Hvorfor 0:21*5? Fordi vi ved RADON data skal slutte i aldersintervallet 105.
lines(0:21*5,bibop*100, type="l", col="red")

## Kombi er det som i teksten henvises som 2A-19
kombi <- numeric(0)
for (i in 1:21){
  kombi[i] <- Slist[i]*(1-qlist[i])
}
plot(0:20*5, kombi*100, type="l", main="Prob. of dying at age i (all causes)", xlab="Age", ylab="Probability in %")


## Nu laver vi H (altså IKKE Hs) :D
plot(0:20*5, hlist*100, main="Risk of not surviving through age i (lung cancer)", xlab="Age", ylab="Probability in %", type="l")

## Nu vil vi kigge på sandsynligheden for at blive x år gammel og at dø ved x år:
## 2A-20
kombi1 <- numeric(0)
for (i in 1:21){
  kombi1[i] <- (hlist[i]/hslist[i]) * (Slist[i]*(1-qlist[i]))
}
plot(0:20*5, kombi1*100, type="l", main="Prob. of dying at age i (lung cancer)", xlab="Age", ylab="Probability in %")

# sammenligning --> Det f.eks interessant at lunc cancer peaker før all causes! Der er en mulig trend at spore.
plot(0:20*5, kombi*100, type="l", main="Prob. of dying at age i", xlab="Age", ylab="Probability in %", col="blue")
lines(0:20*5, kombi1*100, type="l", col="red")
legend("topleft", legend=c("All causes", "Lung cancer"),
       col=c("blue", "red"), lty=c(1,1), cex=0.8)


## 2A-21
Rlung0 <- sum((hlist/hslist) * (1 - qlist) * Slist)
Rlung0

Rlung0 / (1 - Slist[21])

### Sample test data fra Taiwan population 2A-22!:
## tester er contribution from exposed sample (Taiwan). Elist == Excess risk profile.
## Predict1 = 0 ppb (MALE)
## Predict3 = 448 ppb (MALE)
## Predict5 = 934 ppb (MALE)
library("viridis")
plot(1, type="n", xlab="", ylab="", xlim=c(0,21), ylim=c(0.01, 0.012))
rLun <- function(conc, gender, i){
  
  if (gender == "Male"){
    female <- 0
  } else {
    female <- 1
  }
  
  lun.predFunfun <- data.frame(conc = rep(0, to+1),
                          age = 0:to,
                          at.risk=100,
                          gender = rep(gender, to+1),
                          female = rep(female, to+1),
                          village1 = rep(1, to+1))
  
  predictFunfun <- predict(analysis, newdata = lun.predFunfun, se.fit=TRUE)
  
  lun.predFun <- data.frame(conc = rep(conc, to+1),
                         age = 0:to,
                         at.risk=100,
                         gender = rep(gender, to+1),
                         female = rep(female, to+1),
                         village1 = rep(0, to+1))
  
  predictFun <- predict(analysis, newdata = lun.predFun, se.fit=TRUE)
  Rlunge <- 0
  
  pred0 <- exp(predictFunfun$se.fit)
  predVar <- exp(predictFun$se.fit)
  
  if (conc == 0){
    Elist <- rep(21, 0) # Her er 21 0'er!
  } else {
    Elist <- (predVar[seq(1, length(predVar), 4)] / pred0[seq(1, length(pred0), 4)])/100
  }
  
  print(head(Elist))
  
  
  points(Elist)
  lines(Elist, col=viridis(22)[i])
  
  
  for (i in 1:21){
    for (k in i-1){
      Rlunge <- Rlunge + ( (hlist[i]*(1+Elist[i])) / (hslist[i]+hlist[i]*Elist[i]) ) * Slist[i] * ( 1-qlist[i] * exp(-hlist[i]*Elist[i]) ) * exp(-sum(hlist[k]*Elist[k]))
    }
  }
  return(Rlunge)
}

genderlis <- c("Male")#, "Female")
conclis <- c(0, 5, 10, 25, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950)

testListMale <- numeric(0)
testListFemale <- numeric(0)
i = 0
for (gender in genderlis){
  for (conc in conclis){
    if (gender == "Male"){
      testListMale <- c(testListMale,rLun(conc, gender, i))
    }else{
      testListFemale <- c(testListFemale,rLun(conc, gender, i))
    }
    i = i + 1
  }
}

plot(conclis, testListMale, col = "blue")
points(conclis,testListFemale, col = "red")


### 2A-23:
# Hvis man ved at person har overlevet til t0 (i dette tilfælde er t0 = 1) år, hvad er så sandsynligheden for at dø af lungekræft.
sum((hlist[i]/hslist[i]) * kombi[i]) * 100


## qlist er chancen for at overleve til næste aldersgruppe (komme videre fra sin egen)!
## qi is the prob of surviving year i when all causes are acting.

# village1 = 0 skulle gerne have lidt overdødelighed og
# village1 = 1 skulle gerne ligne USA meget.

# Hazard ratio = odds ratio (ikke matematisk ens men man behandler dem ens) og risk ratio og odds ratio er næsten identisk når sandsynlighederne er så små!
# God argumentation!

lun.predxx <- data.frame(conc = rep(200, to+1),
                        age = 0:to,
                        at.risk=100,
                        gender = rep("Male", to+1),
                        female = rep(0, to+1),
                        village1 = rep(0, to+1))
predictxx <- predict(analysis, newdata = lun.predxx, se.fit=TRUE)


Rlunge <- 0
Elist <- predictxx$se.fit[seq(1, length(predictxx$se.fit), 4)] - predict1$se.fit[seq(1, length(predict1$se.fit), 4)]
for (i in 1:21){
  for (k in i-1){
    Rlunge <- Rlunge + (hlist[i]*(1+Elist[i]) / hslist[i]+hlist[i]*Elist[i])* Slist[i] * (1-qlist[i] * exp(-hlist[i]*Elist[i])) * exp(-sum(hlist[k]*Elist[k]))
  }
}
Rlunge

