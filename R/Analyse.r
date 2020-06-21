library(mgcv)
library(ggplot2)
library(survival)
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

# Tjek for korrelation mellem events og variable 
cor(lun$events[lun$group != 1],lun$age[lun$group != 1])
cor(lun$events[lun$group != 1],lun$conc[lun$group != 1])
cor(lun$events[lun$group != 1],lun$female[lun$group != 1])
cor(lun$events,lun$village1)

plot(lun$age[lun$group != 1],lun$events[lun$group != 1],xlab="Age", ylab="Events")
plot(lun$conc[lun$group != 1],lun$events[lun$group != 1])


################ General Additive Model / GLM / Model-definering ###################
analysis <- gam(events~s(age) + s(age,by=female) + I(conc) + gender*village1
                + offset(I(log(at.risk))),
                family=poisson(link = "log"),
                data=lun)
summary(analysis)
AIC(analysis)

# Oversættelsen af GAM'en til en GLM
# analysis <- glm(events~age + gender:age + I(age^2) + conc + gender*village1
#                 + offset(I(log(at.risk))),
#                 family=poisson(link = "log"),
#                 data=lun)

# RESIDUAL PLOT ---> Først indbygget funktion, herefter manuel!
par(mfrow=c(2,1))
plot(analysis$residuals, col=lun$group,  main = "Standardized residual analysis (R-function)", xlab="Observation index", ylab="Residual value")
plot(analysis$fitted.values, (lun$events - analysis$fitted.values) / sqrt(analysis$fitted.values), col=lun$group, main = "Standardized residual analysis (manual)", xlab="Observation index", ylab="Residual value")
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

for (i in 1:length(prediction.data.original$pred)){
  res <- mean(lun$events[0.01*(i-1) <= prediction.data.original$pred & prediction.data.original$pred < 0.01*i])
  v = c(v, res)
  x = c(x, 0.01*i)}

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

########### gg plots (bruges ikke) ###############
lun.pred100 <- data.frame(conc = lun$conc,
                          age = lun$age,
                          pop = lun$at.risk,
                          cases = lun$events,
                          pred.cases = prediction.data.original$pred)

ggplot(lun.pred100, aes(x = conc)) + geom_point(aes(y = cases/pop*100), size = 1) + geom_point(aes(y = pred.cases/pop*100), colour = "red", size = 1, alpha = 0.5)

ggplot(lun.pred100, aes(x = age)) + geom_point(aes(y = cases/pop*100), size = 1) + geom_point(aes(y = pred.cases/pop*100), colour = "red", size = 1, alpha = 0.5)


# Konfidensintervaller 
maxr <- 2

v = vector()
v.upper = vector()
v.lower = vector()
x = vector()


for (i in 1:(length(prediction.data.original$pred)*10)){
  res <- mean(lun$events[0.1*(i-1) <= prediction.data.original$pred & prediction.data.original$pred < 0.1*i])
  res.upper <- mean(lun$events[0.1*(i-1) <= prediction.data.original$upper & prediction.data.original$upper < 0.1*i])
  res.lower <- mean(lun$events[0.1*(i-1) <= prediction.data.original$lower & prediction.data.original$lower < 0.1*i])
  v = c(v,res)
  v.upper = c(v.upper,res.upper)
  v.lower = c(v.lower,res.lower)
  x = c(x, 0.1*i-0.05)
}


v2 <- v[!is.nan(v)]
v1 <- sqrt((abs(v-lun$events))/sqrt(v))[!is.nan(v)]
x1 <- x[!is.nan(v)]

# confidence interval 
conf<- cipoisson(v2, time = 1, p = 0.95, method = c("exact", "anscombe"))

plot(x1, v2, xlim=c(0, maxr), ylim = c(0,maxr), xlab="Predictions", ylab="Actual events", main="Predictions versus actual events")
lines(x1,conf[,1]-v2+x1, col = "red")
lines(x1,conf[,2]-v2+x1, col = "green")
lines(0:maxr,0:maxr, type="l")
polygon(c(x1,rev(x1)),c(conf[,1]-v2+x1, rev(conf[,2]-v2+x1)), col = rgb(0,0,0,0.4), border = NA)



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
plot(USAlun$age, (USAlun$Male+USAlun$Female), main="Number of deaths: Lung cancer (1996)", xlab="Age in years", ylab="Number of deaths", type="l", col="black")
lines(USAlun$age, USAlun$Female, col="red")
lines(USAlun$age, USAlun$Male, col="blue")
legend("topleft", legend=c("Both", "Male", "Female"),
       col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)

# All causes death-plot
plot(USAtotdeaths$age, (USAtotdeaths$Male+USAtotdeaths$Female), main="Number of deaths: All causes (1996)", xlab="Age in years", ylab="Number of deaths", type="l")
lines(USAtotdeaths$age, USAtotdeaths$Female, col="red")
lines(USAtotdeaths$age, USAtotdeaths$Male, col="blue")
legend("topleft", legend=c("Both", "Male", "Female"),
       col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)

# Population plot
# Læg mærke til, at y-aksen er divideret med 5 for at gøre op for, at aldersinddelingen er hvert 5. år.
# Ellers havde der været 5 gange så mange mennesker i USA, som der er.
plot(USApop$age, (USApop$Male+USApop$Female)/5, main="Total population of USA (1996)", xlab="Age in years", ylab="Number of people", type="l")
lines(USApop$age, USApop$Female/5, col="red")
lines(USApop$age, USApop$Male/5, col="blue")
legend("topright", legend=c("Both", "Male", "Female"),
       col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)

######## Den er god nok! ##########
listGenerator <- function(gender){
  
  if (tolower(gender) == "male"){
    gen = 1
  } else {
    gen = 2
  }
  
  ### Mortality rates etc.
  qlist <- numeric(0) 
  hslist <- numeric(0)
  hlist <- numeric(0)
  
  ## Slist er det som i teksten henvises som 2A-18
  Slist <- numeric(0)
  
  ## Kombi er det som i teksten henvises som 2A-19
  kombi <- numeric(0)
  
  ## 2A-20
  kombi1 <- numeric(0)
  
  for (i in 1:21){
    his <- USAtotdeaths[i,gen]/USApop[i,gen]
    hi <- USAlun[i,gen]/USApop[i,gen]
    qi <- exp(-5*his)
    qlist[i] = qi
    hslist[i] = his
    hlist[i] = hi
  }
  
  Slist[1] <- 1  ## Vi begynder at tælle alder i 1 år, derfor er sandsynligheden for at blive 1 år gammel = 1.
  for (i in 2:21){
    Slist[i] <- prod(qlist[1:(i-1)])
  }
  
  for (i in 1:21){
    kombi[i] <- Slist[i]*(1-qlist[i])
  }
  
  for (i in 1:21){
    kombi1[i] <- (hlist[i]/hslist[i]) * (Slist[i]*(1-qlist[i]))
  }
  
  
  Rlung0 <- sum((hlist/hslist) * (1 - qlist) * Slist)
  
  ret <- list("his" = his, "hi" = hi, "qi" = qi, "qlist" = qlist, "hslist" = hslist, "hlist" = hlist, "Slist" = Slist, "kombi" = kombi, "kombi1" = kombi1, "Rlung0" = Rlung0)
  
  return(ret)
}

m <- listGenerator("Male")
f <- listGenerator("Female")

# Plot af hs*list for male og female. 
plot(0:20*5, f$hslist*100, main="Risk of not surviving through age i (all causes)", xlab="Age", ylab="Probability in %", type="l", col="red")
lines(0:20*5, m$hslist*100, col="blue")
legend("topleft", legend=c("Male", "Female"),
       col=c("blue", "red"), lty=c(1,1), cex=0.9)

# Plot af hlist for male og female.
plot(0:20*5, m$hlist*100, main="Risk of not surviving through age i (lung cancer)", xlab="Age", ylab="Probability in %", type="l", col="blue")
lines(0:20*5, f$hlist*100, col="red")
legend("topleft", legend=c("Male", "Female"),
       col=c("blue", "red"), lty=c(1,1), cex=0.9)

# Plot af Slist for male og female. 2A-18!
plot(0:20*5, m$Slist*100, main="Chance of surviving to age i", xlab="Age", ylab="Probability in %", type="l", col="blue")
lines(0:20*5, f$Slist*100, col="red")
legend("topright", legend=c("Male", "Female"),
       col=c("blue", "red"), lty=c(1,1), cex=0.9)

# Plot af kombi for male og female. 2A-19!
plot(0:20*5, f$kombi*100, main="Prob. of dying at age i (all causes)", xlab="Age", ylab="Probability in %", type="l", col="red")
lines(0:20*5, m$kombi*100, col="blue")
legend("topleft", legend=c("Male", "Female"),
       col=c("blue", "red"), lty=c(1,1), cex=0.9)

# Plot af kombi1 for male og female. 2A-20!
plot(0:20*5, m$kombi1*100, main="Prob. of dying at age i (lung cancer)", xlab="Age", ylab="Probability in %", type="l", col="blue")
lines(0:20*5, f$kombi1*100, col="red")
legend("topleft", legend=c("Male", "Female"),
       col=c("blue", "red"), lty=c(1,1), cex=0.9)

# sammenligning --> Det f.eks interessant at lunc cancer peaker før all causes! Der er en mulig trend at spore.
plot(0:20*5, f$kombi*100, type="l", main="Prob. of dying at age i", xlab="Age", ylab="Probability in %", col="red")
lines(0:20*5, f$kombi1*100, lty=2, col="red")
lines(0:20*5, m$kombi*100, lty=1, col="blue")
lines(0:20*5, m$kombi1*100, lty=2, col="blue")
legend("topleft", legend=c("All causes (female)", "Lung cancer (female)", "All causes (male)", "Lung cancer (male)"),
       col=c("red", "red", "blue", "blue"), lty=c(1,2,1,2), cex=0.9)


## 2A-21
m$Rlung0
f$Rlung0

############# Hellig gral plot #2, lifetime prob. and excess risk ############
rLun <- function(conc, gender, colIndex, listCollection, e){
  
  if (gender == "Male"){
    female <- 0
  } else {
    female <- 1
  }
  
  Rlunge <- 0
  
  for (i in 1:21){
    Rlunge <- Rlunge + (listCollection$hlist[i]*(1+e) / (listCollection$hslist[i]+listCollection$hlist[i]*e))* listCollection$Slist[i] * (1-listCollection$qlist[i] * exp(-listCollection$hlist[i]*e))
    for (k in 1:i-1){
      sum <- exp(-sum(listCollection$hlist[k]*e))
    }
    Rlunge <- Rlunge * sum
  }
  return(Rlunge)
}



genderlis <- c("Male", "Female")
conclis <- c(0,5,10,25, seq(50, 900, by = 50))
EL <- exp(0.0015057*conclis) - 1
ELup <- exp((0.0015057 + (1.96*0.000220)) * conclis) - 1
ELdown <- exp((0.0015057 - (1.96*0.000220)) * conclis) - 1

testListMale <- numeric(0)
testListFemale <- numeric(0)
testListMaleup <- numeric(0)
testListFemaleup <- numeric(0)
testListMaledown <- numeric(0)
testListFemaledown <- numeric(0)
colIndex <- 1

for (j in 1:length(conclis)){
  e <- EL[j]
  for (gender in genderlis){
    if (gender == "Male"){
      testListMale <- c(testListMale,rLun(1, gender, colIndex, m, e))
    }else{
      testListFemale <- c(testListFemale,rLun(1, gender, colIndex, f, e))
    }
    colIndex <- colIndex + 1
  }
}

for (j in 1:length(conclis)){
  e <- ELup[j]
  for (gender in genderlis){
    if (gender == "Male"){
      testListMaleup <- c(testListMaleup,rLun(1, gender, colIndex, m, e))
    }else{
      testListFemaleup <- c(testListFemaleup,rLun(1, gender, colIndex, f, e))
    }
    colIndex <- colIndex + 1
  }
}

for (j in 1:length(conclis)){
  e <- ELdown[j]
  for (gender in genderlis){
    if (gender == "Male"){
      testListMaledown <- c(testListMaledown,rLun(1, gender, colIndex, m, e))
    }else{
      testListFemaledown <- c(testListFemaledown,rLun(1, gender, colIndex, f, e))
    }
    colIndex <- colIndex + 1
  }
}


plot(conclis, testListMale, col = "blue", main="Lifetime probability of dying from lung cancer \n with excess risk profile", xlab="Concentration in ppb", ylab="Lifetime probability", ylim=c(0,0.25), pch=20)
points(conclis, testListFemale, col = "red", pch=20)
lines(conclis, testListMale, col="blue", lty=3)
lines(conclis, testListFemale, col="red", lty=3)
polygon(c(conclis, rev(conclis)), c(testListMaleup, rev(testListMaledown)), col=rgb(0,0,1,0.2), border=NA)
polygon(c(conclis, rev(conclis)), c(testListFemaleup, rev(testListFemaledown)), col=rgb(1,0,0,0.2), border=NA)
lines(conclis, rep(m$Rlung0, length(conclis)), col = "blue")
lines(conclis, rep(f$Rlung0, length(conclis)), col = "red")
legend("topleft", legend = c("Male", "Female", "Male Baseline", "Female Baseline"), col = c("blue", "red", "blue", "red"), pch=c(20,20,NA,NA), lty = c(0,0,1,1), cex=0.7)
#
