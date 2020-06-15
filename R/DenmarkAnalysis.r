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
analysis <- gam(events~s(age) + s(age,by=female) + I(conc) + gender*village1
                + offset(I(log(at.risk))),
                family=poisson(link = "log"),
                data=lun)
summary(analysis)
AIC(analysis)

################## DANMARK ANALYSE --> DATA INDLÆSNING #####################
## Lung deaths data
DKlun <- read.table("DanLunge.txt", skip=1, header=FALSE)
colnames(DKlun) <- c("Male", "Female", "Age")
DKlun <- as.data.frame(DKlun)

## Total deaths data
DKtot <- read.table("DanDød.txt", skip=1, header=FALSE)
colnames(DKtot) <- c("Male", "Female", "Age")
DKtot <- as.data.frame(DKtot)

## Total population data  - DENNE POPULATIONSDATA ER FRA 2008 OG IKKE FRA 1996
# (LIGESOM DØDSDATAEN ER) DET SKAL ÆNDRES SENERE.
DKpop <- read.table("DanPop.txt", skip=1, header=FALSE)
colnames(DKpop) <- c("Male", "Female", "Age")
DKpop <- as.data.frame(DKpop)

####################### DANMARK PLOT-TID #########################
## Herunder er plots af hver af de 3 inddelinger med tilhørende overskrifter.
# Lung cancer deaths
plot(DKlun$Age, (DKlun$Male+DKlun$Female), main="Number of deaths: Lung cancer (DK)", xlab="Age in years", ylab="Number of deaths", type="l", col="black")
lines(DKlun$Age, DKlun$Female, col="red")
lines(DKlun$Age, DKlun$Male, col="blue")
legend("topleft", legend=c("Both", "Male", "Female"),
       col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)

# All causes death-plot
plot(DKtot$Age, (DKtot$Male+DKtot$Female), main="Number of deaths: All causes (DK)", xlab="Age in years", ylab="Number of deaths", type="l")
lines(DKtot$Age, DKtot$Female, col="red")
lines(DKtot$Age, DKtot$Male, col="blue")
legend("topleft", legend=c("Both", "Male", "Female"),
       col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)

#Population plot
# Læg mærke til, at y-aksen er divideret med 5 for at gøre op for, at aldersinddelingen er hvert 5. år.
# Ellers havde der været 5 gange så mange mennesker i Danmark, som der er.
plot(DKpop$Age, (DKpop$Male+DKpop$Female)/5, main="Total population of Denmark", xlab="Age in years", ylab="Number of people", type="l")
lines(DKpop$Age, DKpop$Female/5, col="red")
lines(DKpop$Age, DKpop$Male/5, col="blue")
legend("topright", legend=c("Both", "Male", "Female"),
       col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)

###################### Den er god nok! #########################
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
  
  for (i in 1:19){
    his <- DKtot[i,gen]/DKpop[i,gen]
    hi <- DKlun[i,gen]/DKpop[i,gen]
    qi <- exp(-5*his)
    qlist[i] = qi
    hslist[i] = his
    hlist[i] = hi
  }
  
  Slist[1] <- 1  ## Vi begynder at tælle alder i 1 år, derfor er sandsynligheden for at blive 1 år gammel = 1.
  for (i in 2:19){
    Slist[i] <- prod(qlist[1:(i-1)])
  }
  
  for (i in 1:19){
    kombi[i] <- Slist[i]*(1-qlist[i])
  }
  
  for (i in 1:19){
    kombi1[i] <- (hlist[i]/hslist[i]) * (Slist[i]*(1-qlist[i]))
  }
  
  
  Rlung0 <- sum((hlist/hslist) * (1 - qlist) * Slist)
  
  ret <- list("his" = his, "hi" = hi, "qi" = qi, "qlist" = qlist, "hslist" = hslist, "hlist" = hlist, "Slist" = Slist, "kombi" = kombi, "kombi1" = kombi1, "Rlung0" = Rlung0)
  
  return(ret)
}

m <- listGenerator("Male")
f <- listGenerator("Female")

# Plot af hs*list for male og female. 
plot(0:18*5, m$hslist*100, main="Risk of not surviving through age i (all causes)", xlab="Age", ylab="Probability in %", type="l", col="blue")
lines(0:18*5, f$hslist*100, col="red")
legend("topleft", legend=c("Male", "Female"),
       col=c("blue", "red"), lty=c(1,1), cex=0.9)

# Plot af hlist for male og female.
plot(0:18*5, m$hlist*100, main="Risk of not surviving through age i (lung cancer)", xlab="Age", ylab="Probability in %", type="l", col="blue")
lines(0:18*5, f$hlist*100, col="red")
legend("topleft", legend=c("Male", "Female"),
       col=c("blue", "red"), lty=c(1,1), cex=0.9)

# Plot af Slist for male og female. 2A-18!
plot(0:18*5, m$Slist*100, main="Chance of surviving to age i", xlab="Age", ylab="Probability in %", type="l", col="blue")
lines(0:18*5, f$Slist*100, col="red")
legend("topright", legend=c("Male", "Female"),
       col=c("blue", "red"), lty=c(1,1), cex=0.7)

# Plot af kombi for male og female. 2A-19!
plot(0:18*5, f$kombi*100, main="Prob. of dying at age i (all causes)", xlab="Age", ylab="Probability in %", type="l", col="red")
lines(0:18*5, m$kombi*100, col="blue")
legend("topleft", legend=c("Male", "Female"),
       col=c("blue", "red"), lty=c(1,1), cex=0.9)

# Plot af kombi1 for male og female. 2A-20!
plot(0:18*5, m$kombi1*100, main="Prob. of dying at age i (lung cancer)", xlab="Age", ylab="Probability in %", type="l", col="blue")
lines(0:18*5, f$kombi1*100, col="red")
legend("topleft", legend=c("Male", "Female"),
       col=c("blue", "red"), lty=c(1,1), cex=0.9)

# sammenligning --> Det f.eks interessant at lunc cancer peaker før all causes! Der er en mulig trend at spore.
plot(0:18*5, f$kombi*100, type="l", main="Prob. of dying at age i", xlab="Age", ylab="Probability in %", col="red")
lines(0:18*5, f$kombi1*100, lty=2, col="red")
lines(0:18*5, m$kombi*100, lty=1, col="blue")
lines(0:18*5, m$kombi1*100, lty=2, col="blue")
legend("topleft", legend=c("All causes (female)", "Lung cancer (female)", "All causes (male)", "Lung cancer (male)"),
       col=c("red", "red", "blue", "blue"), lty=c(1,2,1,2), cex=0.9)


## 2A-21
m$Rlung0
f$Rlung0

############# Hellig gral plot #2, lifetime prob. and excess risk ############
# Her defineres funktionen rLun, der trækker information ud.
rLun <- function(conc, gender, colIndex, listCollection, e){
  
  if (gender == "Male"){
    female <- 0
  } else {
    female <- 1
  }
  
  Rlunge <- 0
  
  for (i in 1:19){
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
# Tallet o.0015057 kommer fra summary(analysis) og er koefficient-estimatet til conc.
EL <- exp(0.0015057*conclis) - 1

testListMale <- numeric(0)
testListFemale <- numeric(0)
colIndex <- 1

for (j in 1:length(conclis)){
  e <- EL[j]
  for (gender in genderlis){
    if (gender == "Male"){
      testListMale <- c(testListMale, rLun(1, gender, colIndex, m, e))
    }else{
      testListFemale <- c(testListFemale, rLun(1, gender, colIndex, f, e))
    }
    colIndex <- colIndex + 1
  }
}

plot(0:21*(934/21), EL, type="l", main="Excess Risk Profile", xlab="Concentration in ppb", ylab="Excess risk factor", col="darkblue")


plot(conclis, testListMale, col = "blue", main="Lifetime probability of dying from lung cancer \n with excess risk profile (DK)", xlab="Concentration in ppb", ylab="Lifetime probability", ylim=c(0,0.25), pch=20)
points(conclis, testListFemale, col = "red", pch=20)
lines(conclis, testListMale, col="blue", lty=3)
lines(conclis, testListFemale, col="red", lty=3)
lines(conclis, rep(m$Rlung0, length(conclis)), col = "blue")
lines(conclis, rep(f$Rlung0, length(conclis)), col = "red")
legend("topleft", legend = c("Male", "Female", "Male Baseline", "Female Baseline"), col = c("blue", "red", "blue", "red"), pch=c(20,20,NA,NA), lty = c(0,0,1,1), cex=0.7)
#
