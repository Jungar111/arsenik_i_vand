################## USA ANALYSE --> DATA INDLÆSNING #####################

## bladder deaths data
USAfblad <- read.table("usdth_fblad.txt", skip=1, header=FALSE)
USAmblad <- read.table("usdth_mblad.txt", skip=1, header=FALSE)
USAblad <- rbind(USAmblad, USAfblad)
age <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
USAblad <- t(rbind(USAblad, age))
colnames(USAblad) <- c("Male", "Female", "age")
USAblad <- as.data.frame(USAblad)

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

(sum(USAblad$Male)+sum(USAblad$Female))/(sum(USApop$Male) + sum(USApop$Female)) * 100
####################### PLOT-TID #########################
## Herunder er plots af hver af de 3 inddelinger med tilhørende overskrifter.

# bladder cancer deaths
BladderCancerPlot <- function(){
  plot(USAblad$age, (USAblad$Male+USAblad$Female), main="Number of deaths: bladder cancer", xlab="Age in years", ylab="Number of deaths", type="l", col="black")
  lines(USAblad$age, USAblad$Female, col="red")
  lines(USAblad$age, USAblad$Male, col="blue")
  legend("topleft", legend=c("Both", "Male", "Female"),
         col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)
}

# All causes death-plot
AllCausePlot <- function(){
  plot(USAtotdeaths$age, (USAtotdeaths$Male+USAtotdeaths$Female), main="Number of deaths: All causes", xlab="Age in years", ylab="Number of deaths", type="l")
  lines(USAtotdeaths$age, USAtotdeaths$Female, col="red")
  lines(USAtotdeaths$age, USAtotdeaths$Male, col="blue")
  legend("topleft", legend=c("Both", "Male", "Female"),
         col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)
}

#Population plot
# Læg mærke til, at y-aksen er divideret med 5 for at gøre op for, at aldersinddelingen er hvert 5. år.
# Ellers havde der været 5 gange så mange mennesker i USA, som der er.

PopPlot <- function(){
  plot(USApop$age, (USApop$Male+USApop$Female)/5, main="Total population of USA", xlab="Age in years", ylab="Number of people", type="l")
  lines(USApop$age, USApop$Female/5, col="red")
  lines(USApop$age, USApop$Male/5, col="blue")
  legend("topright", legend=c("Both", "Male", "Female"),
         col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)
}


USAtotdeaths[USAtotdeaths$Male+USAtotdeaths$Female == max(USAtotdeaths$Male+USAtotdeaths$Female),]
USAblad[USAblad$Male+USAblad$Male == max(USAblad$Male+USAblad$Male),]



listGenerator <- function(){
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
    his <- USAtotdeaths[i,1]/USApop[i,1]
    hi <- USAblad[i,1]/USApop[i,1]
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
  
  
  Rbladder0 <- sum((hlist/hslist) * (1 - qlist) * Slist)
  
  ret <- list("his" = his, "hi" = hi, "qi" = qi, "qlist" = qlist, "hslist" = hslist, "hlist" = hlist, "Slist" = Slist, "kombi" = kombi, "kombi1" = kombi1)
  
  return(ret)
}

l <- listGenerator()

## hslist er hstjernelist

plot(0:20*5, l$hslist*100, main="Risk of not surviving through age i", xlab="Age", ylab="Probability in %", type="l")

plot(0:20*5, l$Slist*100, main="Chance of surviving to age i", xlab="Age", ylab="Probability in %", type="l")

plot(0:20*5, l$kombi*100, type="l", main="Prob. of dying at age i (all causes)", xlab="Age", ylab="Probability in %")

## Nu laver vi H (altså IKKE Hs) :D
plot(0:20*5, l$hlist*100, main="Risk of not surviving through age i (bladder cancer)", xlab="Age", ylab="Probability in %", type="l")

## Nu vil vi kigge på sandsynligheden for at blive x år gammel og at dø ved x år:
plot(0:20*5, l$kombi1*100, type="l", main="Prob. of dying at age i (bladder cancer)", xlab="Age", ylab="Probability in %")

sum(l$kombi1)*100

o <- c(0:20*5)

o[l$kombi1 == max(l$kombi1)]

# sammenligning --> Det f.eks interessant at bladc cancer peaker før all causes! Der er en mulig trend at spore.
comparisonPlot <- function(){
  plot(0:20*5, kombi*100, type="l", main="Prob. of dying at age i", xlab="Age", ylab="Probability in %", col="blue")
  lines(0:20*5, kombi1*100, type="l", col="red")
  legend("topleft", legend=c("All causes", "bladder cancer"),
         col=c("blue", "red"), lty=c(1,1), cex=0.8)
}



## 2A-21

Rbladder0

Rbladder0 / (1 - Slist[21])

### Sample test data fra Taiwan population 2A-22!:
## tester er contribution from exposed sample (Taiwan).
## Predict1 = 0 ppb (MALE)
## Predict3 = 448 ppb (MALE)
## Predict5 = 934 ppb (MALE)
tester <- predict1$se.fit[seq(1, length(predict1$se.fit), 4)]
tester
length(tester)
Rbladdere <- 0
for (i in 1:21){
  for (k in i-1){
    Rbladdere <- Rbladdere + (hlist[i]*(1+tester[i]) / hslist[i]+hlist[i]*tester[i])* Slist[i] * (1-qlist[i] * exp(-hlist[i]*tester[i])) * exp(-sum(hlist[k]*tester[k]))
  }
}

### 2A-23:
# Hvis man ved at person har overlevet til t0 (i dette tilfælde er t0 = 1) år, hvad er så sandsynligheden for at dø af bladderekræft.
sum((hlist[i]/hslist[i]) * kombi[i]) * 100


## qlist er chancen for at overleve til næste aldersgruppe (komme videre fra sin egen)!
## qi is the prob of surviving year i when all causes are acting.

# village1 = 0 skulle gerne have lidt overdødelighed og
# village1 = 1 skulle gerne ligne USA meget.

# Hazard ratio = odds ratio (ikke matematisk ens men man behandler dem ens) og risk ratio og odds ratio er næsten identisk når sandsynlighederne er så små!
# God argumentation!

exp(-5*36.552/10000)
-log(0.9819)
exp(-0.01826581)


## Taiwan befolkning vs. USA befolkning alders
