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



################## DANMARK ANALYSE --> DATA INDLÆSNING #####################
## Lung deaths data
DKlun <- read.table("DanLunge.txt", skip=1, header=FALSE)
colnames(DKlun) <- c("Age", "Male", "Female")
DKlun <- as.data.frame(DKlun)

## Total deaths data
DKtot <- read.table("DanDød.txt", skip=1, header=FALSE)
colnames(DKtot) <- c("Age", "Male", "Female")
DKtot <- as.data.frame(DKtot)

## Total population data  - DENNE POPULATIONSDATA ER FRA 2008 OG IKKE FRA 1996
# (LIGESOM DØDSDATAEN ER) DET SKAL ÆNDRES SENERE.
DKpop <- read.table("DanPop.txt", skip=1, header=FALSE)
colnames(DKpop) <- c("Age", "Male", "Female")
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
# Ellers havde der været 5 gange så mange mennesker i USA, som der er.
plot(DKpop$Age, (DKpop$Male+DKpop$Female)/5, main="Total population of Denmark", xlab="Age in years", ylab="Number of people", type="l")
lines(DKpop$Age, DKpop$Female/5, col="red")
lines(DKpop$Age, DKpop$Male/5, col="blue")
legend("topright", legend=c("Both", "Male", "Female"),
       col=c("black", "blue", "red"), lty=c(1,1,1), cex=0.8)