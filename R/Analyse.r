library(mgcv)
# Frederik wd
#setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\ArsenikGit\\Data")
# Asger wd
#setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i GIT/Data")
# Joachim wd
#setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")
#Oskar wd 
setwd("C:\\Users\\User\\OneDrive - Danmarks Tekniske Universitet\\SAS_030919\\4. Semester\\42584_Fagprojekt\\Arsenik i drikkevand\\42584_Data\\Arsenik i vand\\Data")
set.seed(69)

############### INDLÆSNING AF LUNGE DATA #################
flun <- read.table("flun.sw.dat", header=TRUE)
mlun <- read.table("mlun.sw.dat", header = TRUE)
flun$gender <- "Female"
mlun$gender <- "Male"
flun$female <- 1
mlun$female <- 0
lun <- rbind(flun,mlun)
# Antal observationer
N <- length(lun$events)
# p.hat (empirisk sandsynlighed)
p.hat <- sum(lun$events/N)


################ General Additive Model / GLM / Model-definering ###################
# analysis <- glm(events~ age + I(age^2) + I(sqrt(conc)) + gender, family=poisson(link= "log"), data=lun, offset=log(at.risk))
analysis <- gam(events~gender+s(age)+s(age,by=female)+s(conc)+offset(I(log(at.risk))),
                family=poisson(link = "log"),
                data=lun)

summary(analysis)

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis, se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

# Transformerer dataen tilbage til original tilstand og sorterer data efter pred
prediction.data.original <- exp(prediction.data)
prediction.data.original <- prediction.data.original[order(lun$events, decreasing = TRUE), ]


############### PLOT LUN #################

maxr <- 607

# Laver foreløbig test, tror det her er den rigtige måde at plotte det på
plot(round(prediction.data.original$pred, digits=2), lun$events[order(lun$events, decreasing = TRUE)], xlim=c(0, maxr), ylim=c(0, maxr), xlab="Predicted events", ylab="Actual events")
lines(0:maxr, 0:maxr, type="l")
sd.Pred <- sd(prediction.data.original$pred[0:maxr])
lines(0:maxr+sd.Pred, 0:maxr, type="l", col = "red")
lines(0:maxr-sd.Pred, 0:maxr, type="l", col = "red")

v = vector()
x = vector()

lun$events <- lun$events[order(lun$events, decreasing = TRUE)]

for (i in 1:length(prediction.data.original$pred)){
  res <- mean(lun$events[0.1*(i-1) <= prediction.data.original$pred & prediction.data.original$pred < 0.1*i])
  v = c(v, res)
  x = c(x, 0.1*i)}

plot(x, v, xlim=c(0, maxr), ylim=c(0, maxr), xlab="Average predicted events", ylab="Average actual events")
lines(0:maxr,0:maxr, type="l")






##LABORERAR LITE MED TAIWAN DEATHRATES## 
library(mgcv)
#första två: 
# 4.26, 4.55, 3.95, 0.20, 0.21, 0.20,
alldata <- c( 0.10, 0.11, 0.10,  0.14, 0.18, 0.10, 0.31, 0.42, 0.19, 0.43, 0.59, 0.25, 0.49, 0.67, 0.30, 0.74, 0.99, 0.48, 1.19, 1.66, 0.73, 1.97, 2.87, 1.09, 2.99, 4.36, 1.67, 4.35, 6.33, 2.43, 6.08, 8.69, 3.58, 8.39, 11.80, 5.19, 12.19, 16.60, 8.18, 19.94, 26.46, 14.27, 33.90, 43.76, 26.03, 60.57, 74.99, 50.08, 105.94, 123.01, 91.88, 168.11, 182.84, 155.54, 254.89, 255.92, 254.11, 338.41, 313.27, 359.21)
#alldata är endast för 2018 - en bättre modell kan evetuelt hittas genom att titta på flera år
n <- length(alldata)



bothsexes <- (alldata[seq(1, length(alldata), 3)])/1000

males <- (alldata[seq(2, length(alldata), 3)])/1000
females <- (alldata[seq(3, length(alldata), 3)])/1000
intervalindex <- c(1:length(males))

plot(intervalindex, log(males))  #ser onekligen rätt lineärt ut 
#Men hur i helvete skall man fånga den der spiken i början (nyfödda är ömtåliga)



lmmodel <- lm(log(males) ~ intervalindex)
lmpred <- predict(lmmodel)
lines(lmpred)
summary(lmmodel)

#residualplot
plot(intervalindex, residuals(lmmodel))
abline(h=0, col="red")
#den var onekligen inte särskilt lineär after all... 


#Jag tror jag behöver mer data




plot(log(intervalindex, males))
plot(exp(intervalindex), males)
plot(intervalindex^6,  males)




logm <- log(males)
logint <- log(intervalindex) 
plot(logint, logm)
poly2 <- lm(logm ~ logint + logint^2)
poly2pred <- predict(poly2)
lines(poly2pred)


## YTTERST BASIC PLOT AV DEATH RATES FOR BOTH, MALES AND FEMALES##
plot(bothsexes, type="l", col="black")
lines(males, col="blue")
lines(females, col="red")



## LAVER EN ENKEL EXPONENTIELL MODELL ## 
expmodel <- lm(log(males)~intervalindex)
summary(expmodel)



#En lineär tredjegradsmodell
poly_tre <- lm(males ~intervalindex + I(intervalindex^2) + I(intervalindex^3 ))
summary(poly_tre)
polypred <- predict(poly_tre)


#Prövar en hurtig spline (variera df för bättre/sämre fit)
spl3 <- smooth.spline(x = intervalindex, y = males, df = 3)



#pröver en glm 
glmmodel <- glm(males ~intervalindex+ intervalindex^2 + intervalindex^3, family = gaussian)
summary(glmmodel)
glmpred <- predict(glmmodel)

exppred <- exp(predict(expmodel))

plot(males, col="black")
lines(pred, col="blue")
lines(spl3, col ="green" )
lines(polypred, col="orange")
lines(glmpred, col="pink")

