library("mgcv")

# Frederik wd
# setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\ArsenikGit\\Data")

# Asger wd
<<<<<<< HEAD
setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i GIT/Data")
=======
#setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i GIT/Data")
>>>>>>> Modeltest

# Joachim wd
#setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")

#Oskar wd 
setwd("C:\\Users\\User\\OneDrive - Danmarks Tekniske Universitet\\SAS_030919\\4. Semester\\42584_Fagprojekt\\Arsenik i drikkevand\\42584_Data\\Arsenik i vand\\Data")

set.seed(69)

fblad <- read.table("fblad.sw.dat", header=TRUE)
mblad <- read.table("mblad.sw.dat", header=TRUE)
fblad$gender <- "Female"
mblad$gender <- "Male"
<<<<<<< HEAD
blad <- rbind(fblad,mblad)

head(blad)
#blad <- blad[c(15:549), ]
=======
fblad$female <- 1
mblad$female <- 0
blad <- rbind(fblad,mblad) 

blad[c(175:220), ]

head(blad)
#blad <- blad[c(15:549), ] 
>>>>>>> Modeltest

# Antal observationer
N <- length(blad$events)

# p.hat (empirisk sandsynlighed)
p.hat <- sum(blad$events/N)

max(blad$conc)

# Her defineres modellen:
<<<<<<< HEAD
analysis<-glm(events~conc+age,family=poisson(link=log),data=blad,offset=log(at.risk)) 
summary(analysis)


=======

analysis<-glm(events ~ conc + age + gender,family=poisson(link=log),data=blad,offset=log(at.risk))

summary(analysis)

analysis<-lm(events ~ conc + age + gender,data=blad,offset=log(at.risk))

plot(analysis)
>>>>>>> Modeltest
# Hvor god er modellen 
drop1(analysis, test="Chisq")

# Taylor udvider til ny analysis
analysis2 <- update(analysis, ~.+I(1/2*conc^2)+I(1/2*age^2))
analysis2 <- update(analysis2, ~.+I(1/3*conc^3))
summary(analysis2)

# Hvor god er modellen 
drop1(analysis2, test="Chisq")

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis2,se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

<<<<<<< HEAD
prediction.data.original <- exp(prediction.data)
prediction.data.original <- prediction.data.original[order(blad$events, decreasing = TRUE),]
=======
# Transformerer dataen tilbage til original tilstand og sorterer data efter pred
prediction.data.original <- exp(prediction.data)
prediction.data.original <- prediction.data.original[order(prediction.data.original$pred),]
>>>>>>> Modeltest


# Sorterede plots i original data transformation 
plot(prediction.data.original$pred, blad$events[order(prediction.data.original$pred)], col="blue")
lines(prediction.data.original$lower, blad$events[order(prediction.data.original$pred)], col="red")
lines(prediction.data.original$upper, blad$events[order(prediction.data.original$pred)], col="red")


# Laver foreløbig test, tror det her er den rigtige måde at plotte det på
cbind(blad$events[order(blad$events, decreasing = TRUE)],round(prediction.data.original$pred,1))
plot(blad$events[order(blad$events, decreasing = TRUE)],round(prediction.data.original$pred,0))


#plot i log transformation 
plot(prediction.data$pred, blad$events[order(prediction.data$pred)], col="blue")
lines(prediction.data$lower, blad$events, col="red")
lines(prediction.data$upper, blad$events, col="red")

# Histogram over log transformeret procent for at få kræft
hist(log(blad$events/blad$at.risk))

# Data til histogrammet oven over
cbind(blad[blad$events>4,] ,log(blad$events/blad$at.risk)[blad$events > 4])

# Taylor udvider til ny analysis
analysis2 <- update(analysis, ~.+I((age)^2) + I(conc^2))
analysis2 <- update(analysis2, ~.+I((conc)^3))
analysis2 <- update(analysis2, ~.+I(conc^4))
analysis2 <- update(analysis2, ~.+I(conc^5))
analysis2 <- update(analysis2, ~.+I(conc^6))
analysis2 <- update(analysis2, ~.+I(conc^7))
summary(analysis2)

plot(analysis2)

analysis2 <- gam(events~gender+s(age,by=female)+s(age)+s(conc)+
                   s(age,conc)+offset(I(log(at.risk))),
                 family=poisson(link = "log"),
                 data=blad)

# Hvor god er modellen 
drop1(analysis2, test="Chisq")

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis2,se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

prediction.data.original <- exp(prediction.data)
prediction.data.original <- prediction.data.original[order(blad$events, decreasing = TRUE),]


# plots i original data transformation 
#plot(prediction.data.original$pred, blad$events, col="blue")
#lines(prediction.data.original$lower, blad$events, col="red")
#lines(prediction.data.original$upper, blad$events, col="red")
<<<<<<< HEAD
=======

maxr <- 100

# Laver foreløbig test, tror det her er den rigtige måde at plotte det på
plot(round(prediction.data.original$pred,2),blad$events[order(blad$events, decreasing = TRUE)], xlim=c(0, maxr), ylim=c(0, maxr))
lines(0:maxr,0:maxr, type="l")

v = vector()
x = vector()

blad$events<- blad$events[order(blad$events, decreasing = TRUE)]

for (i in 1:length(prediction.data.original$pred)){
  res <- mean(blad$events[0.1*(i-1) <= prediction.data.original$pred & prediction.data.original$pred < 0.1*i])
  v = c(v,res)
  x = c(x, 0.1*i)
}

sd.Pred <- sd(prediction.data.original$pred)

length(prediction.data.original$upper)
plot(x, v, xlim=c(0, maxr), ylim = c(0,maxr))
lines(0:maxr,0:maxr, type="l")
lines(0:maxr+sd.Pred,0:maxr, type="l", col = "red")
lines(0:maxr-sd.Pred,0:maxr, type="l", col = "red")
plot(analysis2$residuals)
>>>>>>> Modeltest

maxr <- 85

<<<<<<< HEAD
# Laver foreløbig test, tror det her er den rigtige måde at plotte det på
cbind(blad$events[order(blad$events, decreasing = TRUE)],round(prediction.data.original$pred,1))
plot(blad$events[order(blad$events, decreasing = TRUE)],round(prediction.data.original$pred,2), xlim=c(0, maxr), ylim=c(0, maxr))
plot(round(prediction.data.original$pred,2),blad$events[order(blad$events, decreasing = TRUE)], xlim=c(0, maxr), ylim=c(0, maxr))
lines(0:maxr,0:maxr, type="l")
=======
a#plot i log data 
plot(prediction.data$pred, blad$events[order(prediction.data$pred)], col="blue")
lines(prediction.data$lower, blad$events, col="red")
lines(prediction.data$upper, blad$events, col="red")
>>>>>>> Modeltest

blad$events[order(blad$events, decreasing = TRUE)]

<<<<<<< HEAD

#plot i log data 
plot(prediction.data$pred, blad$events[order(prediction.data$pred)], col="blue")
lines(prediction.data$lower, blad$events, col="red")
lines(prediction.data$upper, blad$events, col="red")
<<<<<<< HEAD
=======
gm <- gam(events~gender+s(age,by=female)+s(age)+s(conc)+
      s(age,conc)+offset(I(log(at.risk))),
    family=poisson(link = "log"),
    data=blad)

plot(gm)
>>>>>>> Modeltest
=======

>>>>>>> dfbb22b3e2ff0afc2ae0a2b73cbfdfba48f7c1db
