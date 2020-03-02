# Frederik wd
# setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\ArsenikGit\\Data")

# Asger wd
setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i GIT/Data")

# Joachim wd
#setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")

#Oskar wd 
setwd("C:\\Users\\User\\OneDrive - Danmarks Tekniske Universitet\\SAS_030919\\4. Semester\\42584_Fagprojekt\\Arsenik i drikkevand\\42584_Data\\Arsenik i vand\\Data")


fblad <- read.table("fblad.sw.dat", header=TRUE)
mblad <- read.table("mblad.sw.dat", header=TRUE)
fblad$gender <- "Female"
mblad$gender <- "Male"
blad <- rbind(fblad,mblad)

head(blad)
#blad <- blad[c(15:549), ]

# Antal observationer
N <- length(blad$events)

# p.hat (empirisk sandsynlighed)
p.hat <- sum(blad$events/N)


# Her defineres modellen:
analysis<-glm(events~conc+age,family=poisson(link=log),data=blad,offset=log(at.risk)) 
summary(analysis)


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

prediction.data.original <- exp(prediction.data)
prediction.data.original <- prediction.data.original[order(blad$events, decreasing = TRUE),]


# plots i original data transformation 
#plot(prediction.data.original$pred, blad$events, col="blue")
#lines(prediction.data.original$lower, blad$events, col="red")
#lines(prediction.data.original$upper, blad$events, col="red")

maxr <- 85

# Laver foreløbig test, tror det her er den rigtige måde at plotte det på
cbind(blad$events[order(blad$events, decreasing = TRUE)],round(prediction.data.original$pred,1))
plot(blad$events[order(blad$events, decreasing = TRUE)],round(prediction.data.original$pred,2), xlim=c(0, maxr), ylim=c(0, maxr))
plot(round(prediction.data.original$pred,2),blad$events[order(blad$events, decreasing = TRUE)], xlim=c(0, maxr), ylim=c(0, maxr))
lines(0:maxr,0:maxr, type="l")

blad$events[order(blad$events, decreasing = TRUE)]


#plot i log data 
plot(prediction.data$pred, blad$events[order(prediction.data$pred)], col="blue")
lines(prediction.data$lower, blad$events, col="red")
lines(prediction.data$upper, blad$events, col="red")

