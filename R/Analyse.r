# Frederik wd
#setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\ArsenikGit\\Data")

# Asger wd
#setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i vand/Data")

# Joachim wd
#setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")

#Oskar wd 
 setwd("C:\\Users\\User\\OneDrive - Danmarks Tekniske Universitet\\SAS_030919\\4. Semester\\42584_Fagprojekt\\Arsenik i drikkevand\\42584_Data\\arsenik_i_vand\\Data")


fblad <- read.table("fblad.sw.dat", header=TRUE)
head(fblad)
#fblad <- fblad[c(15:549), ]

# Antal observationer
N <- length(fblad$events)

# p.hat (empirisk sandsynlighed)
p.hat <- sum(fblad$events/N)


# Her defineres modellen:
analysis<-glm(events~sqrt(conc)+age,family=poisson(link=log),data=fblad,offset=log(at.risk)) 
summary(analysis)



# Hvor god er modellen 
drop1(analysis, test="Chisq")

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis,se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

# Transformerer dataen tilbage til original tilstand og sorterer data efter pred
prediction.data.original <- exp(prediction.data)
prediction.data.original <- prediction.data.original[order(prediction.data.original$pred),]


# Sorterede plots i original data transformation 
plot(prediction.data.original$pred, fblad$events[order(prediction.data.original$pred)], col="blue")
lines(prediction.data.original$lower, fblad$events[order(prediction.data.original$pred)], col="red")
lines(prediction.data.original$upper, fblad$events[order(prediction.data.original$pred)], col="red")


# Laver foreløbig test, tror det her er den rigtige måde at plotte det på
cbind(fblad$events[order(fblad$events, decreasing = TRUE)],round(prediction.data.original$pred,1))
plot(fblad$events[order(fblad$events, decreasing = TRUE)],round(prediction.data.original$pred,0))


#plot i log transformation 
plot(prediction.data$pred, fblad$events[order(prediction.data$pred)], col="blue")
lines(prediction.data$lower, fblad$events, col="red")
lines(prediction.data$upper, fblad$events, col="red")

# Histogram over log transformeret procent for at få kræft
hist(log(fblad$events/fblad$at.risk))

# Data til histogrammet oven over
cbind(fblad[fblad$events>4,] ,log(fblad$events/fblad$at.risk)[fblad$events > 4])

# Taylor udvider til ny analysis
analysis2 <- update(analysis, ~.+I(sqrt(conc)^2)+I(age^2))
summary(analysis2)

# Hvor god er modellen 
drop1(analysis2, test="Chisq")

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis2,se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

prediction.data.original <- exp(prediction.data)
prediction.data.original <- prediction.data.original[order(fblad$events, decreasing = TRUE),]


# plots i original data transformation 
plot(prediction.data.original$pred, fblad$events, col="blue")
lines(prediction.data.original$lower, fblad$events, col="red")
lines(prediction.data.original$upper, fblad$events, col="red")


# Laver foreløbig test, tror det her er den rigtige måde at plotte det på
cbind(fblad$events[order(fblad$events, decreasing = TRUE)],round(prediction.data.original$pred,1))
plot(fblad$events[order(fblad$events, decreasing = TRUE)],round(prediction.data.original$pred,2), xlim=c(0, 25), ylim=c(0, 25))
fblad$events[order(fblad$events, decreasing = TRUE)]


#plot i log data 
plot(prediction.data$pred, fblad$events[order(prediction.data$pred)], col="blue")
lines(prediction.data$lower, fblad$events, col="red")
lines(prediction.data$upper, fblad$events, col="red")

