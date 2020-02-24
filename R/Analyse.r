# Frederik wd
#setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\Data")

# Asger wd
#setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i vand/Data")

# Joachim wd
#setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")

#Oskar wd 
# setwd("C:\\Users\\User\\OneDrive - Danmarks Tekniske Universitet\\SAS_030919\\4. Semester\\42584_Fagprojekt\\Arsenik i drikkevand\\42584_Data\\arsenik_i_vand\\Data")


fblad <- read.table("fblad.sw.dat", header=TRUE)
head(fblad)

# Antal observationer
N <- length(fblad$events)

# p.hat (empirisk sandsynlighed)
p.hat <- sum(fblad$events/N)


# Her defineres modellen:
analysis<-glm(events~conc+age,family=poisson(link=log),data=fblad,offset=log(at.risk)) 
summary(analysis)

# Hvor god er modellen 
drop1(analysis, test="Chisq")

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis,se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

prediction.data.original<-exp(prediction.data)
prediction.data.original <- prediction.data.original[order(prediction.data.original$pred),]


# plots i log transformation 
plot(prediction.data.original$pred, fblad$events[order(prediction.data.original$pred)], col="blue")
lines(prediction.data$lower, fblad$events, col="red")
lines(prediction.data$upper, col="red")

# JEG FORSTÅR IKKE KODEN HERUNDER!!!
#index for undersøgelses punkt 
i1 <- 255
# sandsynlighhed for at få kræft i indexpunktet 
p1 <- fblad$events[i1]/fblad$at.risk[i1]*100
#plot af sandsynlighed for kræft i orginal data 
plot(prediction.data.original$pred,var)
lines(prediction.data.original$lower,var, col="red")
lines(prediction.data.original$upper,var, col="red")
points(var[i1],p1,col="green")

hist(log(fblad$events/fblad$at.risk))
cbind(fblad[fblad$events>4,] ,log(fblad$events/fblad$at.risk)[fblad$events > 4])

