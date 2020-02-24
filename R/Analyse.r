library(lattice)



# Frederik wd
#setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\Data")

# Asger wd
#setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i vand/Data")

# Joachim wd
setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")

fblad <- read.table("fblad.sw.dat", header=TRUE)
head(fblad)
fblad1 <- fblad[c(15:549), ]

N <- length(fblad1$events)

p.hat <- sum(fblad1$events/N)

# sæt en formel til glm
conc <- fblad1$conc
age <- fblad1$age
pop <- fblad1$at.risk
# flere variable...

analysis<-glm(events~conc + age,family=poisson(link=log),data=fblad1) 

# Hvor god er modellen 
drop1(analysis, test="Chisq")

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis,se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)


prediction.data.original<-exp(prediction.data)

# plots i log transformation 
plot(var,prediction.data$pred, col="blue")
lines(var,prediction.data$lower, col="red")
lines(var,prediction.data$upper, col="red")


#index for undersøgelses punkt 
i1 <- 255
# sandsynlighhed for at få kræft i indexpunktet 
p1 <- fblad1$events[i1]/fblad1$at.risk[i1]*100
#plot af sandsynlighed for kræft i orginal data 
plot(prediction.data.original$pred,var)
lines(prediction.data.original$lower,var, col="red")
lines(prediction.data.original$upper,var, col="red")
points(var[i1],p1,col="green")



