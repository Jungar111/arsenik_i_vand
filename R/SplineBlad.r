library("mgcv")

# Frederik wd
# setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\ArsenikGit\\Data")

# Asger wd
setwd("/Users/AsgerSturisTang/OneDrive - Danmarks Tekniske Universitet/DTU/4. Semester/Arsenik i GIT/Data")

# Joachim wd
#setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")

#Oskar wd 
# setwd("C:\\Users\\User\\OneDrive - Danmarks Tekniske Universitet\\SAS_030919\\4. Semester\\42584_Fagprojekt\\Arsenik i drikkevand\\42584_Data\\arsenik_i_vand\\Data")

set.seed(69)

fblad <- read.table("fblad.txt", header=TRUE)
mblad <- read.table("mblad.txt", header=TRUE)
fblad$gender <- "Female"
mblad$gender <- "Male"
fblad$female <- 1
mblad$female <- 0
blad <- rbind(fblad,mblad) 

N <- length(blad$events)

analysis <- gam(events~s(age)+s(log(1+conc))+s(age,by=female)+s(age,conc)+
                   offset(I(log(at.risk))),
                 family=poisson(link = "log"),
                 data=blad)
# AIC
AIC(analysis)

plot(analysis)

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis,se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

prediction.data.original <- exp(prediction.data)
prediction.data.original <- prediction.data.original[order(blad$events, decreasing = TRUE),]


# plots i original data transformation 
#plot(prediction.data.original$pred, blad$events, col="blue")
#lines(prediction.data.original$lower, blad$events, col="red")
#lines(prediction.data.original$upper, blad$events, col="red")

maxr <- 150

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
plot(analysis$residuals)
