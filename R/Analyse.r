library(mgcv)
# Frederik wd
#setwd("C:\\Users\\frede\\OneDrive\\Dokumenter\\DTU\\4. Semester\\Fagprojekt\\ArsenikGit\\Data")
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

