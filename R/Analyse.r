# Joachim wd
setwd("/Users/JoachimPorsA/Documents/4. Semester - DTU/Fagprojekt/Data/Arsenik i vand/Data")

set.seed(69)

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
analysis<-glm(events~log(1 + conc) + age + I(age^2) + ,family=poisson(link=log),data=blad, offset=log(at.risk)) 
summary(analysis)

# Hvor god er modellen 
drop1(analysis, test="Chisq")

# Laver signifikans niveauer 
prediction.temp<-as.data.frame(predict(analysis,se.fit=T))
prediction.data<-data.frame(pred=prediction.temp$fit, upper=prediction.temp$fit+ 1.96*prediction.temp$se.fit, lower=prediction.temp$fit-1.96*prediction.temp$se.fit)

# Residual sum of squares (SSE)
xxi <- prediction.data[1]
xxgen <- mean(blad$events)
sum((xxi - xxgen)^2)

maxr <- 5

# Laver foreløbig test, tror det her er den rigtige måde at plotte det på
plot(round(prediction.data.original$pred,2),blad$events[order(blad$events, decreasing = TRUE)], xlim=c(0, maxr), ylim=c(0, maxr))
lines(0:maxr,0:maxr, type="l")

v = vector()
x = vector()

blad$events<- blad$events[order(blad$events, decreasing = TRUE)]

for (i in 1:length(prediction.data.original$pred)){
  res <- mean(blad$events[0.1*(i-1) <= prediction.data.original$pred & prediction.data.original$pred < 0.1*i])
  v = c(v,res)
  x = c(x, 0.1*i)}

plot(x, v, xlim=c(0, maxr), ylim=c(0, maxr))
lines(0:maxr,0:maxr, type="l")

