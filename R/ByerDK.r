Aapop <- read.table("Aarhus_pop.txt", skip=1, header=FALSE)
colnames(Aapop) <- c("age", "Male", "Female")
Aapop <- as.data.frame(Aapop)
Aapop


plot(Aapop$age,Aapop$Female, col="red", type="l", )
lines(Aapop$age,Aapop$Male, col="blue")
