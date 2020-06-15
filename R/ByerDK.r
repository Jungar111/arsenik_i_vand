########### Aarhus ###############

Aapop <- read.table("Aarhus_pop.txt", skip=1, header=FALSE)
colnames(Aapop) <- c("age", "Male", "Female")
Aapop <- as.data.frame(Aapop)
Aapop


plot(Aapop$age,Aapop$Female, col="red", type="l", )
lines(Aapop$age,Aapop$Male, col="blue")



######### Lolland ##############

Lolpop <- read.table("Lolland_pop.txt", skip=1, header=FALSE)
colnames(Lolpop) <- c("age", "Male", "Female")
Lolpop <- as.data.frame(Lolpop)
Lolpop


plot(Lolpop$age,Lolpop$Female, col="red", type="l", )
lines(Lolpop$age,Lolpop$Male, col="blue")


######### Slagelse ##############

Slapop <- read.table("Slagelse_pop.txt", skip=1, header=FALSE)
colnames(Slapop) <- c("age", "Male", "Female")
Slapop <- as.data.frame(Slapop)
Slapop


plot(Slapop$age,Slapop$Female, col="red", type="l", )
lines(Slapop$age,Slapop$Male, col="blue")
