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



library(readxl)
library(readr)
AarhusBefolkning <- read_csv("hele-befolkningen-1-januar-2013.csv", skip = 1)

grundvand <- read_excel("grundvand-2.xls")
grundvand <- grundvand[grundvand$BORINGSANVENDELSE == "Vandværksboring" & grundvand$AKTIV_INDVINDING_JN  == "J",]

viby <- grundvand[grundvand$ANLAEGS_NAVN == "Vibyværket",]
mårslet <- grundvand[grundvand$ANLAEGS_NAVN == "Mårslet Vandværk",]
beder <- grundvand[grundvand$ANLAEGS_NAVN == "Bederværket",]

viby <- viby[viby$DATO_SENESTE_ANALYSE == "2020-04-23",]
viby$MEDIAN_ANALYSEVAERDI

mårslet <- mårslet[mårslet$DATO_SENESTE_ANALYSE == "2019-08-14",]
mårslet$MEDIAN_ANALYSEVAERDI

beder <- beder[beder$DATO_SENESTE_ANALYSE == "2020-04-02",]
beder$MEDIAN_ANALYSEVAERDI



studstrupConc <- 14
studsstrup.atrisk <- 842

studsstrup.atriskM <- studsstrup.atrisk/2
studsstrup.atriskF <- studsstrup.atrisk/2


(testListFemale[conclis == studstrupConc] - testListFemale[conclis == 0])*studsstrup.atrisk
(testListMale[conclis == studstrupConc] - testListMale[conclis == 0])*studsstrup.atrisk
