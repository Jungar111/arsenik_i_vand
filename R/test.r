
file = 'Data/wells.txt'
wellData <- read.table(file, header = TRUE, sep = "", dec = ".")

plot(data)

summary <- glm(conc ~ nwell,family = poisson(link = log), data = wellData)
plot(summary)
