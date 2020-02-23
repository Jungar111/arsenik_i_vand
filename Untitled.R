file = 'Data/wells.txt'
well.data <- read.table(file, header = TRUE, sep = "", dec = ".")

summary <- glm(conc ~ nwell,family = poisson(link = log), data = well.data)


plot(summary)