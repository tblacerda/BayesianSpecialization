# Prior Predictive Simulation
dat <- read.table("cookies.txt", header=TRUE)
head(dat)
table(dat$location)

boxplot(chips ~ location, data = dat)
