rm(list=ls()) 
library(Hmisc)

data <- read.csv("COVID19_line_list_data.csv")
describe(data)

# cleaned up data
data$deaths_dummy <- as.integer(data$death != 0)
sum(data$deaths_dummy) / nrow(data) # mortalitate = 0.058064

# Varsta
# H0: oamenii care au murit sunt mai batrani
deaths = subset(data, death_dummy == 1)
cured = subset(data, death_dummy == 0)
mean(deaths$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# Statistic significat?
z.test(cured$age, deaths$age, alternative="two.sided", conf.level = 0.99)
# diferenta dintre o persoana care moare si una care traieste este in intervalul -25.52122 si -15.50661
# p-value is 2.2e-16 ~ 0 < 0.05, deci oamenii care au murit de Covid 19 sunt mai batrani


# Gen
# H0: genul nu are efect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$deaths_dummy, na.rm = TRUE) #8.5%!
mean(women$deaths_dummy, na.rm = TRUE) #3.7%
# Statistic significat?
z.test(men$deaths_dummy, women$deaths_dummy, alternative="two.sided", conf.level = 0.99)
# Barbatii au cu 0,8% pana la 8.8% sanse mai mari de mortalitate in cazul infectarii cu Covid19
# p-value = 0.002 < 0.05, semnificativ statistic
