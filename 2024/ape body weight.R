library(readxl)
library(tidyverse)
chimps <- read_csv("https://bioanth.org/media/chimps.csv")
gorillas <-read_csv("https://bioanth.org/media/gorillas.csv")
gibbons <- read_csv("https://bioanth.org/media/gibbons.csv")

ggplot(chimps, aes(age, measurement_value, colour = sex)) + geom_point(alpha = 0.1) + geom_smooth()
ggplot(gorillas, aes(age, measurement_value, colour = sex)) + geom_point(alpha = 0.1) + geom_smooth()


ggplot(gibbons, aes(age, measurement_value, colour = sex)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth()


ggplot(gibbons[gibbons$age>10,], aes(measurement_date, measurement_value)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth()

ggplot(gorillas, aes(age, measurement_value, colour = sex)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth()

ggplot(chimps, aes(age, measurement_value, colour = sex)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()

ggplot(d_GH, aes(age, weight, colour = sex )) + 
  geom_point(alpha = 0.1) + 
  geom_smooth()
