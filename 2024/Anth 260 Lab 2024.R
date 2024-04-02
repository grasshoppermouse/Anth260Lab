library(HistData)
library(conflicted)

(78.5 + 1.08*67.0)/2

mean(GaltonFamilies$father)
mean(GaltonFamilies$mother)

galton2 <- GaltonFamilies[!duplicated(GaltonFamilies$family),]

mean(galton2$father)
mean(galton2$mother)

hist(galton2$father)
hist(galton2$mother)

plot(GaltonFamilies$midparentHeight, GaltonFamilies$childHeight)

library(tidyverse)
ggplot(GaltonFamilies, aes(midparentHeight, childHeight, colour = gender)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  geom_abline(intercept = 0, slope = 1)

cor.test(GaltonFamilies$midparentHeight, GaltonFamilies$childHeight)

sons <- GaltonFamilies[GaltonFamilies$gender == 'male',]

cor.test(sons$midparentHeight, sons$childHeight)
cor.test(sons$father, sons$childHeight)
cor.test(sons$mother, sons$childHeight)

m <- lm(childHeight ~ father, sons)
summary(m)

m <- lm(childHeight ~ midparentHeight, sons)
summary(m)

daughters <- GaltonFamilies[GaltonFamilies$gender == 'female', ]

cor.test(daughters$midparentHeight, daughters$childHeight)

m <- lm(childHeight ~ midparentHeight, daughters)
summary(m)

plot(galton2$father, galton2$mother)
cor.test(galton2$father, galton2$mother)

library(tidyverse)
sons <- GaltonFamilies[GaltonFamilies$gender == 'male',]
ggplot(sons, aes(midparentHeight, childHeight)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  geom_abline(intercept = 0, slope = 1)


library(ape)

sequences <- c(
  "a", "a", "c", "g", "c", "t", "a", "c",
  "a", "a", "c", "g", "a", "t", "a", "c",
  "a", "t", "c", "g", "c", "t", "g", "c",
  "c", "t", "c", "g", "c", "t", "g", "c"
)

sequences <- matrix(sequences, nrow = 4, ncol = 8, byrow = T)
sequences

d <- dist.gene(sequences)
d
out <- hclust(d, method = 'average')
plot(out)

# Ape data


