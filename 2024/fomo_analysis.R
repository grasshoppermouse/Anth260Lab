library(tidyverse)

fomo <- read_csv("")

world <- map_data("world")
ggplot(world, aes(long, lat, group = group)) +
  geom_polygon() +
  geom_point(data = fomo, aes(Longitude, Location), group = 0, colour = 'red')
