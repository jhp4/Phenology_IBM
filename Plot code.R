
library("ggplot2")


plantdata <- read.csv("plantsummaryhunger.csv")
polldata <- read.csv("pollsummaryhunger.csv")

str(plantdata)

plantdata$speciesid <- as.factor(plantdata$speciesid)
polldata$speciesid <- as.factor(polldata$speciesid)

plantgraph <- ggplot(data = plantdata, aes(x = season, y = population, color = speciesid)) +
  geom_line() +
  geom_point() + 
  labs(title = "Plant species persistence by poll hunger limit",
       subtitle = "(across 30 seasons)",
       y = "Population count", x = "Season") + 
  facet_grid(~ hunger)+
  theme_bw()+
  theme(legend.position = "none")

plantgraph


pollgraph <- ggplot(data = polldata, aes(x = season, y = population, color = speciesid)) +
  geom_line() +
  geom_point() + 
  labs(title = "Pollinator species persistence by poll hunger limit",
       subtitle = "(across 30 seasons)",
       y = "Population count", x = "Season") + 
  facet_grid(~ hunger)+
  theme_bw()+
  theme(legend.position = "none")

pollgraph
