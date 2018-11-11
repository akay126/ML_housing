# ANIMATED VISUALIZATIONS #
# install.packages('tweenr')
# library(tweenr)
devtools::install_github('thomasp85/gganimate')
# remove.packages('gganimate')
library(gganimate)
library(tibble)
####################

my.house.data %>% 
  mutate(SalePrice = exp(SalePrice)) %>% 
  group_by(YearBuilt,Neighborhood) %>% 
  summarise(LotArea = sum(LotArea), SalePrice = sum(SalePrice), GrLivArea = sum(GrLivArea), n = n()) %>% 
  complete(Neighborhood,YearBuilt, fill = list(LotArea = 0,
                                               SalePrice = 0,
                                               GrLivArea = 0,
                                               n = 0)) %>%
  arrange(Neighborhood,YearBuilt) %>%
  group_by(Neighborhood) %>%
  mutate(LotArea = cumsum(LotArea), SalePrice = cumsum(SalePrice),
         GrLivArea = cumsum(GrLivArea), n = cumsum(n)) %>%
  ggplot() +
  geom_point(aes(x=GrLivArea, y=SalePrice/LotArea,
                 size=n, col=reorder(Neighborhood,YearBuilt)),
             alpha=0.8) +
  theme(legend.position = "bottom")

#####################

ghost_points_ini <- tibble(
  YearBuilt = as.integer(1860),
  Neighborhood = 'NAmes',
  SalePrice = 0, n = 5)

ghost_points_fin <- tibble(
  YearBuilt = as.integer(2020),
  Neighborhood = 'NAmes',
  SalePrice = 0, n = 5)

real_points <- my.house.data %>% 
  mutate(SalePrice = exp(SalePrice)) %>% 
  group_by(YearBuilt,Neighborhood) %>% 
  summarise(SalePrice = mean(SalePrice), n = n())

gg <- ggplot() +
  geom_point(aes(x=YearBuilt, y=SalePrice,
                 size=n, color=Neighborhood,
                 frame=YearBuilt, cumulative=TRUE),
             data = real_points,alpha=0.8) +
  coord_cartesian(xlim = c(1872,2010)) +
  theme(legend.position = "bottom") +
  # Here comes the gganimate code
  labs(title = 'Year: {closest_state}', x = 'Lot Area', y = 'Above Ground Living Area') +
  # transition_time(YearBuilt) +
  transition_states(YearBuilt, transition_length = 3, state_length = 1)

animate(gg, detail = 2)

p <- ggplot(iris, aes(Sepal.Width, Petal.Width)) +
  geom_point() +
  labs(title = "{closest_state}") +
  transition_states(Species, transition_length = 3, state_length = 1)

animate(p, detail = 2)
