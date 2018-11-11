########### RESPONSE VARIABLE #########

house.data %>% 
  # mutate(SalePrice = log(SalePrice)) %>%
  mutate(SalePrice = log((SalePrice^1.4 - 1)/1.4)) %>%
  # mutate(SalePrice = (SalePrice^lambda - 1)/lambda) %>%
  ggplot() +
  # geom_histogram(aes(x=SalePrice))
  geom_qq(aes(sample=SalePrice)) +
  geom_qq_line(aes(sample=SalePrice), col= 'red')

fit.d <- fitdistr(my.house.data$SalePrice, 'exponential')
ks.test(log((house.data$SalePrice^1.4 - 1)/1.4),'pnorm')

x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)
# Does x come from a shifted gamma distribution with shape 3 and rate 2?
ks.test(x, "pnorm") # two-sided, exact
