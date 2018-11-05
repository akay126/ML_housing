########### MISSINGNESS ###############

library(VIM)
VIM::aggr(house.data)

library(mice)
mice::md.pattern(house.data)

