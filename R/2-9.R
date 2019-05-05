library(ISLR)

dim(Auto)
summary(Auto)

# (a)
# quantitative: mpg, cylinders, displacement, horsepower, weight,
# acceleration, year
# qualitative: name, origin

# (b)
# apply the range function to the first seven columns of Auto
sapply(Auto[, 1:7], range)
#       mpg cylinders displacement horsepower weight acceleration year
# [1,]  9.0         3           68         46   1613          8.0   70
# [2,] 46.6         8          455        230   5140         24.8   82

# (c)
sapply(Auto[, 1:7], mean)
#         mpg    cylinders displacement   horsepower       weight acceleration 
#   23.445918     5.471939   194.411990   104.469388  2977.584184    15.541327 
#        year 
#   75.979592

sapply(Auto[, 1:7], sd)
#         mpg    cylinders displacement   horsepower       weight acceleration 
#    7.805007     1.705783   104.644004    38.491160   849.402560     2.758864 
#        year 
#    3.683737

# (d)
newAuto = Auto[-(10:85),]
dim(newAuto) == dim(Auto) - c(76,0)
newAuto[9,] == Auto[9,]
newAuto[10,] == Auto[86,]

sapply(newAuto[, 1:7], range)
#       mpg cylinders displacement horsepower weight acceleration year
# [1,] 11.0         3           68         46   1649          8.5   70
# [2,] 46.6         8          455        230   4997         24.8   82
sapply(newAuto[, 1:7], mean)
#         mpg    cylinders displacement   horsepower       weight acceleration 
#   24.404430     5.373418   187.240506   100.721519  2935.971519    15.726899 
#        year 
#   77.145570 
sapply(newAuto[, 1:7], sd)
#         mpg    cylinders displacement   horsepower       weight acceleration 
#    7.867283     1.654179    99.678367    35.708853   811.300208     2.693721 
#        year 
#    3.106217 

# (e)
pairs(Auto)
plot(Auto$mpg, Auto$weight)
# Heavier weight correlates with lower mpg.
plot(Auto$mpg, Auto$cylinders)
# More cylinders, less mpg.
plot(Auto$mpg, Auto$year)
# Cars become more efficient over time.

# (f)
pairs(Auto)
# See descriptions of plots in (e).
# All of the predictors show some correlation with mpg. The name predictor has 
# too little observations per name though, so using this as a predictor is 
# likely to result in overfitting the data and will not generalize well.