source("Equal Width.R")
source("Equal Frequency.R")
source("IEM.R")

data = iris

test1 = width_discretize(iris, 3)
test2 = freq_discretize(iris, 4)
test3 = IEM(iris)
