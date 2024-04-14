rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()

# Non-parametric tests: two sample comparison

DATA <- getMe.nas(datasets::mtcars)
wilcox.test(DATA$mpg ~ DATA$am)
# p<0.05

# We reject the null hypothesis

# We say there is an statistical difference between these two

.desc.numeric(DATA$mpg,factor(DATA$am))

# This is just doing wilcox.test

# Nevertheless, this will not be the case on the exam

# You should code again that function that decides which path to take


# Let's go with Kendall and more

# Correlation

