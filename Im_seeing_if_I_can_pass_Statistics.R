rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()
DATA <- getMe.nas(datasets::mtcars)


# I MUST do my own functions


# We are going to be tight in time for sure

.desc.numeric(DATA$mpg,factor(DATA$gear))


# You also need to test for homocedasticity



compare.samples(DATA$mpg,factor(DATA$gear))

  bartlett.test(DATA$mpg~factor(DATA$gear))


  # I was doing a function that sees if one of them is not normal or all of them are normal

# So, we just have to keep doing that function I guess

thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime(DATA$mpg,DATA$gear)
  bartlett.test(DATA$mpg~DATA$gear)


  # I want to make a function that solves all my problems

# So that I needn't worry about

# Do not worry if your code is completely right, I just want you to work and have the mentality of winner



library(ggplot2)
ggplot(DATA, aes(x=hp,y=mpg))+
  geom_point()+
  geom_smooth(method = "lm")


summary(lm(mpg ~ hp, data = DATA))


model <- summary(lm(mpg ~ hp+ wt, data = DATA))


install.packages('GGally')
library('GGally')
GGally::ggpairs(DATA[,c("mpg","hp","wt")])


install.packages('car')
library('car')
tryCatch(car::vif(model),
         error = function(e){
           print("can't compute VIF:")
           print(e)
         })


knitr::kable(correlation::correlation(DATA[,c("mpg","hp","wt")]))


library('correlation')


shapiro.test(residuals(model))


plot(fitted(model), residuals(model))


abline(h = 0, lty = 2)


car::ncvTest(model)


saturated.model.vars <- c("mpg","hp","gear","cyl","carb")
weird.function.I.do.not.understand.but.later.on.you.will.understand.it<-function(saturated.model.vars, data = DATA){

DATA.sat.mod <- na.omit(data[,saturated.model.vars])
model.string.ind <- paste(names(data.sat.mod)[-1],collapse = " + ")
model.string <- paste(names(data.sat.mod)[1],"~",model.string.ind, collapse = "")
saturated.model <- lm(as.formula(model.string), data = data.sat.mod)
summary(saturated.model)

}


library(GGally)
GGally::ggpairs(DATA.sat.mod[,saturated.model.vars])










tryCatch(car::vif(saturated.model),
         error = function(e){
           print("can't compute VIF:")
           print(e)
         })


knitr::kable(correlation::correlation(DATA.sat.mod[,saturated.model.vars]))



# He talks a lot and he switches his cars to not coemission or something like that

# I am thinking on eating mango



# without emissions?

# Linear coregresison?

# What ?



nortest::lillie.test(residuals(saturated.model))



plot(fitted(saturated.model), residuals(saturated.model))
abline(h =  0, lty = 2)

source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/cdc.samp.R")
DATA <- cdc.samp
DATA



.desc.numeric(DATA$age,digits = 2)


class(desc(factor(DATA$gender)))
class(.desc.factor(DATA$gender))
desc(var = "gender",data = DATA)
rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()



