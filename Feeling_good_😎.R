# Ok, let's go with Unit 8 again

# Gilvenko - Cantelli

rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()



n <- 16
mean_population <- 800
sd <- 40
mean_sample <- 775

denominator <- sd/sqrt(n)
numerator <- mean_sample - mean_population
Z <- numerator / denominator

qnorm(Z,mean=mean_population,sd=sd)


#-------------------------------------------------------------------------------------------------------------------------------------------------
mean_sample <- 12.4
# Giovenko - Cantelli

sd <- 0.1
 n <- 4
# Z = (X - u)/(o/sqrt(n))

se <- sd/(sqrt(n))
m <- 12.4

# You have got 2 Z's

# Standardization

#pnorm(Z.value) # where Z is (x - u) / o, o is sd, u is the mean and X is the Random Variable. Z is normalized or standardized

x1 <- 12.1
x2 <- 12.7
z1 <- (x1-12.4)/se
z2 <- (x2-12.4)/se
prob_z <- pnorm(sort(c(z1,z2),decreasing = F))
result <- diff(prob_z)


text_z <- round(sort(c(z1,z2),decreasing = T))
text_result <- paste("P(Z<=",text_z,")",sep = "")
text_result <- paste(unlist(text_result), collapse = "-")
text_result <- paste(text_result,"=",result)
cat("\n",text_result)


rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()


DATA <- getMe.nas(datasets::mtcars)
DATA
class(DATA)

rm(list=ls())
library(readxl)
DATA <- read_excel("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/Elevator Data.xlsx")
names(DATA) <- toupper(names(DATA))
names(DATA) <- toupper(gsub(" ","_",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub(".","_",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub("(","_",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub(")","_",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub("ª","_",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub("Á","A",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub("É","E",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub("Í","I",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub("Ó","O",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub("Ú","U",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub("º","_",names(DATA),fixed = TRUE))
names(DATA) <- toupper(gsub("Ñ","N",names(DATA),fixed = TRUE))




DATA<-getMe.nas(DATA)
x <- DATA$DV_SPEED_FPM
na.rm <- FALSE
digits <- Inf











x <- DATA$DV_SPEED_FPM
na.rm <- TRUE
digits <- Inf
mu <- 200

mu <- 200
x <- DATA$DV_SPEED_FPM
na.rm <- TRUE
digits <- Inf

getMe.One.Sample.Comparison(DATA$hp,mu = 200)

DATA <- getMe.nas(datasets::mtcars)




# Comparing two independent samples

# Wilcoxon-Mann-Whitney U

# Non-parametric tests

.desc.numeric(DATA$mpg,factor(DATA$am))
rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()


rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()
