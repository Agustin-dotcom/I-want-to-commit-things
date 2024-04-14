rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()
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



nortest::lillie.test(DATA$DV_SPEED_FPM)
length(DATA$DV_SPEED_FPM)




median(DATA$DV_SPEED_FPM,na.rm = TRUE)
table(DATA$DV_SPEED_FPM < median(DATA$DV_SPEED_FPM,na.rm=TRUE))


table(DATA$DV_SPEED_FPM > median(DATA$DV_SPEED_FPM,na.rm=TRUE))

#Hmm.. we haven't got the same result as the pdf


wilcox.test(DATA$DV_SPEED_FPM, mu = 200)


# We have rejected the null hypothesis as true location is not equal to 200 and we have also got the p.values < 0.05



.desc.numeric(DATA$DV_SPEED_FPM, DATA$DEVICE_TYPE,na.rm = TRUE,trim=0,digits=Inf)


wilcox.test(DATA$DV_SPEED_FPM ~ DATA$DEVICE_TYPE)


DATA_0 <- datasets::mtcars


wilcox.test(DATA_0$mpg ~ factor(DATA_0$am))


# We reject the null hypothesis, therefore we say there IS a significant statistical difference



.desc.numeric(DATA_0$mpg, factor(DATA_0$am),na.rm = T)


