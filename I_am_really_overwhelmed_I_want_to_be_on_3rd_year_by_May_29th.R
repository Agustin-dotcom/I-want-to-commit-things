v<-c(rnorm(50),-555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555)

.desc.numeric(rnorm(v))



all(class(v))
class(v)

all(class(v))


class(v)<-c(class(v),"agustin")
all(class(v)!="agustin")
?all


# We should NEVER have an else

# Unless we are computing values


rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()

DATA <- datasets::mtcars


.desc.numeric(DATA$mpg,c("a","b","c","d","e"))
!is.factor(c("a","b","c","d","e"))
compare.samples(DATA$mpg,c("a","b","c","d","e"))

# You are doing well, Agustín

# I must keep going

# Do not quit, Agustín

# I cannot believe Giulia cried while coding

# It reminded me of me crying with OC

# I want to quit, I do not want to do anything

# Nevertheless, I am going to keep going
# And I am going to crush my upcoming exams
# Because no one is coming to save you

# Cry, please cry because it will make you feel better

# I feel really disgusted about myself: I told my dad something about Ainara and I feel bad now

# I am so mean with people
# And so disgusting as well, saying 'son of a bitch' to Tomasso, Patrik and even my sister

# Do I still know what respect is?

# I do not think so

# I want to be this cool nasty guy

# When really, you do not have to be a prick

# You did a nice gesture with Giulia, by the way

# I do not regret that

# Let's keep coding little by little



# I do not have respect for Fernando

# That is why I sent him a short video


# I am so grateful I am struggling with Statistics inasmuch I am going to learn something from this

# Trust me, there is no small enemy

# I have my Statistics exam failed and I am so sad

# I am so disappointed of myself

# I want to do better

# I want to compensate


# I want you to cry because I know you are going to feel better








?sapply



nortest::lillie.test(DATA$DV_SPEED_FPM)

length(DATA$DV_SPEED_FPM)
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

# Can you make this a function? Of course you can

x <- DATA$DV_SPEED_FPM
na.rm <- T


# table(DATA$DV_SPEED_FPM < median(DATA$DV_SPEED_FPM,na.rm=T))
# .desc.numeric(DATA$DV_SPEED_FPM)

# table(DATA$DV_SPEED_FPM > median(DATA$DV_SPEED_FPM,na.rm = T))

first_term <- table(x<median(x,na.rm=na.rm))[1]
second_term <- table(x<median(x,na.rm=na.rm))[2]

first_term_1 <- table(x>median(x,na.rm=na.rm))[1]
second_term_1 <- table(x>median(x,na.rm=na.rm))[2]

# table(x>median(x,na.rm=na.rm))
if(first_term!=second_term || first_term_1!=second_term_1) stop("There are not the same number of observations below and above the median.")


wilcox.test(DATA$DV_SPEED_FPM, mu=200)


# I want to do the exam

x <- NULL
equation <- function(x){(22*(x^2) - 855)/1197}

equation
# Between 6 and 9

function_mean <- function(x){x*((22*(x^2) - 855)/1197)}

mean <- integrate(function_mean,6,9)
mean <- round(mean$value, digits=3)
attributes(mean)


function_sd <- function(x){(x^2)*((22*(x^2) - 855)/1197)}

sd <- integrate(function_sd,6,9)$value - mean^2


sd <- sqrt(sd)
sd
round(sd,digits=3)

# P(7.577<X<8.881) = P(X<8.881) - P(X<7.577)

prob <- integrate(equation,7.577,8.881)$value
round(prob,digits=3)

prob_2 <- integrate(equation,6,8.82)$value
round(prob_2,digits=3)


brands <- c("Asus","Saphire","Gigabyte")

err_thr <- c(50.88/100,92.47/100,23.72/100)
names(err_thr) <- c("Asus","Saphire","Gigabyte")
brand_rates <- c(35.37/100,50.57/100,29.98/100)
names(brand_rates) <- c("Asus","Saphire","Gigabyte")
production_quota <- c(51.87/100,0.07/100,48.06/100)
names(production_quota) <- c("Asus","Saphire","Gigabyte")
brand_rates["Asus"]
err_thr["Asus"]
production_quota["Asus"]


value_I_Want<- brand_rates["Saphire"]*err_thr["Saphire"]
value_I_Want<-round(value_I_Want*100,digits=4)


round(brand_rates["Asus"] * (1-err_thr["Asus"])*100,digits=4)


round(brand_rates["Saphire"] * (1-err_thr["Saphire"])*100,digits=4)


#P(ASUS/REJECTED)=( P(REJECTED/ASUS) AND P(ASUS) )/ P(REJECTED)
#P(ASUS/REJECTED)=(P(ASUS AND REJECTED)) / P(REJECTED)
#INDEPENDENT --> P(ASUS)=P(ASUS/REJECTED), P(REJECTED)=P(REJECTED/ASUS)


this_is_what_we_Want <- (brand_rates["Asus"]*err_thr["Asus"]) /(sum(err_thr*brand_rates))
round(this_is_what_we_Want*100,digits=4)


mean <- 823
sd <- 3.466


# "at least"

# A server handles the mails of a corporation. The corporation has a very high workers rotation,
# with a mean monthly number of employees of 823(sd:3.466). The workers that get an average of 165 messages every 5 day



sd <- 3.466
variance <- sd^2
variance


# P(6<=X) = 1 - P(x<=6) + P(X=6)

?ppois
1 - ppois(6,variance,lower.tail = FALSE) + dpois(6,variance)
round(ppois(6,variance,lower.tail=T) + dpois(6,variance),digits=4)



# P(X<x) = 54.59/100

#' per day -> per 24h

# P(X>x) = (100-54.59)/100


ppois(lower.tail = F,q = c(54.59/100),lambda = variance)


# 1 month -> 30 days -> 30*24 hours -> 30*24*60 min

ratio_min <- 6/(30*24*60)

# 165 msg -> 5 day



Raj_play <- c(60.48/100,0.0833,0.1135,0.0744,0.124)
names(Raj_play) <- c("spock","rock","paper","scissors","lizard")
Penny_play <- c(0.0947,0.0948,60.29/100,0.112,0.0956)
names(Penny_play) <- c("spock","rock","paper","scissors","lizard")

Raj_play["rock"]+Penny_play["rock"]


round(Penny_play["rock"]*Raj_play["paper"] + Penny_play["rock"]*Raj_play["spock"],digits=4)


round(Raj_play["rock"]*Penny_play["scissors"]+Raj_play["rock"]*Penny_play["lizard"]+Raj_play["paper"]*Penny_play["rock"]+Raj_play["paper"]*Penny_play["spock"]+
  Raj_play["scissors"]*Penny_play["paper"]+Raj_play["scissors"]*Penny_play["lizard"]+Raj_play["lizard"]*Penny_play["spock"]+Raj_play["lizard"]*
  Penny_play["paper"]+Raj_play["spock"]*Penny_play["rock"]+Raj_play["spock"]*Penny_play["scissors"],digits=4)


Raj_play["rock"]




mean <- 1.5621
sd <- 0.1876
qnorm(0.5,mean=mean,sd=sd)

qnorm(44.86/100,mean=mean,sd=sd)
?pnorm






round(qnorm((100-44.86)/100,mean = mean, sd = sd),digits=4)


round(pnorm(1.7097)-pnorm(1.3935),digits=4)

round(qnorm(0.5+((56.3/2)/100),mean = mean,sd=sd),digits=4)
pnorm(1.7079)-pnorm(-1.7079)


# qnorm(((56.3/2)/100)+pnorm(1.5621))

round(qnorm(.5-((56.3/2)/100)),digits = 4)
