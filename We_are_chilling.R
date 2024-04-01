rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
ls()
getMe.libraries()


# Let;s continue with Lab 4

# Yesterday we did an amazing job with that function

DATA <- data.frame(ID = 1, SEX = "M", ER_REASON_V1 = "COVID",
                   ER_TIME_V1 = 3, ER_REASON_V2 = "Flu", ER_TIME_V2 = 6)
DATA <- rbind(DATA, c(2, "F", "Heartattack", 15, "COVID", 16))
DATA <- rbind(DATA, c(3, "F", "CarAccident", 53, "Flu", 8))
DATA <- rbind(DATA, c(4, "M", "Flu", 7, "COVID", 10))
DATA <- rbind(DATA, c(5, "M", "COVID", 12, "Fracture", 4))
DATA <- rbind(DATA, c(6, "F", "Fracture", 2, NA, NA))
DATA
?reshape
wide_f<-reshape(data = DATA,v.names=c("ER_REASON","ER_TIME"),direction = "long",varying = c("ER_REASON_V1","ER_TIME_V1","ER_REASON_V2","ER_TIME_V2"),times=1:2,timevar = "VISIT",idvar = "ID")
row.names(wide_f)<-NULL
wide_f
# wide_f$id<-NULL
# wide_f


# There we go
names(wide_f)[1]<-"ID"

wide_f$ID_SUBJECT<-1:nrow(wide_f)

wide_f<-wide_f[,c(ncol(wide_f),1:(ncol(wide_f)-1))]


# Ok, I know what he has done



class(wide_f$ER_TIME)
wide_f<-as.numeric(wide_f$ER_TIME)
class(wide_f)

class(wide_f$ER_TIME)
wide_f$ER_TIME <- as.numeric(wide_f$ER_TIME)


class(wide_f)


t1<-table(unlist(wide_f[,"ER_REASON"]))
t2<-data.frame(cbind(t1,prop.table(t1)*100))
names(t2) <- c("n","%")


wide_f


long_f<-reshape(data=wide_f[,-1],direction="wide",timevar = "VISIT",idvar = "ID_SUBJECT",v.names  = c("ER_REASON","ER_TIME"),sep="_V")

names(long_f)[2]<-"SEX"
# Sorry for the names, they are not correct (long_f,wide_f), it is the other way around


DATA<-datasets::mtcars

DATA$vs <- factor(DATA$vs,levels = c(0,1),labels = c("V-shaped","Straight"))
.desc.numeric(DATA$mpg,DATA$am,trim=0)
mean(DATA$mpg)
trimmed(data = DATA,trim=0,values="mpg")
trimmed
ceiling(0)
sort(sort(DATA$mpg)[-1:0],decreasing=T)[-1:0]
trimmed(data = DATA,trim=1,values="mpg")
rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()

DATA$am <- factor(DATA$am,levels = c(0,1),labels = c("Automatic","Manual"))

compare.samples(DATA$mpg,DATA$am,DEBUG=T)

# I CAN'T GET VALUES OUT OF MY OWN FUNCTION
# IT'S HORRIBLE!!

comparison <- compare.samples(DATA$mpg,DATA$am)
library(ggplot2)

ggplot(DATA,aes(x=am,y=mpg,fill=am))+
  geom_violin(alpha=0.3)+
geom_boxplot(alpha=0.5,width=0.3)+
  geom_jitter(alpha=0.7,width=0.1)+
  theme(legend.position = "none")+
  labs(x="",y="Miles per gallon")+
  annotate("text",x=2.2,y=13, label = paste0("bold(t):",round(comparison$statistic,digits=3)),parse = TRUE, hjust = 0)+
  annotate("text",x=2.2,y=12,label=paste("bold(df):",round(comparison$parameter,digits = 3)),parse = TRUE,hjust=0)+
  annotate("text",x=2.2,y=11,label=paste("bold(p):",round(comparison$p.value,digits = 3)),parse = TRUE,hjust=0)

thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime(DATA$mpg,DATA$am,DEBUG=TRUE)
?lapply


as.data.frame(shapiro.test(DATA$mpg))
sh<-shapiro.test(DATA$mpg)

class(sh) <- c(class(sh),"data.frame")
class(sh)
as.data.frame(sh)
attributes(sh)

sh_df <- data.frame(p.value=sh$p.value,statistic=sh$statistic)
row.names(sh_df)<-NULL
sh_df$p.value
sh_df
