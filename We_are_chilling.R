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





?seq_along

DATA$vs

t1<-table(DATA$vs)

t1<-table(unlist(DATA[,"vs"]))


rbind(prop.table(t1,margin=1)*100)
t2<-data.frame(rbind(prop.table(t1,margin=1)*100))

t1<-data.frame(rbind(t1))
names(t1) <- paste0(names(t1),"_n")


names(t2) <- paste0(names(t2),"_%")


t.final <- cbind(t1,t2)[,order(c(2*(seq_along(t1)-1)+1,2 * (seq_along(t2))))]
t.final


getMe.summary(DATA$vs,DATA$am)
IWantThe(DATA$vs,DATA$am,"t")$description


?datasets::mtcars


# This is what he meant when he wanted us to show all the values on a data.frame



# Let's do a function doing this, it is not complicated

x <- DATA$vs # Just like here
y <- DATA$am
data<-DATA
DATA<-getMe.nas(DATA)
values <- c("vs","am")
# We are just going to do it with data
x<-"vs"
# Because otherwise, I am going to have
class(x)
# Well, you can pass by separate...
# It does need to be on one

# Now, we just put the freq and all as Fernando did
digits<-Inf
i<-2
# Now, 'x' and 'y' are mandatory and if 'data' is not null, then we use 'x' and 'y' as indices
ThisIsWhatHeMeantByDataFrame<-function(x,y,data=NULL,na.rm=TRUE,digits=Inf){
    if(!is.null(data)){
      if(!is.data.frame(data)) stop("The data specified is not a data frame.")
      else{
        if(!is.character(x) | !is.character(y)) stop("If you give 'data', you must put names on 'x' and 'y'.")
        else{
            x <- data[,x]
            y <- data[,y]
        }
      }
    }
  if(na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  }
    t1 <-table(unlist(x),unlist(y))
    t2 <- data.frame(rbind(round(prop.table(t1,margin=1)*100,digits=digits)))
    t1<-data.frame(rbind(t1))
    # names(t1)
    # names(t1[1,]) <- parse(names(t1[1,]),"_n")
    # attributes(t1)

    ## just putting '_n' ##
      names(t1) <- paste0(names(t1),"_n")
      names(t2) <- paste0(names(t2),"_%")
    ## just putting '_n' ##
    # Now, I want to merge these two tables
    t.final <- cbind(t1,t2)[,c(2*(seq_along(t1)-1)+1,2*seq_along(t2))]
    # cbind(t1,t2)[,c(1,3,2,4)]

    t.final <- cbind(ohhYeah=rownames(t.final),t.final)
    rownames(t.final) <- NULL
    return(t.final)
}
# data.frame(rbind(1,2))


ThisIsWhatHeMeantByDataFrame("vs","am",data=DATA,na.rm=TRUE,digits=2)
getMe.summary(DATA$vs,DATA$am)$p.value


x<-DATA$vs
y<-DATA$am
