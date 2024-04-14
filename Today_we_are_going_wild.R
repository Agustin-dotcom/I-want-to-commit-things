# I am going to do a compare function


# First, I need to know whether my variables are numeric or categorical to be able to do correlation, normality, apply tapply, etc

# We have got 4 cases:

# N N
# N C
# C N
# C C

# Where 'C' stands for Categorical and 'N' for numeric

# DO NOT NEST

# And having 'C N' is the same as having 'N C' right? Because in both cases we divide de numeric variable into two distributions with the help of the categorical variable

rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()


DATA <- getMe.nas(datasets::mtcars)

x1 <- DATA$mpg
x2 <- DATA$hp

x3 <- DATA$mpg
x4 <- DATA$am

x5 <- DATA$am
x6 <- DATA$vs

x <- x1
y <- x2

# If one of them is not numeric nor categorical (factor), we report an error

na.rm <- TRUE
digits <- Inf
DEBUG <- TRUE
range <- c(-Inf,Inf)
compare<-function(x=NULL,y=NULL,digits=Inf,range=c(-Inf,Inf),na.rm = TRUE, mu = NULL, DEBUG = FALSE, paired = FALSE){
if (!((is.numeric(x)&&is.numeric(y))||
    (is.numeric(x)&&is.factor(y))||
    (is.factor(x)&&is.numeric(y))||
    (is.factor(x)&&is.factor(y))||
    (is.factor(x)&&is.null(y))||
    (is.numeric(x))&&is.null(y)||
    (is.null(x)&&is.numeric(y))||
    (is.null(x)&&is.factor(y)))) stop("'x' and 'y' must be either numerical or categorical.")

# And what if we only have one sample?
if(na.rm){
  x <- na.omit(x)
  y <- na.omit(y)
}
#if(is.null(x)&&is.null(y)) stop("You must introduce, at least, one sample.")


if(is.null(x)&&!is.null(y)) {
  x <- y
  y <- NULL
  }

switch (sum(c(!is.null(x),!is.null(y))),
  2 = we.compare.2.samples(x=x,y=y,digits=digits,range=range,na.rm = na.rm, DEBUG = DEBUG, paired = paired),
  1 = we.compare.1.sample(x=x,y=y,digits=digits,range=range, na.rm = na.rm, mu = mu) # DONE
)


}



we.compare.2.samples<-function(x=NULL,y=NULL,digits=Inf,range=c(-Inf,Inf),na.rm = TRUE, DEBUG = FALSE, paired = FALSE){
  if(is.numeric(x)&&is.numeric(y)) compare.2.numeric.variables(x=x,y=y,digits=digits,range=range,na.rm = na.rm, DEBUG = DEBUG) #TODO
  if(is.numeric(x)&&is.factor(y)) compare.x.numeric.y.factor(x=x,y=y,digits=digits,range=range,na.rm = na.rm, paired = paired) # DONE
  if(is.factor(x)&&is.numeric(y)) compare.x.factor.y.numeric(x=x,y=y,digits=digits,range=range,na.rm = na.rm, paired = paired) # DONE
  if(is.factor(x)&&is.factor(y)) compare.2.factor.variables(x=x,y=y,digits=digits,range=range,na.rm = na.rm) # TODO
}

we.compare.1.sample<-function(x=NULL,y=NULL,digits=Inf,range=c(-Inf,Inf),na.rm = TRUE, mu = NULL){

.result <- getMe.One.Sample.Comparison(x,mu = mu,digits = digits)
.result
}



compare.x.factor.y.numeric<-function(x=NULL,y=NULL,digits=Inf,range=c(-Inf,Inf),na.rm = TRUE, DEBUG = FALSE, paired = FALSE){
  compare.x.numeric.y.factor(x=y,y=x,digits=digits,range=range,na.rm=na.rm, DEBUG = DEBUG, paired = paired)
}

compare.x.numeric.y.factor<-function(x=NULL,y=NULL,digits=Inf,range=c(-Inf,Inf),na.rm = TRUE,DEBUG = FALSE, paired = FALSE){
  .desc.numeric(x=x,y=y,na.rm = na.rm,digits=digits)
  compare.samples(x = x, y = y, DEBUG = DEBUG, na.rm = na.rm, digits = digits, paired = paired) # Maybe it is better to re DO this one...
}

x <-  DATA$mpg
mu <- 200
y <- DATA$hp

compare.2.numeric.variables<-function(x=NULL,y=NULL,digits=Inf,range=c(-Inf,Inf),na.rm = TRUE){
  library('Kendall')

  .spearman <- cor.test(na.omit(x),na.omit(y),method = "spearman")
  .spearman.statistic <- NA
  .spearman.statistic <- .spearman$statistic
  .spearman.parameter <- NA
  .spearman.parameter <- .spearman$parameter
  .spearman.p.value <- NA
  .spearman.p.value <- .spearman$p.value


  .kendall <- Kendall::Kendall(x,y)
  .kendall.tau <- NA
  .kendall.tau <- .kendall$tau
  .kendall.sl <- NA
  .kendall.sl <- .kendall$sl
  .kendall.S <- NA
  .kendall.S <- .kendall$S
  .kendall.D <- NA
  .kendall.D <- .kendall$D
  .kendall.varS <- NA
  .kendall.varS <- .kendall$varS

  .test <- data.frame("spearman.statistic" = .spearman$statistic,
                      "spearman.parameter" = .spearman$parameter,
                      "spearman.p.value" = .spearman$p.value,
                      "kendall.tau" = .kendall$tau,
                      "kendall.sl" = .kendall$sl,
                      "kendall.S" = .kendall$S,
                      "kendall.D" = .kendall$D,
                      "kendall.varS" = .kendall$varS)
  if(is.normal(x) && is.normal(y)){
    .test <- NULL

    .pearson <- cor.test(x,y,method = "pearson")
    .pearson.statistic <- NA
    .pearson.statistic <- .pearson$statistic
    .pearson.parameter <- NA
    .pearson.parameter <- .pearson$parameter
    .pearson.p.value <- NA
    .pearson.p.value <- .pearson$p.value

    .pearson.estimate <- NA
    .pearson.estimate <- .pearson$estimate
    .pearson.conf.low <- NA
    .pearson.conf.low <- .pearson$conf.int[1]
    .pearson.conf.high <- NA
    .pearson.conf.high <- .pearson$conf.int[2]
    .pearson.conf.level <- NA
    .pearson.conf.level <- attr(.pearson$conf.int,"conf.level")

    .test <- data.frame("pearson.statistic" = .pearson.statistic,
                        "pearson.parameter" = .pearson.parameter,
                        "pearson.p.value" = .pearson.p.value,
                        "pearson.estimate" = .pearson.estimate,
                        "pearson.conf.low" = .pearson.conf.low,
                        "pearson.conf.high" = .pearson.conf.high,
                        "pearson.conf.level" = .pearson.conf.level )
  }
  .test
}


??warning
?cor.test
cor.test(x,y,method = "pearson")
cor()
?seq_along
install.packages('Kendall')
rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.libraries.R")
getMe.libraries()

# Primero, hace las cosas super simples y cuando te funcione, ya luego le vas cargando

# Vamos a hacer los dos test y ya esta


