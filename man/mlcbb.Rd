\name{mlcbb}
\alias{mlcbb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Auxiliary function used by the function mlcbbsel}
\description{
 Auxiliary function used by the function mlcbbsel
}

\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }


\examples{

## The function is currently defined as
function(u,v,thetamin,deltamin,thetamax,deltamax, copulamodel=c("CBB1","CBB2" , "CBB3","CBB4","CBB5","CBB6", "CBB7","CBB8", "CBB9", "CBB10","CMM1", "CMM2", "CMM3", "CMM4", "CMM5", "CMM6", "CMM7", "CMM8", "CMM9", "CMM10","CBM1", "CBM2", "CBM3", "CBM4", "CBM5", "CBM6", "CBM7", "CBM8", "CBM9", "CBM10","CMB1", "CMB2", "CMB3", "CMB4", "CMB5", "CMB6", "CMB7", "CMB8", "CMB9", "CMB10"))
{
PMAX<-0

DMAX<-20
TMAX<-20
if(missing(deltamax)){deltamax<- DMAX}
if(missing(thetamax)){thetamax<- TMAX}

if(missing(deltamin) && copulamodel=="CBB1"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CBB2"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CBB3"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CBB4"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CBB5"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CBB6"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CBB7"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CBB8"){deltamin<-0.005};
if(missing(deltamax) && copulamodel=="CBB8"){deltamax<-0.995};
if(deltamax==DMAX && copulamodel=="CBB8"){deltamax<-0.995}
if(missing(deltamin) && copulamodel=="CBB9"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CBB10"){deltamin<-0.005};

if(missing(deltamin) && copulamodel=="CMM1"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CMM2"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CMM3"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CMM4"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CMM5"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CMM6"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CMM7"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CMM8"){deltamin<-0.005};
if(missing(deltamax) && copulamodel=="CMM8"){deltamax<-0.995};
if(deltamax==DMAX && copulamodel=="CMM8"){deltamax<-0.995}
if(missing(deltamin) && copulamodel=="CMM9"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CMM10"){deltamin<-0.005};

if(missing(deltamin) && copulamodel=="CBM1"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CBM2"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CBM3"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CBM4"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CBM5"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CBM6"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CBM7"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CBM8"){deltamin<-0.005};
if(missing(deltamax) && copulamodel=="CBM8"){deltamax<-0.995};
if(deltamax==DMAX && copulamodel=="CBM8"){deltamax<-0.995}
if(missing(deltamin) && copulamodel=="CBM9"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CBM10"){deltamin<-0.005};


if(missing(deltamin) && copulamodel=="CMB1"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CMB2"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CMB3"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CMB4"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CMB5"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CMB6"){deltamin<-1.005};
if(missing(deltamin) && copulamodel=="CMB7"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CMB8"){deltamin<-0.005};
if(missing(deltamax) && copulamodel=="CMB8"){deltamax<-0.995};
if(deltamax==DMAX && copulamodel=="CMB8"){deltamax<-0.995}
if(missing(deltamin) && copulamodel=="CMB9"){deltamin<-0.005};
if(missing(deltamin) && copulamodel=="CMB10"){deltamin<-0.005};


if(missing(thetamin) && copulamodel=="CBB1"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CBB2"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CBB3"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CBB4"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CBB5"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CBB6"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CBB7"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CBB8"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CBB9"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CBB10"){thetamin<-0.005};
if(missing(thetamax) && copulamodel=="CBB10"){thetamax<-0.995};
if(thetamax==TMAX && copulamodel=="CBB10"){thetamax<-0.995}




if(missing(thetamin) && copulamodel=="CMM1"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CMM2"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CMM3"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CMM4"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CMM5"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CMM6"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CMM7"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CMM8"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CMM9"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CMM10"){thetamin<-0.005};
if(missing(thetamax) && copulamodel=="CMM10"){thetamax<-0.995};
if(thetamax==TMAX && copulamodel=="CMM10"){thetamax<-0.995}






if(missing(thetamin) && copulamodel=="CBM1"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CBM2"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CBM3"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CBM4"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CBM5"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CBM6"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CBM7"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CBM8"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CBM9"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CBM10"){thetamin<-0.005};
if(missing(thetamax) && copulamodel=="CBM10"){thetamax<-0.995};
if(thetamax==TMAX && copulamodel=="CBM10"){thetamax<-0.995}




if(missing(thetamin) && copulamodel=="CMB1"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CMB2"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CMB3"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CMB4"){thetamin<-0.005};
if(missing(thetamin) && copulamodel=="CMB5"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CMB6"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CMB7"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CMB8"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CMB9"){thetamin<-1.005};
if(missing(thetamin) && copulamodel=="CMB10"){thetamin<-0.005};
if(missing(thetamax) && copulamodel=="CMB10"){thetamax<-0.995};
if(thetamax==TMAX && copulamodel=="CMB10"){thetamax<-0.995}





n<-sum(u>=-1)
m<-5
inct<-(thetamax-thetamin)/(m+1)
incd<-(deltamax-deltamin)/(m+1)
PMAX <- -10**(250)
for (tet in 1:m){ for (del in 1:m){ 
teta  <- thetamin + tet*inct
delta <- deltamin + del*incd

pvalor<-clibmodel(c(teta,delta),u,v,model=copulamodel)

if (pvalor>PMAX) { PMAX <- pvalor; tlmax<- teta ; dlmax <- delta }
  } }



result<-optim(c(tlmax,dlmax), method = "L-BFGS-B" ,  clibmodel,lower = c(thetamin,deltamin), upper = c(thetamax,deltamax) , control=list(fnscale=-1), x=u , y=v , model = copulamodel )

  }
}
\keyword{internal}
