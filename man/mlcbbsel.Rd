
\name{mlcbbsel}
\alias{mlcbbsel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for maximum likelihood copula selection and fitting }
\description{
  Use numerical maximum likelihood to choose and fit a bivariate copula model (from a library of 40 models) to the data. 
}
\usage{
mlcbbsel(U, V)
}

\arguments{
  \item{U}{ vector with the first coordenate of the bivariate data }
  \item{V}{ vector with the second coordenate of the bivariate data (same size as\code{U})  }
}
\details{
   (U,V) margins must have Uniform(0,1) marginal distribution 
}
\value{
  It return a LIST with,
  \item{copmax }{  the best fitting copula model. The copula models in the library correspond to BB1, BB2,...,BB10 from Joe, H., (1997) and its 90, 180 and 270 degree rotations.}
  \item{parmax }{  the maximum likelihood estimates of the parameters for the best fitting copula}
  \item{llmax }{  the log-likelihood on the estimated parameters for the best copula}
    \item{todo }{ contain a matrix with the maximum likelihood ressults for all the copula models. The first column is the copulamodel number (from 1 to 40), the second column is the maximum log-likelihood for that particular model and the third and fourth column contain the parameters values for which that maximum log-likelihood was attained.}
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hal l. }
\author{ Jesus Garcia. IMECC-UNICAMP and
Veronica Gonzalez-Lopez. IMECC-UNICAMP }

\examples{
# The Data (the margins are uniform)
U <- c( 0.43, 0.1, 0.2, 0.33, 0.24, 0.29, 0.14, 0.4, 0.39, 0.8, 0.63, 0.16, 0.24, 0.14,0.71, 0.39, 0.48, 0.29, 0.38, 0.37)
V <- c(0.01, 0.26, 0.2, 0.36, 0.34, 0.43, 0.27, 0.61, 0.08, 0.25, 0.72, 0.15, 0.14, 0.12, 0.74, 0.18, 0.58, 0.15, 0.34, 0.13)
# find the maximun likelihood estimates
res<-mlcbbsel(U,V)
#the best fitting copula model:
res$copmax
#the parameters for the best fitting copula model:
res$parmax
#the log-likelihood of the best fitting copula model with those parameters:
res$llmax



## The function is currently defined as
function(U,V)
{
	
model=c("CBB1","CBB2" , "CBB3","CBB4","CBB5","CBB6", "CBB7","CBB8", "CBB9", "CBB10","CMM1", "CMM2", "CMM3", "CMM4", "CMM5", "CMM6", "CMM7", "CMM8", "CMM9", "CMM10","CBM1", "CBM2", "CBM3", "CBM4", "CBM5", "CBM6", "CBM7", "CBM8", "CBM9", "CBM10","CMB1", "CMB2", "CMB3", "CMB4", "CMB5", "CMB6", "CMB7", "CMB8", "CMB9", "CMB10")

respmodel=c("CBB1","CBB2" , "CBB3","CBB4","CBB5","CBB6", "CBB7","CBB8", "CBB9", "CBB10",
"180 degree rotation of CBB1","180 degree rotation of CBB2" , "180 degree rotation of CBB3","180 degree rotation of CBB4","180 degree rotation of CBB5","180 degree rotation of CBB6", "180 degree rotation of CBB7","180 degree rotation of CBB8", "180 degree rotation of CBB9", "180 degree rotation of CBB10",
"90 degree rotation of CBB1","90 degree rotation of CBB2" , "90 degree rotation of CBB3","90 degree rotation of CBB4","90 degree rotation of CBB5","90 degree rotation of CBB6", "90 degree rotation of CBB7","90 degree rotation of CBB8", "90 degree rotation of CBB9", "90 degree rotation of CBB10",
"270 degree rotation of CBB1","270 degree rotation of CBB2" , "270 degree rotation of CBB3","270 degree rotation of CBB4","270 degree rotation of CBB5","270 degree rotation of CBB6", "270 degree rotation of CBB7","270 degree rotation of CBB8", "270 degree rotation of CBB9", "270 degree rotation of CBB10")



ncop<-1

TODOCOP <-c(1:40)
TODOPV  <-c(1:2)*0
TODOTET <-c(1:2)*0
TODODEL <-c(1:2)*0
PVMAX<- -10**(100)
pmax<-10

n<-sum(U != -10**200)

for(nmodel in 1:40)
{
RES   <- mlcbb(U,V,copulamodel=model[nmodel])
PV <- RES$value
PAR<- RES$par
if (PV>PVMAX) {PVMAX<-PV;PARMAX<-PAR;COPMAX<-nmodel;LLMAX<-PV}
TODOPV[nmodel]<- PV
TODOTET[nmodel] <-PAR[1]
TODODEL[nmodel] <-PAR[2]
  }


ORDEN<-order(TODOPV,TODOCOP,decreasing= TRUE)

TODO<-matrix(c(1:40*4)*0,40,4)

TODO[,1]<-respmodel[TODOCOP[ORDEN]]
TODO[,2]<-TODOPV[ORDEN]
TODO[,3]<-TODOTET[ORDEN]
TODO[,4]<-TODODEL[ORDEN]

result <- list(todo=TODO,copmax=respmodel[COPMAX],parmax=PARMAX,llmax=LLMAX)
  }
}
\keyword{ misc }
