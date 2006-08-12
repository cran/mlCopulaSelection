\name{dcbb4}
\alias{dcbb4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB4 copula density function  }
\description{
  Calculate the value of the BB4 density.
}
\usage{
dcbb4(theta, delta, u, v)
}

\arguments{
  \item{theta}{ Parameter \code{theta} of the BB4, (0<\code{theta}).  }
  \item{delta}{ Parameter \code{delta} of the BB4, (0<\code{delta}). }
  \item{u}{ First coordenate where de density will be evaluated. (0<\code{u}<1)}
  \item{v}{ Second coordenate where de density will be evaluated. (0<\code{v}<1)}
}

}
\value{
  value of de density BB4 for the parameters  \code{theta}  and  \code{delta} on ( \code{u} ,  \code{v} ) 
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
res<-dcbb4(1.5,1.5,0.75,0.6)


## The function is currently defined as
function(theta,delta,u,v)
{VV<-(u^(-theta)-1)^(-delta);
	WW<-(v^(-theta)-1)^(-delta);
	h<-VV+WW;
	S<-u^(-theta)+v^(-theta)-1;
	duh<-delta*theta*u^(-theta-1)*(u^(-theta)-1)^(-delta-1);
	dvh<-delta*theta*v^(-theta-1)*(v^(-theta)-1)^(-delta-1);
	duS<-(-theta)*u^(-theta-1);
	dvS<-(-theta)*v^(-theta-1);
	densi<-(1/theta)*(1/theta+1)*(S-h^(-1/delta))^(-1/theta-2)*(dvS+1/delta*h^(-1/delta-1)*dvh)*(duS+1/delta*h^(-1/delta-1)*duh)+(-1/theta)*(S-h^(-1/delta))^(-1/theta-1)*((-1/delta)*(1/delta+1)*(h)^(-1/delta-2)*dvh*duh)
	}
}
\keyword{ misc }
