\name{dcbb6}
\alias{dcbb6}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB6 copula density function  }
\description{
  Calculate the value of the BB6 density.
}
\usage{
dcbb6(t, d, u, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{ Parameter \code{t} of the BB6, (1<\code{t}).  }
  \item{d}{ Parameter \code{d} of the BB6, (1<\code{d}). }
  \item{u}{ First coordenate where de density will be evaluated. (0<\code{u}<1)}
  \item{v}{ Second coordenate where de density will be evaluated. (0<\code{v}<1)}
}

}
\value{
  value of de density BB6 for the parameters  \code{t}  and  \code{d} on ( \code{u} ,  \code{v} ) 
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
res<-dcbb6(1.5,1.5,0.75,0.6)



## The function is currently defined as
function(t,d,u,v)
{
	
  t19 = (d*t*(-log(-(-u+1.)**t+1.))**(d-1.)*(-log(-(-v+1.)**t+1.))**(d-1.)*exp(-(-log(-(-u+1.)**t+1.))**d-(-log(-(-v+1.)**t+1.))**d)*exp(-(-log(-(-u+1.)**t+1.))**d-(-log(-(-v+1.)**t+1.))**d)**(1./d-1.)*(-u+1.)**(t-1.)*(-v+1.)**(t-1.)*(-exp(-(-log(-(-u+1.)**t+1.))**d-(-log(-(-v+1.)**t+1.))**d)**(1./d)+1.)**(1./t-1.))/(((-u+1.)**t-1.)*((-v+1.)**t-1.))-(t*(-log(-(-u+1.)**t+1.))**(d-1.)*(-log(-(-v+1.)**t+1.))**(d-1.)*exp(-(-log(-(-u+1.)**t+1.))**d-(-log(-(-v+1.)**t+1.))**d)**2*exp(-(-log(-(-u+1.)**t+1.))**d-(-log(-(-v+1.)**t+1.))**d)**(2./d-2.)*(-u+1.)**(t-1.)*(-v+1.)**(t-1.)*(1./t-1.)*(-exp(-(-log(-(-u+1.)**t+1.))**d-(-log(-(-v+1.)**t+1.))**d)**(1./d)+1.)**(1./t-2.))/(((-u+1.)**t-1.)*((-v+1.)**t-1.))+(d*t*(-log(-(-u+1.)**t+1.))**(d-1.)*(-log(-(-v+1.)**t+1.))**(d-1.)*exp(-(-log(-(-u+1.)**t+1.))**d-(-log(-(-v+1.)**t+1.))**d)**2*exp(-(-log(-(-u+1.)**t+1.))**d-(-log(-(-v+1.)**t+1.))**d)**(1./d-2.)*(-u+1.)**(t-1.)*(-v+1.)**(t-1.)*(1./d-1.)*(-exp(-(-log(-(-u+1.)**t+1.))**d-(-log(-(-v+1.)**t+1.))**d)**(1./d)+1.)**(1./t-1.))/(((-u+1.)**t-1.)*((-v+1.)**t-1.))
  }
}
\keyword{misc}
