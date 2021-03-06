% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/coocmetric.r
\name{coocmetric}
\alias{coocmetric}
\title{Metrics for Point Process Realizations Based on Co-occurrence}
\usage{
coocmetric(S1, S2, measure = "sim", type = "count", tau = 1, M = NULL)
}
\arguments{
\item{S1}{marked point process data.}

\item{S2}{marked point process data.}

\item{measure}{\code{"sim"} for similarity and "dist" for distance. Default \code{"sim"}.}

\item{type}{if \code{"count"}, counting near-by event measure by Quian is computed. If \code{"smooth"}, smoothed counting co-occurrence measure by Hunter and Milton is computed. Default \code{"count"}.}

\item{tau}{a parameter for filtering function.}

\item{M}{a precision matrix for filter of marks, i.e., exp( - r' M r) is used for filtering marks. It should be symmetric and positive semi-definite.}
}
\value{
Similarity or distance between two inputs (marked) point process S1 and S2.
}
\description{
For comparing two SPP realizations, it is natural to count the number of events which can be considered to be co-occurring. There are two metrics for SPP realizations based on the notion of co-occurrence.
The first one proposed by Quian Quiroga et al. (2002) directly counts near-by events. The second counting metric co-occurrence is proposed by Hunter and Milton (2003), which is based on a smoothing function.
}
\details{
\code{coocmetric} computes co-occurrence base metrics for two point process realizations. This function counts the number of events in S1 which is coincided with those in S2, and vice versa.
}
\examples{
## The aftershock data of 26th July 2003 earthquake of M6.2 at the northern Miyagi-Ken Japan.
data(Miyagi20030626)
## time longitude latitude depth magnitude
## split events by 7-hour
sMiyagi <- splitMPP(Miyagi20030626,h=60*60*7,scaleMarks=TRUE)$S
N <- 10
sMat <- matrix(0,N,N)
tau<-0.2
  cat("calculating coocmetric(smooth)...")
 for(i in 1:(N)){
   cat(i," ")
   for(j in i:N){
     S1 <- sMiyagi[[i]]$time;S2 <- sMiyagi[[j]]$time
    sMat[i,j] <- coocmetric(S1,S2,type="smooth",tau=tau,M=diag(1,4))
   }
 }
 sMat <- sMat+t(sMat)
 tmpd <- diag(sMat) <- diag(sMat)/2
 sMat <- sMat/sqrt(outer(tmpd,tmpd))
image(sMat)
}
\author{
Hideitsu Hino \email{hinohide@cs.tsukuba.ac.jp}, Ken Takano, Yuki Yoshikawa, and Noboru Murata
}
\references{
R. Quian Quiroga, T. Kreuz, and P. Grassberger. Event synchronization: a simple and fast method to measure synchronicity and time delay patterns, Physical Review E, Vol. 66(4), 041904, 2002.

J. D. Hunter and G. Milton. Amplitude and frequency dependence of spike timing: implications for dynamic regulation, Journal of Neurophysiology, Vol. 90, pp. 387-94, 2003.
}

