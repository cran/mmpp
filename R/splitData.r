#' Split MPP Data by a Fixed-length Window
#'
#' This function splits point process instance into a list of splitted point process instances with length \code{h}.
#'
#' \code{splitMPP} splits point process instance into a list of splitted point process instances with length \code{h}.
#'
#'
#' @param mppdata marked point process in data.frame composed of "time, mark1, mark2, ..."
#' @param h width of the time window. Default is set to h=60*60*48, which is two days when $time is recorded in second. This is suitable for a special seismic data only.
#' @param Origin Logical. If \code{TRUE}, the beginning of the window is assumed to be the origin of time. Default \code{TRUE}
#' @param scaleMarks Logical. If \code{TRUE}, marks (except time) are normalized to have unit variance in whole time series (not in individual windows). Default \code{FALSE}.
#' @param scaleWindow Logical. If \code{TRUE}, time interval (window.length) is normalized to one.
#' @param original.tic vector of original time stamp for the input mppdata. If given, this function outputs a list of splitted MPP and a list of time stamps for elements of the splited mpp list.
#' @export
#' @examples
#' ##The aftershock data of 26th July 2003 earthquake of M6.2 at the northern Miyagi-Ken Japan.
#' data(Miyagi20030626)
#' ##  no. longitude latitude magnitude     time  depth year month day
#' ## split events by 5-day
#' sMiyagi <- splitMPP(Miyagi20030626,h=60*60*5,scaleMarks=TRUE,original.tic=Miyagi20030626[,1])
splitMPP <- function(mppdata,h=60*60*48,Origin=TRUE,scaleMarks=FALSE,scaleWindow=TRUE,original.tic=NULL){
  if(scaleMarks){
    X <- scale(mppdata[,-1])
    mppMeans <- attr(X,"scaled:center")
    mppSDs <- attr(X,"scaled:scale")
    mppdata[,-1] <- X;rm(X)
  }else{
    mppMeans <- NULL;mppSDs <- NULL
  }
  time.seq <- mppdata$time

  ## the dimension of marks
  markdim <- dim(mppdata)[2]-1

  ## mark names
  marknames <- paste("mark",seq(1,markdim),sep="")
  
  tics <- seq(time.seq[1],time.seq[length(time.seq)],by=h)
  S <- list()
  noEventID <- NULL
  if(!is.null(original.tic)){ orig.tic.list <- list()}
  for(i in 1:(length(tics)-1)){
    
    id <- which(((time.seq > tics[i])*(time.seq < tics[i+1]))==1)
    S[[length(S)+1]] <- mppdata[id,]
    if(!is.null(original.tic)){ orig.tic.list[[length(orig.tic.list)+1]] <- original.tic[id]}

    ## when there is no event in the window, set the event occuring time to "-1" and marks to "0"
    ## treatment of these windows is left for the user
    if(length(S[[length(S)]][,1])==0){
      noEventID <- c(noEventID,i)
      S[[length(S)]] <- c(-1,rep(0,markdim))
      names(S[[length(S)]]) <- c("time",marknames); S[[length(S)]] <- data.frame(t(S[[length(S)]]))
    }else if(Origin){   ## if Origin is TRUE, the event time is recorded from the begging of the window
      S[[length(S)]]$time <- S[[length(S)]]$time-tics[i]
    }

    if(scaleWindow){
      S[[length(S)]]$time <- S[[length(S)]]$time/h
    }
  }
  if(!is.null(original.tic)){
    return(list(S=S, mppMeans=mppMeans, mppSDs=mppSDs,noEventID=noEventID,original.tic.list=orig.tic.list))
  }else{
    return(list(S=S, mppMeans=mppMeans, mppSDs=mppSDs,noEventID=noEventID))
  }
}
