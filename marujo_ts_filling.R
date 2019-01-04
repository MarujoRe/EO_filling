library(raster)
library(foreach)
library(doParallel)
library(zoo)

start.time <- Sys.time()

calcMeanTS <- function (TSlist){
  useindex <- lapply(TSlist,is.na)
  meanTS <- rep(0, length=length(useindex[[1]]) )
  countVec <- rep(0, length=length(useindex[[1]]) )
  for(a in 1:length(TSlist)){
    pos <- which(useindex[[a]] == F)
    countVec[pos] <- countVec[pos] +1
    meanTS[pos] <- meanTS[pos] + TSlist[[a]][pos]
  }
  meanTS[meanTS==0] <- NA
  meanTS <- meanTS/countVec
  meanTS <- na.spline(meanTS)
}

#For each segment
for(i in 1:numSegs){
  segmentTS <- list()
  numTS <- length(SegmentList[[i]])
  #For each TimeSeries in segment
  #Extract all time series
  for(k in 1:numTS){
    segmentTS[[k]] <- as.numeric(extract(myStack, 
                            SegmentList[[i]][k]))
  }
  # cat(paste("FILL "))
  #Check if any element is NA in the segment
  if( any( unlist( lapply(segmentTS, is.na) )) ){
    TS_mean <- calcMeanTS(segmentTS)
    #which TS contain NA but are not all values
    TS_with_NA <- intersect( which( sapply( 
               lapply(segmentTS, is.na), any )),
                        which(!sapply( lapply(
                        segmentTS,is.na),all)))
                            
    to_be_filled_pos <- vector("list",numImgs)
    to_be_filled_value <- vector("list",numImgs)
    for(k in TS_with_NA){
      NAindex <- which(is.na(segmentTS[[k]]))
      for( img in NAindex){
        to_be_filled_pos[[img]] <- append(
                        to_be_filled_pos[[img]],
                        SegmentList[[i]][k])
        to_be_filled_value[[img]] <- append(
                        to_be_filled_value[[img]],
                        TS_mean[img])
      }
    }

    for( img in 1:numImgs){
      if(!is.null(to_be_filled_pos[[img]])){
                to_be_filled_pos_list[[img]] <- 
                    append(to_be_filled_pos_list[[img]],
                           to_be_filled_pos[[img]])
                to_be_filled_value_list[[img]] <- 
                    append(to_be_filled_value_list[[img]],
                           to_be_filled_value[[img]])
      }
    }
    rm(to_be_filled_pos,to_be_filled_value)
  }
}

stopCluster(cl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

