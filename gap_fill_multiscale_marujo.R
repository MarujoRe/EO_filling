#############################################################
## Gap-fill raster images through Marujo
## rennanmarujo@gmail.com - Mar 2019
#############################################################

##############################
# 0 - Load librairies
##############################
library(raster)
library(foreach)
library(doParallel)

#############################################################
fillMarujo <- function(imgGap,imgRef,imgSegMask1,imgSegMask2,imgSegMask3){
    cat("Filling...")
    # Get all image value as a vector
    Gappixels <- values(imgGap)
    Refpixels <- values(imgRef)
    seg1pixels <- values(imgSegMask1)
    seg2pixels <- values(imgSegMask2)
    seg3pixels <- values(imgSegMask3)
    
    segNum <- unique(seg1pixels)
    
    # Label the vector with the segment number
    names(Gappixels) <- values(imgSegMask1)
    names(Refpixels) <- values(imgSegMask1)
    
    # Get the value as a vector, test if the value is NA
    GapNApixels <- is.na(Gappixels)
    # Selects only pixels with NA
    NA_which <- which(GapNApixels)
    # Get the segments that contain those NA pixels
    segsWithNA <- vector()
    segsWithNA <- as.numeric(unique(names(NA_which)))
    Segmentationpixels <- which(names(Gappixels) %in% segsWithNA)
    
    # Selects ALL pixels that will be used to perform the filling process
    GapElements <- imgGap[Segmentationpixels]
    names(GapElements) <- imgSegMask1[Segmentationpixels]
    # Store the elements in a list of segments
    SegmentListGap  <- list()
    SegmentListGap  <- split(GapElements, f = names(GapElements))
    
    
    #Select pixels that will be used to perform the filling process
    RefElements <- Refpixels[Segmentationpixels]
    #names(RefElements) <- imgSeg[Segmentationpixels]
    SegmentListRef  <- list()
    SegmentListRef  <- split(RefElements, f = names(RefElements))
    SegmentListpixel <- list()
    SegmentListpixel <- split(Segmentationpixels, f = names(RefElements))
    #print(paste("Level ",flagSeg, " i:", i, " band:", n-3, sep=''))
    
    listIndexpixelOrderNA <- which(is.na(unlist(SegmentListGap)))
    
    SegmentListGap_filled <- list()
    SegmentListGap_filled <- foreach (s=1:length(segsWithNA), .inorder=F) %do% {
        if (any( is.na(SegmentListGap[[s]]) ) ){ # Check if there is a NA value
            if(!all( is.na(SegmentListGap[[s]]) ) ){ # not all are NA
                RefsegmentMean = mean(SegmentListRef[[s]], na.rm = T)
                GapsegmentMean = mean(SegmentListGap[[s]], na.rm = T)
                RefsegmentDeviationProp = SegmentListRef[[s]] / RefsegmentMean
                indexNA = which(is.na(SegmentListGap[[s]]) )
                
                SegmentListGap[[s]][indexNA] = GapsegmentMean * RefsegmentDeviationProp[indexNA]
                return(SegmentListGap[[s]])
            }
            else{ #all are NA #use level 2
                seg2 <- seg2pixels[SegmentListpixel[[s]][1]][1]
                seg2Indexes <- which(imgSegMask2[] == seg2)
                valuesSeg2 <- Gappixels[seg2Indexes]
                if(!all( is.na(valuesSeg2) ) ){ # not all are NA
                    RefsegmentMean = mean(Refpixels[seg2Indexes], na.rm = T)
                    GapsegmentMean = mean(Gappixels[seg2Indexes], na.rm = T)
                    RefsegmentDeviationProp = SegmentListRef[[s]] / RefsegmentMean
                    indexNA = which(is.na(SegmentListGap[[s]]) )
                    
                    SegmentListGap[[s]][indexNA] = GapsegmentMean * RefsegmentDeviationProp[indexNA]
                    return(SegmentListGap[[s]])
                }
                else{
                    seg3 <- seg3pixels[SegmentListpixel[[s]][1]][1]
                    seg3Indexes <- which(imgSegMask3[] == seg3)
                    valuesSeg3 <- Gappixels[seg3Indexes]
                    if(!all( is.na(valuesSeg3) ) ){ # not all are NA
                        RefsegmentMean = mean(Refpixels[seg3Indexes], na.rm = T)
                        GapsegmentMean = mean(Gappixels[seg3Indexes], na.rm = T)
                        RefsegmentDeviationProp = SegmentListRef[[s]] / RefsegmentMean
                        indexNA = which(is.na(SegmentListGap[[s]]) )
                        
                        SegmentListGap[[s]][indexNA] = GapsegmentMean * RefsegmentDeviationProp[indexNA]
                        return(SegmentListGap[[s]])
                    }
                }
                
            }
        } 
    }
    pixelOrder <- unlist(SegmentListpixel)
    pixelOrderNA <- pixelOrder[listIndexpixelOrderNA]
    imgGap[pixelOrder] = unlist(SegmentListGap_filled) 
    
    return(imgGap)
}

#############################################################
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
registerDoParallel(cl)

start.time <- Sys.time()

### DEMO Data ###
imgSegMask1 <- raster(matrix(c(10,10,10,10,10,20,20,10,10,20,20,10,10,10,10,30),4,4)) # scale 10
imgSegMask2 <- raster(matrix(c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,30),4,4)) # scale 15
imgSegMask3 <- raster(matrix(c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10),4,4)) # scale 20
imgGap      <- raster(matrix(c(1,2,3,4,2,20,22,2,NA,NA,NA,NA,NA,NA,NA,NA),4,4))
imgRef      <- raster(matrix(c(10,20,30,40,20,20,22,20,30,40,50,60,70,80,90,00),4,4))
#############################################################

filledimg <- fillMarujo(imgGap,imgRef,imgSegMask1,imgSegMask2,imgSegMask3)

stopCluster(cl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken





