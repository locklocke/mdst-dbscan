########################################################################
# implementation of ST-DBSCAN for multidimensional data                #
# Changlock Choi                                                       #
# 2021-05-25                                                           #
########################################################################

########################################################################
# INPUTS :                                                             #
# x     = x-axis coordinate of point data                              #                                  
# y     = y-axis coordinate of point data                              #
# time  = time coordinate of point data                                #
# value = additional attributes of point data for clustering           #
# eps = distance maximum for longitude and latitude                    #
# eps2 =  distance maximum for date                                    #
# eps3 = distance maximum for value                                    #
# minpts = number of points to consider a cluster                      #
########################################################################

mdstdbscan <- function (x, 
                        y, 
                        time,
                        value, 
                        eps, 
                        eps2, 
                        eps3, 
                        minpts) { 

  
  distdata <- cbind.data.frame(x, y)
  time <- time
  value <- value
  
  n <- nrow(distdata)
  
  classn <- cv <- integer(n)
  isseed <- logical(n)
  cn <- integer(1)
  
  for (i in 1:n) {
    unclass <- (1:n)[cv < 1]
    
    ##making distance
    a <- data.frame(x = distdata[i, 1], y = distdata[i, 2])
    fordist <- cbind.data.frame(a, distdata)
    idist <- abs(sqrt((fordist[,1] - fordist[,3])^2 + (fordist[, 2] - 
                                                         fordist[, 4])^2))
    forvaluedist <- cbind.data.frame(value[i], value)
    ivaluedist <- abs(forvaluedist[, 1] - forvaluedist[, 2])
    fortime <- cbind.data.frame(time[i], time)
    itimedist <- abs(fortime[, 1] - fortime[, 2])
    
    if (cv[i] == 0) {
      
      reachables <- intersect(unclass[idist[unclass] <= eps],  
                              unclass[itimedist[unclass] <= eps2])
      reachables <- intersect(reachables, unclass[ivaluedist[unclass] <= eps3])
      if (length(reachables) + classn[i] < minpts)
        cv[i] <- (-1)                    
      else {
        cn <- cn + 1                   
        cv[i] <- cn
        isseed[i] <- TRUE
        reachables <- setdiff(reachables, i)
        unclass <- setdiff(unclass, i)       
        classn[reachables] <- classn[reachables] + 1
        while (length(reachables)) {
          cv[reachables] <- cn           
          ap <- reachables                           
          reachables <- integer()
          
          for (i2 in seq(along = ap)) {
            j <- ap[i2]
            
            ##make distance again when cluster is expanding
            b <- data.frame(x = distdata[j, 1], y = distdata[j, 2])
            jfordist <- cbind.data.frame(b, distdata)
            jdist <- sqrt((jfordist[,1] - jfordist[,3])^2 + 
                            (jfordist[, 2] - jfordist[, 4])^2)
            jforvaluedist <- cbind.data.frame(value[j], value)
            jvaluedist <- abs(jforvaluedist[, 1] - jforvaluedist[, 2])
            jfortime <- cbind.data.frame(time[j], time)
            jtimedist <- abs(jfortime[, 1] - jfortime[, 2])
            
            jreachables <- intersect(unclass[jdist[unclass] <= eps],  
                                     unclass[jtimedist[unclass] <= eps2])
            jreachables <- intersect(jreachables, unclass[jvaluedist[unclass]
                                                          <= eps3])
            
            if (length(jreachables) + classn[j] >= minpts) {
              isseed[j] <- TRUE
              cv[jreachables[cv[jreachables] < 0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables] == 0])
            }
            classn[jreachables] <- classn[jreachables] + 1
            unclass <- setdiff(unclass, j)
          }
        }
      }
    }
    if (!length(unclass))
      break
  }
  
  if (any(cv == (-1))) {
    cv[cv == (-1)] <- 0
  }
  result <- list(cluster = cv, eps = eps, 
              eps2 = eps2, eps3 = eps3,
              minpts = minpts, density = classn)
  rm(classn)

  class(result) <- "mdst-dbscan"
  return(result)
}
