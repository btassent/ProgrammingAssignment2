## Calculating the inverse of a matrix
## the function "makeCacheMatrix" basically creates a matrix 
## the function "cacheSolve" calculates the inverse of the matrix created
## by the first function "makeCacheMatrix"


## This (below) is how the matrix is defined
makeCacheMatrix <- function(x = matrix()) {
        invrs=NULL
        set=function(y) {
                  x<<-y
                  invrs<<-NULL        
          }
        get<-function() x
        setinverse<-function(inverse) invrs<<-inverse
        getinverse<-function() invrs
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
     }

## This is how the inverse of the cache matrix defined above is 
## calculated. 
cacheSolve <- function(x, ...) {
       invrs<-x$getinverse()
       if(!is.null(invrs)) { ##this is for when the inverse was already calculated
                   message("getting cached data")
                   return(invrs)
          }     
       data<-x$get()
       invrs<-inverse(data, ...)
       x$setinverse(invrs)
       invrs
     }
