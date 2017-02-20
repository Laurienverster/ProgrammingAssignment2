## The functions below are used to chache the (mean) inverse of a matrix

## This function creates a special vector (list containing a function to 
## set and get value of the vector and set and get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
     l <- NULL
     set <- function(y) {
          x <<- y
          l <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) l <<- inverse
     getinverse <- function () l
     list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function calculates the mean of the vector created above
## It first checks to see if it has already been calculated
## If so, it gets the mean fro the cache and skips the rest
## Else, it calculates the mean of the data and sets the value 
## of the mean to the cache

cacheSolve <- function(x, ...) {
     l <- x$getinverse()
     if (!is.null(l)) {
          message("Getting cached data")
          return(l)
     }
     data <- x$get()
     l <- solve(data, ...)
     x$setinverse(l)
     l
}