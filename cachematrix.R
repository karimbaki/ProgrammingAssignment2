## Matrix inversion is usually a costly computation and there may be some benefit to caching 
##the inverse of a matrix rather than compute it repeatedly.
## The two functions below cache the inverse of a matrix

## makeCacheMatrix incorporates a function to:

    ## 1. set the value of the matrix
    ## 2. get the value of the matrix
    ## 3. set the value of the inverse of the matrix
    ## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The below function uses "solve" to return the inverse of the matrix. 
## It checks to see if the inverse has already been calculated.  
## If it hasn't it will compute and return the inverse, and if it has, it will return the already calculated result. 

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data.")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
}


