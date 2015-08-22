## makeCacheMatrix accept a Maxtrix as argument and provide the following functions:
## set() - store the matrix
## get() - retrieve the matrix
## setInverse() - store the inverse matrix provided. Note this function does not calculate the inverse of the matrix
## getInverser() - get the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    im <- NULL
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) im <<- inv
    getInverse <- function() im
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)  

}


## cacheSolve accept an object that is of type makeCacheMatrix and provide the following functions:
## check if the inverse matrix already calculated and stored in cache by using the getIverse() function
## of makeCachMatrix.
## If it does not exist, calculate the inverse matrix and store in cached using setInverse() function
## of makeCacheMatrixmt

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  im
}
