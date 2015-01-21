## makeCacheMatrix and solveMatrix represent high performance matrix inverse function 
## which makes use of caching.


## Creates a matrix object including a matrix and its inverse if already computed

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## Calculates the matrix inverse if i is NULL. i will be reset to NULL whenever $set is called.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    xc <- x$get()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

