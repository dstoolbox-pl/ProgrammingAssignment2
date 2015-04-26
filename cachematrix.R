
## makeCacheMatrix creates a closure that will hold
## a matrix (x)
## the inverse of the matrix (inv)
## four fuctions:
## get - return the stored matrix
## set - replace the stored matrix with a new one and invalidate the cached inverse
## getInverse - return the stored inverse
## setInverse - replace the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    get <- function() x

    set <- function(y) {
    	x <<- y
    	inv <<- NULL
    }
    
    getInverse <- function() inv
    
    setInverse <- function(i) inv <<- i
    
    list (set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## the cacheSolve function expects a makeCacheMatrix defined above
## given the parameter it will check if the inverse value of the matrix is already cached
## if the inverse value is cached, it returns the cached value
## if the value is not cached (null) it retrieves the matrix, calculates the inverse  
##   and stores the inverse value

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
    	message("returning cached data")
    	return(i)
    }
    
    m <- x$get()
    i <- solve(m)
    x$setInverse(i)
    i
}
