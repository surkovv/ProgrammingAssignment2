
## Makes CacheMatrix from matrix m
## Cached matrix is a list of functions:
## get - returns stored matrix data
## set - sets data to m
## getInverse - returns variable inverse
## setInverse - sets inverse to m

makeCacheMatrix <- function(m = matrix()) {
    data <- m
    inverse <- NULL
    
    get <- function() data
    
    set <- function(m) {
        data <<- m
        inverse <<- NULL
    }
    
    getInverse <- function() inverse
    
    setInverse <- function(inv) inverse <<- inv 
    
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## Accepts CacheMatrix x
## returns CacheMatrix y, y is inversed x
## if x doesn't change, function returns cached matrix
## x must be numeric square matrix

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        return(makeCacheMatrix(inv))
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    makeCacheMatrix(inv)
}
