## The following functions are used to cached the inverse of a given matrix, in order to avoid 
## unnecesary computations of that value

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        # If the matrix changes, we must reset the cached content
        inverse <<- NULL
    }
    get <- function()  {
        x
    }
    setinverse <- function(inv) {
        inverse <<- inv
    }
    getinverse <- function() {
        inverse
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve returns the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
