## Pair of functions to.
## 1. Store a matrix and cache its inverse.
## 2. Call function to compute, but try to get from cache as an optimisation.
## Assumption.  NO validation that a supplied matrix is indeed invertible.

## Sample test case
## sampleMatrix <- matrix(c(1,0,5,2,1,6,3,4,0), ncol=3)
## s <- makeCacheMatrix(sampleMatrix)
## cacheSolve(s) # Not cached
## cacheSolve(s) # Cached, you'll see message "getting cached data"


################################################################################
# makeCacheMatrix.R 
# Author.  Alnis Bajars. 2015-05-20
#
# Caches invertible matrix as a performance optimisation.
# Exploits the ability to cache objects to another environment.
################################################################################

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    # Initialise matrix object into environment.
    set <- function(y) {
        x <<- y
        inv <<- NULL # Clear cached inverse as it will have to be (re)computed
    }
    # Get input matrix
    get <- function() x
    # Store matrix inverse computed by caller
    setMatrix <- function(inverse) inv <<- inverse
    # Get matrix inverse
    getMatrix <- function() inv
    # Store functions for this object
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix) 
}

################################################################################
# cacheSolve.R
# Author.  Alnis Bajars. 2015-05-20
#
# Inverts matrix using standard solve function.
# Assumption.  No validation that the matrix is invertible, need to check yourself.
# The "secret sauce" is a (not THE) performance optimisation that checks.
#   1. That the matrix is already cached.
#   2. Has not changed since being cached.
################################################################################

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Try to retrieve cached result from environment
    m <- x$getMatrix()
    # Have we already computed and cached result?
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # Get the input matrix
    thisMatrix <- x$get()
    # The purpose of this function
    m <- solve(thisMatrix, ...)
    # Store result in cache
    x$setMatrix(m)
    # Show the result
    m
}
