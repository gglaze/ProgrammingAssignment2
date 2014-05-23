## These two functions invert matrices and cache the results. This potentially
## speeds computation by avoiding repeated calculation of the same matrix inversion.

# *********************************************************
# makeCacheMatrix creates and initializes a matrix object able
# to store and retrieve a matrix and its cached inverse.
# Inputs: x -- an invertible matrix (optional)
# outputs: a list containing 4 functions suitable for use by cacheSolve().
#    set: stores invertible matrix x.
#    get: returns value of previously stored invertible matrix.
#    setInverse: stores inverse of a matrix x. Does not calculate the inverse.
#    getInverse: returns value of previously stored matrix inverse.
# processes:
#    initialize matrix to optional input x.
#    reset cached inverse value to NULL
#    create list functions described above
# *********************************************************
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # initialize inverse matrix to NULL
    # function "set" initializes already created matrix object
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x  # function "get" recalls input matrix
    # function "setInverse" calculates and caches matrix inverse
    setInverse <- function(solve) inverse <<- solve
    # function "getInverse" recalls cached matrix inverse
    getInverse <- function() inverse
    # return list object containing four functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
# *********************************************************
# cacheSolve returns the inverse of an invertible matrix.
# Inputs: the matrix list object created by makeCacheMatrix().
#     list function "get" provides matrix to be inverted
#     list function "getInverse" furnishes either the cached inverse or NULL
# Outputs: stores matrix inverse using list function "setInverse."
# Returns: inverse of matrix provided by list function "get"
# Processes: 
#     If cached value not present, calculates, caches, and returns matrix inverse.
#     If cached value present, returns inverse retrieved from cache.
# *********************************************************
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse() # get cached inverse
    if(!is.null(inverse)) {   # return cached inverse if present
        message("getting cached data")
        return(inverse)
    }
    # no cached value found, so get matrix to be inverted
    data <- x$get()
    inverse <- solve(data) # invert the matrix
    x$setInverse(inverse)  # cache the inverted matrix
    inverse                # return the inverted matrix
}
