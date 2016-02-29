## @param x
##	Must be an invertible square matrix (# of rows = # of columns).
##
## @return
##	A special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	# Functions for getting and setting base data matrix.
	get <- function() x
	set <- function(y) {
		x <<- y
		# Since base data has changed, inverse has to be
		# recomputed, so set it to NULL.
		inverse <<- NULL
	}

	# Functions for getting and setting the inverse of the base data.
	getinverse <- function() inverse
	setinverse <- function(inv) inverse <<- inv

	# A list containing functions to get/set the base matrix data set
	# and to cache that data set's matrix-inverse.
	list(get = get, set = set,
	     getinverse = getinverse,
	     setinverse = setinverse)
}


## @param x
##	Special "matrix" object that caches its inverse; created by the
##	"makeCacheMatrix" function.
##
## @return
##	A matrix that is the inverse of "x".
cacheSolve <- function(x, ...) {
	# If "inverse" is not NULL, then return the cached inverse matrix.
	inverse <- x$getinverse()
	if (!is.null(inverse)) {
		message("Getting cached data...")
		return(inverse)
	}

	# Otherwise, if "inverse" is NULL, then we haven't cached in x yet.
	# So, compute the inverse of x's base data matrix, cache it in x,
	# and then return it.
	inverse <- solve(x$get())
	x$setinverse(inverse)
	inverse
}
