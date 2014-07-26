## A pair of functions, when used together will compute the inverse of a matrix and cache the value.
## Subsequent request for the inverse will return the cached value rather than re-computing.

## Create a list of functions used to cache a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {

	# initialize globalInverse variable
	globalInverse <- NULL

	# create "set" function which will set global x to its arg and globalInverse to NULL
	set <- function(y) {
        x <<- y
        
        # the matrix is being set to new instance, so clear the cached inverse
		globalInverse <<- NULL
	}

	# define "get" function which returns the matrix
	get <- function() x

	# define setInverse function which will set globalInverse to its arg
	setInverse <- function(anInverse) globalInverse <<- anInverse

	# define getInverse function which returns the cached inverse
	getInverse <- function() globalInverse

	# create the special "matrix" object
	list(set = set, get = get,
	       setInverse = setInverse,
           getInverse = getInverse)
}

## Check if the inverse of the matrix contained in the instance of x
## has been computed. If it has, return it. If not, compute the inverse,
## cache it using the x instance, and then return it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # get the existing inverse from the makeCacheMatrix instance (x)
    localInverse <- x$getInverse()

    # if it exists, then return it as it does not need to be calculated again
    if(!is.null(localInverse)) {
        message("inverse already cached, return it")
        return(localInverse)
    }

    # get the matrix data from the makeCacheMatrix object
    data <- x$get()

    # generate the inverse
    localInverse <- solve(data, ...)

    # cache the inverse
    x$setInverse(localInverse)

    # return the inverse
    localInverse
}
