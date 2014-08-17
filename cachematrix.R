# The function makeCacheMatrix creates a list which contains
# a matrix and its optionally cacheable inverse, both accessible
# through exposed functions. The function cacheSolve takes this list
# and if the inverse of the matrix in the list is not cached
# (i.e. if the value of the getinverse() function is NULL) cacheSolve
# will compute the inverse, store this in the list and return the
# computed inverse.

# Takes a matrix and returns a list which allows storing the matrix
# and its inverse together. The list returned has the following attributes:
#
# set           : store a matrix in this list,
#                 setting its cached inverse to null.
# get           : retrieve the matrix stored in this list.
# setinverse    : store a matrix - this must be the inverse
#                 of the matrix set using the "set" function.
# getinverse    : retrieve the matrix set by "setinverse".
# hasinverse    : whether the inverse is already cached.
#
# Parameters:
# x : a matrix.
#
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL

        set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x

	setinverse <- function(inverse) i <<- inverse

	getinverse <- function() i

        hasinverse <- function() !is.null(i)

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

# Computes the inverse of a matrix. Takes a list computed using
# makeCacheMatrix, returns the cached inverse or if no inverse is
# cached, computes, caches and returns the inverse of the matrix.
#
# Parameters:
# x : a list created with makeCacheMatrix.
#
cacheSolve <- function(x, ...) {
	i <- x$getinverse()

	if(!is.null(i)) {
                ## Return the cached inverse
		message("Retrieving cached inverse ...")
		i
	} else {
                ## Compute, cache and return the inverse
                message("Computing inverse ...")
		data <- x$get()
		i <- solve(data)
		x$setinverse(i)
		i
	}
}
