# This function takes a matrix as a parameter, and provides a
# modular interface (by way of the functions defined within it) 
# to store and retrieve a cache of the matrix inverse.
#
# The inverse matrix calculation is not performed by this function,
# that is done by the cacheSove() function below, which takes as an
# argument a variable created from the execution of this function.

makeCacheMatrix <- function(x = matrix()) {
	
	# Verify that the argument x is both a matrix, and symetrical
	if (!isSymmetric.matrix(x)) stop("x is not a symmetric matrix")
	
	# The cached inverse of matrix x. Initialised to null, 
	# which implies that the cached inverse has not been set.
	i <- NULL
	
	# The setter for the matrix, used to change the matrix that we
	# are caching the inverse of. Only invalidate the cached inverse 
	# if the new matrix is actually different from the original.
	set <- function(y) {
		if (!identical(x, y)) {
			x <<- y
			i <<- NULL
		}
	}
	
	# The getter for the matrix.
	get <- function() x
	
	# The setter for the cached inverse
	setinv <- function(inv) i <<- inv
	
	# The getter for the cached inverse
	getinv <- function() i
	
	# The return value from this function, a list containing the
	# 4 functions which define the interface
	list(set 	= set, 
		 get 	= get,
		 setinv	= setinv,
		 getinv	= getinv)
}

# This function returns the inverse of a matrix enclosed 
# in the parameter x. The parameter x is expected to have been
# created by the the makeCacheMatrix() function.
#
# If a cached inverse is found in x, that will be returned.
# Otherwise the matrix inverse will be calculated, saved to x,
# and then returned.

cacheSolve <- function(x, ...) {
	
	# Validate that the parameter x is a list with the expected
	# named elements. Probably not required, the function will 
	# halt if the calls to the functions in x fail.
	
	if (!is.list(x)) stop("x is not a list")
	
	expectedNames <- c("get", "getinv", "set", "setinv")
	if (!identical(sort(attributes(x)$names), expectedNames)) {
		stop("x does not contain expected interface")
	}

	# Try to retrieve the cached inverse
	i <- x$getinv()
	if(!is.null(i)) {
		# Cache exists, so return it.
		message("getting cached data")
		return(i)
	}
	
	# Calculate the inverse, save to a local variable
	i <- solve(x$get(), ...)
	
	# Write the calculated value back to the cache
	x$setinv(i)
	
	# Return the calculated value
	i
}
