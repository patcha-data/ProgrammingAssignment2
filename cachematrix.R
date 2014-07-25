## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below Pair of functions helps to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	
	set <- function(y) {

		x <<- y

		inv <<- NULL

		}

	get <- function() x

	# The <<- operator sets value to function's defind environment.
	# And so Below single line function will set inv variable of makeCacheMatrix. 
	# inv was null before. inv will be null till some one calls below setinv function.
	# Look at cachesolve, it is calling setinv function

	setinv <- function(invr) inv <<- invr

	getinv <- function() inv

	# return list which is input to cachesolve
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
	#x must be a list which has functions getinv,get and setinv. 
	#Below code tryies to access these functions with the help of '$'.

	inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return (inv)
		}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	
        ## Return a matrix that is the inverse of 'x'
	inv
}
