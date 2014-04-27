## makeCacheMatrix(x) caches the inverse matrix of 'x' while cacheSolve(x) returns the inverse matrix of 'x'
## and transfers its value to makeCacheMatrix(x) to be stored

## makeCacheMatrix(x) assigns a value of the inverse of 'x' to 'inv.matrix', which is calculated by
## cacheSolve(x) function, and caches it

makeCacheMatrix <- function(x = matrix()) {
	inv.matrix <- NULL
	set <- function(y) {
		x <<- y
		inv.matrix <<- NULL
	}
	get <- function () x
	set.inverse <- function(solve) inv.matrix <<- solve
	get.inverse <- function() inv.matrix
	list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}

## cacheSolve(x) gets and returns the value of the inverse of 'x' ('inv.matrix') from makeCacheMatrix(x) function
## if it has been already cached there. Otherwise cacheSolve(x) computes, returns, and then transfers the values of 
## the inverse of 'x' to the makeCacheMatrix(x)'s cache

cacheSolve <- function(x, ...) {
      inv.matrix <- x$get.inverse()
	if(!is.null(inv.matrix)) {
		message("getting cached data")
		return(inv.matrix)
	}
	data <- x$get()
	inv.matrix <- solve(data, ...)
	x$set.inverse(inv.matrix)
	inv.matrix	
}
