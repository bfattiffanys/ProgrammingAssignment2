## 'makeCacheMatrix' function takes a matrix 'x', gets its inverse and stores it.'cacheSolve' function retrieves original variable and its inverse if available, and if not stored, then it computes and stores inverse, and returns it.

## 'makeCacheMatrix' takes a matrix 'x', gets its inverse, stores it, and provides directions for 'cacheSolve' function when that function is called. 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) inverse <<- solve
	getmatrix <- function() inverse
	list(set = set, get = get, 
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}


## 'cacheSolve' takes 'x' from 'makeCacheMatrix' and checks to see if its inverse is stored/cached, providing message "getting cached data"; if inverse is found, then function returns the inverse. if '!is.null = NULL', then 'cacheSolve' computes the inverse of matrix 'x', stores it, as well as returns the inverse.

cacheSolve <- function(x, ...) {
	inverse <- x$getmatrix()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	matrix <- x$get()
	inverse <- solve(matrix, ...)
	x$setmatrix(inverse)
	inverse
}
