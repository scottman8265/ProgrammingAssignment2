## These functions take a new matrix and calculates the inverse.
## If the matrix is not a new matrix, these functions will retrieve
## the inverse of the matrix from a stored variable

## makeCacheMatrix sets a series of functions to be used by cacheSolve
## to get and set cached inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	checkSame <- function(x) x == m
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get, 
	setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks to see if the inverse of matrix has already been 
## calculated.  If it has then it returns the inverse stored in memory.
## If inverse is not found in memory, it calculates the inverse,
## stores it in memory, and returns the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if (!is.null(i)) {
		message("getting cache")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
