
## Yueh-O Wang
## Assignment 2
## 
## Two functions:
## makeCacheMatrix : create a special matrix object which can cache its inverse
## cacheSolve : compute a inverse of the matrix returned by makeCacheMatrix
##

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function (y) {
				x <<- y
				inv <<- NULL
		}
		get <- function () x
		setInv <- function(inverse) {
				inv <<- inverse
		}
		getInv <- function () inv
		list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...) {
		inv <- x$getInv()
		if (!is.null(inv)) {
				message("getting cached data")
				return(inv)
		} 
		dt <- x$get()
		inv <- solve(dt, ...)	
		x$setInv(inv)
		inv
}