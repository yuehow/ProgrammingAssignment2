
## Yueh-O Wang
## Assignment 2
## 
## Two functions:
## makeCacheMatrix : create a special matrix object which can cache its inverse
## cacheSolve : compute a inverse of the matrix returned by makeCacheMatrix
##

##
## create and initalize an caching inverse of matrix object
## 1)initialize inverse matrix to NULL
## 2)defined  set, get matrix function and setInv, getInv for inverse matrix
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

##
## When cacheSolve is called check if the inverse matrix is already cached.
## If cached inverse matrix is exist, return it, otherwise calculate the inverse matrix
## and cached the inverse matrix in the object, then return the inverse matrix
##
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