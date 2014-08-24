## the makecachematrix() function creates and stores the inverse of a matrix. 
## the cachesolve() function returns the inverse of a matrix either 
## from cache or by calculating. These functions assume that x is invertible.

## makeCacheMatrix() function takes a matrix as argument and can do a bunch of 
## set and get operations on the matrix x and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() {x}
	set_inv <- function(x_inv) {inverse <<- x_inv}
	get_inv <- function() {inverse}
	list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## cacheSolve() function returns the inverse of a matrix x either from cache 
## or by calculating if the current value of inverse is NULL 

cacheSolve <- function(a) {
	inverse <- a$get_inv()
	if(!is.null(inverse)){
	message("getting cached value")
	return(inverse)
	}        
	data <- a$get()
	inverse <- solve(data)
	a$set_inv(inverse)
	inverse		
}
