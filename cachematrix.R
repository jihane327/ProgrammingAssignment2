## Handy functions to compute the inverse of a matrix X. 
## The inverse is cached in order to avoid costly and repeated computations.


## Creates a special list that stores the matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL
	
	get <- function() x
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse
	
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Returns the cached inverse of the matrix if already computed.
## Otherwise, computes the inverse and caches it
cacheSolve <- function(x, ...) {

        inverse <-x$getInverse()
        if(!is.null(inverse)){
        	message("cached inverse of the matrix")
        	return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat,...)
        x$setInverse(inverse)
        inverse
}
