## These two functions work together to create a special matrix that performs
## an inverse function, and cacheing the result, thereby conserving processing power.
## If the inverse of the matrix has already been performed, the cached copy is returned. 
## If the inverse hasn't been performed, it performs it, caches the result, and returns it. 

## This is the function that establishes the matrix, along with the functions. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function () x
	setinverse <- function(mainverse) m <<- mainverse
	getinverse <- function () m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This is the function that performs the cacheing, and tests if it has been performed. 

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m

}
