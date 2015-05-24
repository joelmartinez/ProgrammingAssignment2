## functions in this file let you calculate the inverse of matrices
## in a performant way. Should be used when you have many matrices to calculate

## creates a special type of matrix, with getters and setters for the underlying data, and the inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## calculates the inverse of the supplied CacheMatrix (created via `makeCacheMatrix`)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
