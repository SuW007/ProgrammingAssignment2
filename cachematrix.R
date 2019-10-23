## makeCacheMatrix ceates a special Matrix and cach its inverse
## The second function is to compute the inverse of a square matrix

makeCacheMatrix <- function( x = matrix()) {
	i <- NULL

	#set value
	set <- function(y){
		x <<- y
		i <<- NULL
	}

        # get value
	get <- function() x
	
	# set inverse
	setinverse <- function(inverse) i <<- inverse

	# get inverse
	getinverse <- function() i

	list(set = set,
	     get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}
#solve
cacheSolve <- function(x, ...){
	i <- x$getinverse()
	if(!is.null(i)) {
		message("fetching cached data")
		return(i)
	}
 	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
	
