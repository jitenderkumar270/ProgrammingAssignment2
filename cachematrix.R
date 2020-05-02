## As calculation of matrix inverse is a costly operation, hence, it is better to cache ther inverses and use them latter in time when requires.

## This function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	matrixinverse <- NULL                     
	set <- function(y) {                      
		x <<- y
		matrixinverse <<- NULL              
	}

	get <- function() x                           
	setinverse <- function(solve) matrixinverse <<- solve 
	getinverse <- function() matrixinverse        
	list(set = set, get = get,                    
		setinverse = setinverse,
		getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then 
#the cachesolve should retrieve the inverse from the cache.
cacheSolve<- function(x, ...) {                 
 	matrixinverse <- x$getinverse()
	if(!is.null(matrixinverse)) {                 
		message("getting cached data - Inverse of the matrix")
		return(matrixinverse)
	}
	data <- x$get()                               
	matrixinverse <- solve(data, ...)
	x$setinverse(matrixinverse)
	matrixinverse
}