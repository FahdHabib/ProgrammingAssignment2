## Put comments here that give an overall description of what your
## functions do
'''
The function makeCacheMatrix takes input in the form of a matrix (called x). 
It has four functions, get (to return the matrix), set (to assign value to the matrix), getinverse (to return the matrix inverse) 
and setinverse (to calculate the inverse of the matrix and save it).
'''
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

'''
The function cacheSolve basically returns the inverse of the matrix x. 
First, it checks whether an inverse has already been calculated. In that case, the cached inverse is returned else the inverse is calculated.
'''
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
