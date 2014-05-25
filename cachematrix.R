## This code is used to calculate the inverse of a matrix and aims to speed up
## computation by caching the result. If the inverse is solved twice on the 
## same 'special' matrix, then the cached result is returned instead of 
## calculating the result again.

## Write a short comment describing this function
## This function returns a 'special' matrix which is actually a list, containing
## a function to set the value of the matrix, get the value of the matrix, 
## set the inverse of the matrix and get the inverse of the matrix. Use get,
## set, getinverse and setinverse, respectively, to acces each of the metioned
## functions in the list.
## This assumes that x is a square matrix.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This method will return the inverse of the given 'special' matrix. It will
## cache the result so subsequent calls will be returned quickly.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
