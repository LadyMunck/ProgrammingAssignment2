## In order to reduce repetitive computation of an inversed Matrix, 
##caching is used in the following functions

## This function is to get and set the value of the matrix, as well as get and set the value 
## of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
  }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The function below computes the inverse of the matrix and sets the value in the cache. 
## It skips the computation process on any values that have already been computed

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
    }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
