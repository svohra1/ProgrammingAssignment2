## Functions create a matrix object that can store its inverse
## in its cache, and can be retrieved with a seperate function call

## This function creates a "special matrix" that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y){
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverted <<- inverse
    getinverse <- function() inverted
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)

}


## This function computes the inverse of a "special matrix" computed
## with makeCacheMatrix. If the inverse has already been computed,
## then it is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
