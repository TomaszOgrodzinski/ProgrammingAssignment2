##RProgramming - Assignment 2

## The functions in this assignment are intended to make calculating the inverse
## of matrixes more efficinet by avoiding recalculating the inverse. They do this
## by storing the inverse in cache and retrieving it when required rather than recalculating

## Create a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #initialize variable that will store inverse
    
    #get/set functions
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse #assigns a value to i in parant function
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of matrix. If already computed retreive from cache.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    
    #If there is an inverse in cash use it
    if(!is.null(i)) {
        message("getting cached data")
        return(i) #Exits function using cached value. Further code is not executed.
    }
    
    #If no cached value calculate and set inverse value
    data <- x$get()
    i <- solve(data, ...) #calculate the inverse
    x$setInverse(i)
    i ## Return a matrix that is the inverse of 'x'
}
