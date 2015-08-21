## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {     # Receives a mattrix object
    i <- NULL                                   # Set an inverse variable
    set <- function(y) {                        
        x <<- y                                 # Saves mattrix in cache
        i <<- NULL                              # Set inverse as NULL in cache
    }
    get <- function() x                         # Retrieves mattrix
    setinverse <- function(inv) i <<- inv       # Saves inverse in cache
    getinverse <- function() i                  # Retrieves inverse mattrix
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## To know if the matrix have change we compare the matrix received as
    ##   parameter with the matrix stored in Global Environment.
    i <- x$getinverse()                 # Get inverse matrix
    if(!is.null(i)) {                   # If inverse already exists
        message("getting cached data")  # then return saved value
        return(i)
    }
    data <- x$get()                         # If it doesn't exist
    i <- solve(data, ...)                   # then calculate it
    x$setinverse(i)                         # and save it in cache
    i
}
