## This set of functions is designed to make repeated calculations of matrix 
## inversion computationally less expensive by caching the result and retrieving 
## it when the function is called subsequently.


## makeCacheMatrix function creates a list (an object) that stores the matrix to be 
## inverted as well as functions (methods) needed to set (save) and get (retrieve)
## the matrix itself and the result of the inversion.

makeCacheMatrix <- function(x = matrix()) { # takes a matrix as an argument
        inverse <- NULL # sets inverse to NULL when called
        set <- function(y) {
            x <<- y
            inverse <<- NULL
        }
        get <- function() x # retrieves the initial matrix
        setinverse <- function(inverseUpd) inverse <<- inverseUpd # updates the inverse
        getinverse <- function() inverse # retrieves the cached inverse
        list(set = set, get = get, # creates a list (object)
             setinverse = setinverse,
             getinverse = getinverse) 
}


## cacheSolve calculates and returns the result of the matrix inversion.
## This function uses an object produced by the makeCacheMatrix function to cache
## the result of the matrix inversion on the first call and retrieve the cached 
## value on subsequent calls. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse() # retrieves the cached inverse
        if(!is.null(inverse)) { # checks if cache has data in it
            message("getting cached data")
            return(inverse) # if cache is not empty, returns the cached inverse
        }
        data <- x$get() # retrieves the initial matrix form the list (object)
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse  
}