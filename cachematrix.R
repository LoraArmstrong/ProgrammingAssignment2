## These functions allow the inverse of a matrix to be
## cached so that it does not have to be recalculated 
## if it has already been determined once

## makeCacheMatrix takes a normal matrix as an argument 
## and uses it to make a special 'CacheMatrix' 
## whose inverse can be cached. Assumes the matrix supplied
## is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        
        #i is the inverse of matrix x
        #initially set to NULL 
        i <- NULL
        
        # This function allows the user to change the value
        # of the matrix & reset cached inverse to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # This function gets & returns the value of the matrix
        get <- function() x
        
        # This function sets the i (the cached inverse)
        # to the argument inv
        setinv <- function(inv) i <<- inv
        
        # This function returns the value of i (the cached inverse)
        getinv <- function () i
        
        # Return list of functions that were defined
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes a CacheMatrix as an argument
## and returns the inverse of that matrix.
## If the inverse has already been calculated, the cached value
## is retrieved and returned. Otherwise the inverse is calculated,
## cached, and returned.

cacheSolve <- function(x, ...) {
        # Retrieve the cached inverse value of CacheMatrix x 
        # and store in variable i
        i <- x$getinv()
        
        # If i is NOT null, inverse has already been calculated
        # so display 'getting cached data' msg and return
        # previously calculated value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # If i is null, inverse needs to be calculated
        # Get value of the the matrix
        data <- x$get()
        
        # Calculate inverse of the matrix
        i <- solve(data, ...)
        
        # Set cached inverse to the value calculated
        x$setinv(i)
        
        # Return the inverse
        i
}
