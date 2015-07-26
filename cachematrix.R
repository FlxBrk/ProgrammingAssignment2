### Function to create a special matrix.
### Takes a matrix as argument.
### Will return a list of functions to operate with it.
makeCacheMatrix <- function(x = matrix()) {
    
    # Variable to save/cache the inverse
    inv <- NULL
    
    ## Function to SET the matrix
    set <- function(y) {
        # Set the vector
        x <<- y
        
        # Initialize the cache-variable
        # (no inverse was calculated yet)
        inv <<- NULL
    }
    
    ## Function to GET the matrix
    get <- function(){
        # Return the matrix
        x
    }
    
    ## Function to SET the inverse of the matrix
    setinv <- function(solve) {
        # Note, that "solve" can only create an inverse of a SQUARE matrix
        
	# To calculate the inverse of a non quadratic matrix
        # "ginv" would be the function to use.
        # But that would require the MASS package to be loaded first,
        # which was not specified in the task...
        
        # Store the inverse in variable "inv" (parent directory)
        inv <<- solve
    }
    
    ## Function to GET the inverse of the matrix
    getinv <- function() {
        # Return the inverse
        inv
    }
    
    # Return a list of the setter and getter functions
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

### Function to cache the inverse of a special matrix.
### Takes a makeCacheMatrix-Construct as argument.
### Will cache and return the inverse of the special matrix argument.
cacheSolve <- function(x, ...) {
    
    # Grab the inverse of the matrix
    inv <- x$getinv()
    
    ## Has the inverse already been calculated...?
    if( !is.null(inv) ) {
        message("getting cached data")
        # ... if so, return it
        return(inv)
    }
    
    ## ...If not, do it now
    # Save/get the matrix to do calculations on it
    data <- x$get()
    
    # Calculate the inverse and store it
    # (Once again "solve" is used)
    inv <- solve( data, ...)
    
    # Set the calculated inverse to the matrix
    x$setinv(inv)
    
    # Return the inverse
    inv
}