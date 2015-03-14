## Functions to manage matrices and cached inversions.


## Create a special "matrix" object that can cache its inverse.
##
## data - A matrix type, defaults to an empty matrix.
##
## Returns an object which can be passed to `cacheSolve` to obtain the inverse
## of the stored matrix.

makeCacheMatrix <- function (data = matrix()) {
    # Used to store the matrix inverse, once calculated:
    stored_inverse <- NULL
    
    # Set a new value for the matrix and invalidate `stored_inverse`:
    set <- function (new_matrix) {
        data <<- new_matrix
        stored_inverse <<- NULL
    }
    
    # Get the stored matrix:
    get <- function () data
    
    # Store the inverse of `data`:
    setinverse <- function (inverse) stored_inverse <<- inverse
    
    # Get the stored inverse, or NULL if one has not been stored:
    getinverse <- function () stored_inverse
    
    # Return a list containing the interface functions:
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Compute the inverse of the special "matrix" returned by `makeCacheMatrix`.
##
## If the inverse has already been calculated (and the matrix has not changed),
## then `cacheSolve` retrieves the inverse from the cache.
##
## cm - a "matrix" object created with `makeCacheMatrix`
##
## Returns the inverse of the stored matrix.

cacheSolve <- function (cm, ...) {
    # Request the stored matrix inverse:
    inverse <- cm$getinverse()
    
    # If we have a stored inverse, message the fact, and return the value:
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # Calculate the matrix inverse, store it, and return it:
    data <- cm$get()
    inverse <- solve(data, ...)
    cm$setinverse(inverse)
    inverse
}
