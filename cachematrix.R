# makeCacheMatrix takes a numeric matrix, stored in variable x internally
# cacheSolve creates inverse matrix, stored in variable inv internally
# Use cacheSolve to cache the inverse

# Use x$get() to get the original matrix
# Use x$set() to set the original matrix (it sets the inverse matrix to NULL)

# Use x$getinv() to get the inverse matrix
# Use x$setinv() to manually set the inverse matrix

# Use cacheSolve(x) to calculate the inverse matrix (if it is missing), and get it

# I have used this source as a help:
# https://github.com/ecxr/RProg/blob/master/proj2/cachemean.R

makeCacheMatrix <- function(x = matrix()) 
{
    # Default value for Inverse is NULL
    inv <- NULL 
    
    # Sets new value for the matrix
    # Resets Inverse to NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }

    # Get the original matrix
    get <- function()
    {
        x
    }

    # Sets the desired Inverse matrix
    setinv <- function(inverse) 
    {
        inv <<- inverse
    }
    
    # returns the inverse
    getinv <- function() 
    {
        inv
    }

    # Commands of this object
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


cacheSolve<- function(x, ...) 
{

    # get the cached inverse matrix
    inv <- x$getinv()

    # If it has been stored return with it
    if(!is.null(inv)) 
    {
        message("getting cached data")
        return(inv)
    }
    
    # Calculates inverse if it is not stored 
    data <- x$get()
    inv <- solve(data)

    # now set the inverse matrix so we cache it and dont need to needlessly recalculate
    x$setinv(inv)
    
    # return the caching vector
    inv
}
