# makeCacheMatrix takes a numeric matrix, stored in variable x internally
# cacheSolve creates inverse matrix, stored in variable inv internally
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

    # Get the matrix
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


    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


cacheSolve<- function(x, ...) 
{

    inv <- x$getinv()

    if(!is.null(inv)) 
    {
        message("getting cached data")
        return(inv)
    }
	
    # Calculates inverse if it is not stored 
    data <- x$get()
    inv <- solve(data)

    # now set the mean in x so we cache it and dont need to needlessly
    x$setinv(inv)
    
    # return the caching vector
    inv
}
