## Return a list object that represents an augmented matrix that can potentially store its own inverse accessible using the cacheSolve() function
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL  ## Store the inverse locally 
   set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv

    # Return a list object with the 4 setter and getter functions
    list(set = set, 
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Return the inverse of the matrix ‘x’ from cache or solve and return if not already cached
cacheSolve <- function(xc, ...) {
    
    ## Try and get the inverse of x from the cache    
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
        
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}

