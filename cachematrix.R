## Put comments here that give an overall description of what your
## functions do:
## this pair of functions optimize the computation of the inverse of a matrix by caching
## the result, so it can be used without recalculating the inverse if it is used several times.
## It makes use of the R scoping rules to preserve the state inside the R object

## Write a short comment describing this function:
## This function creates a special 'matrix' object which is actually a list with functions to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse
##      4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # initialize
    inv <- NULL
    
    # function to change the matrix and thus reset its inv
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # function to retrieve the matrix
    get <- function() x
    
    #funtion to set the inverse
    setinv <- function(inverse) inv <<- inverse
    
    # function to retrieve the inverse
    getinv <- function() inv
    
    # create and return the list containing the previous functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function:
## This function returns the inverse of the special matrix created with the above function.
## It returns the inverse calculating it only if it had  not already been calculated 
## (and stores it for future uses). Otherwise it simply returns the already stored value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # retrieve the inverse stored in the special matrix
    inv <- x$getinv()
    
    # if a value has been retrieved, then return it because we had it cached
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise get the matrix, compute the inverse, store it and return it
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
