## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y){
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function delivers the inverted matrix from makeCacheMatrix,
# first looking for a cached version and if not there
# computing and returning the inversion.

cacheSolve <- function(x, ...) {
    # Return an inverted matrix of the passed matrix.
    inv <- x$getinv()

    # Look for a cached version
    if(!is.null(inv)) {
        message("Aw yiss, a cached version.")
        return(inv)
    }
    
    # Otherwise invert the matrix.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

# Test it out
test <- makeCacheMatrix()
test$set(matrix(c(1:4),2,2))
cacheSolve(test)
