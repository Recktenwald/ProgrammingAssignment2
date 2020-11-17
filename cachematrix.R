## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # when initializing we do not have computed the inverse yet
    set <- function(y){ 
        # setter function
        # even if we had the inverse of the previous matrix
        # when changing it, this invalidates the inverse
        # so we note that we do not have it anymore by setting it to NULL
        x <<- y
        inv <<- NULL
    }
    get <-  function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){ # check if we already had an inverse
        message("getting cached data")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...) # compute the inverse
    x$setinv(inv) # and cache it
    inv
}

