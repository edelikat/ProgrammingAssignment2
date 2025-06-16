## This function creates a special "matrix" and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invmtrx <- NULL
        set <- function(y) {
                x <<- y
                invmtrx <<- NULL
        }
        get <- function() x
        setinv <- function(inv) x <<- inv
        getinv <- function() invmtrx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the inverse of matrix x. If an inv already exists, it returns the cahced inverse.

cacheSolve <- function(x, ...) {
        invmtrx <- x$getinv()
        if(!is.null(invmtrx)) {
                message("getting cached data")
        } else {
                invmtrx <- solve(x$get())
                x$setinv(invmtrx)
        }
        invmtrx
}