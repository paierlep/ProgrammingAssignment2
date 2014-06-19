## This example enables it to optimize the calculation of the inverse of a
## matrix, simply by putting already calculated results into a cache. In
## this assignment we are allowed to assume that for every matrix that is going
## to use these functions it is possible to calculate the inverse
##
## Short example:
##   > m <- makeCacheMatrix(matrix(c(2,1,3,2), ncol=2))
##   > m$get()
##   > cacheSolve(m)
##   >     [,1] [,2]
##   [1,]    2   -3
##   [2,]   -1    2
##   > cacheSolve(m)
##   getting cached data
##        [,1] [,2]
##   [1,]    2   -3
##   [2,]   -1    2

## This function acts as some kind of constructor for the matrix cache
## and provides three function: `set` is used when creating the cache,
## `get` is used to get the values of the matrix, `setinv` is for storing the
## result of the inverse, and `getinv` to get the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i

        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function is used to actually calc the inverse and store it in the cache
## If the matrix object already holds a result of the inverse calculation, we
## can simply return this. else the calculation is done by using `solve` and
## the result is stored in the cache by using `setinv`

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if (!is.null(m)) {
                message("getting cached data");
                return(m);
        }

        data <- x$get();
        m <- solve(data, ...)
        x$setinv(m);
        return (m)
}
