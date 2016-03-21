# This function calculates inverse of Matrix. Matrix has to be defined for which the inv can be caluclated.
#Has more functions associated with the main function that get the inv and set it.

makeCacheMatrix <- function(x, ...) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
#Cache Solve gets inv of the matrix, check if has some value, if yes then it pulls it from cache, otherwise calculate inv.
# Extra comment
cachesolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix,...)
        x$setinv(inv)
        inv
}