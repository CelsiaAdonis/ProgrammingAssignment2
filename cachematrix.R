## Calculates and caches the inverse of your matrix.

## Creates a cache for the inverse of your matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setinv <- function(my_inverse) inv_matrix <<- my_inverse
        getinv <- function() inv_matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Checks whether the inverse of your matrix has already been calculated.
## If not, it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        inv_matrix <- x$getinv()
        if (!is.null(inv_matrix)) {
                message ("Retrieving cached data...")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data)
        x$setinv(inv_matrix)
        inv_matrix
}
