## Creates a special matrix capable of caching its own inverse

## Makes the special cache matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinverse =  setinv, 
         getinverse = getinv)
}


##Returns the inverse of the matrix and, if the cached matrix
##is null, caches a new inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv))
    {
        message("Getting previously calculated inverse")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinverse(inv)
    inv
}
