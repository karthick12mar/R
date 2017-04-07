
#To store matrix in a vector along with its inversion

makeCacheMatrix <- function(x = matrix()) {
         inverse <- NULL
         set <- function(y) {
                 x <<- y
                 inverse <<- NULL
         }
         get <- function() x
         setinv <- function(inv) inverse <<- inv
         getinv <- function() inverse
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}

#To get matrix inversion from cache or calculate and save the inverse 
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get ()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
