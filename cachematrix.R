## makeCacheMatrix function create list that contains functions get, getsolve, set, setsolve

makeCacheMatrix <- function(x = matrix()) {
a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) a <<- solve
    getsolve <- function() a
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## "a" matrix is the inverse of matrix "x"

cacheSolve <- function(x, ...) {
         a <- x$getsolve()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setsolve(a)
    a
}
