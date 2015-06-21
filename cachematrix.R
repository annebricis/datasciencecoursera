## These functions calculate and cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" which is a list containing a function 
## which can set the value of the matrix, get the value of the matrix, set the inverse
## of the matrix or get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve is a function that first checks whether the inverse has already been
## calculated; if so it gets the inverse from the cache otherwise it calculates the
## inverse and then stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
