## In this file, there are two functions as an implementation of
## getting and caching the inverse of any invertable matrix.


## This function was written create the cached inverse matrix
## It contains four in-line functions. The main overall
## functionality of this function is either to set and get
## the matrix, or to set and get the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function was written to get the inverted matrix.
## Either by getting the cached one, or by calculating
## it. Using Solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}