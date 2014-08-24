## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
##
## So, the makeCacheMatrix function:
## creates such object (let's call it cacheableMatrix) to hold cached inversed matrices
## and
## cacheSolve function:
## takes the created cacheableMatrix object, computes its inverse matrix, and
## returns it, if a second call is made for the same matrix object, then its
## cached results are returned (instead of computing the inverse yet again)


## This function creates a special object that holds a "matrix" and can cache its inverse
## it's "methods" are: get, set, setinvmartx, getinvmatrx

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvmatrx <- function(imatrx) m <<- imatrx
    getinvmatrx <- function() m

    list(set = set, get = get, setinvmatrx = setinvmatrx, getinvmatrx = getinvmatrx)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve shall retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    m <- x$getinvmatrx()

    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    data <- x$get()
    m <- solve(data, ...)
    x$setinvmatrx(m)

    m
}
