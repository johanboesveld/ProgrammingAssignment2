## Below are two functions that calculate and cache the inverse of a matrix (assumed is that the matrix supplied is invertible!)
## If the inverse has already been calculated (and the matrix has not changed), it should retrieve the inverse from the cache.

## function makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse,this special object is a list containing 4 functions
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, getinverse=getinverse, setinverse=setinverse)
             }


## function cacheSolve:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## if an inverse already is cached, return that matrix
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        ## no cached inverse exist -> calculate the inverse of the matrix and cache it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
## end of programming assignment 2
