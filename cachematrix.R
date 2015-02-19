## Below are two functions that calculate and cache the inverse of a matrix (assumed is that the matrix supplied is invertible!)
## If the inverse has already been calculated (and the matrix has not changed), it should retrieve the inverse from the cache.

## function makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse,this special object is a list containing 4 functions
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL                                    ## clear any old inverse
        set <- function(y) {                            ## function to store the original matrix
                x <<- y                                 ## in variable x in the environment created by makeCacheMatrix()
                invm <<- NULL                           ## clear inverse when original matrix has changed
        }
        get <- function() x                             ## function to retrieve the original matrix
        setinverse <- function(solve) invm <<- solve    ## function to store the calculated inverse in variable m
                                                        ## again in the environment created by makeCacheMatrix()
        getinverse <- function() invm                   ## function to retrieve the cached inverse
        ## create the special object (list with 4 functions)        
        list(set = set, get = get, getinverse=getinverse, setinverse=setinverse)
             }


## function cacheSolve:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## check if an inverse has been cached, if so: return the cached inverse 
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        ## no cached inverse exist -> calculate the inverse of the matrix and cache it
        data <- x$get()                 ## retrieve the original matrix
        inv <- solve(data, ...)         ## calculate the inverse
        x$setinverse(inv)               ## cache the inverse
        inv                             ## return the calculated inverse
}
## end of programming assignment 2
