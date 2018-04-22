## These functions take a matrix, cache it, invert it, cache the inverse, and 
## return the inverse from either cache or direct calculation.

## This function takes a square matrix and creates an object that can
## cache and return the original matrix, as well as cache and return its
## inverse once it's been calculated by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    ## First check if the matrix is square
    if(class(x) != "matrix"){
        stop("Object should be a matrix")
    } else if(dim(x)[1] != dim(x)[2]){
        stop("Matrix should be a square matrix")
    }
        
    ## If matrix is square, proceed 
    minv <- NULL
    set <- function(y){
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setminv <- function(inverse) minv <<- inverse
    getminv <- function() minv
    
    list(set = set, get = get,
         setminv = setminv, getminv = getminv)
}


## This function takes an object produced by MakeCacheMatrix() and returns 
## the inverse of the original matrix. 
## It first checks whether there is a cached version of the inverse of the 
## original matrix, and if there isn't, it calculates the inverse. 

cacheSolve <- function(x, ...) {
    
    minv <- x$getminv()
    if(!is.null(minv)){
        message("I'm lazy. Using the cached inverse matrix")
        return(minv)
    }
    origmat <- x$get()
    minv <- solve(origmat, ...)
    x$setminv(minv)
    minv
}


## A quick test to chech everything works

a <- matrix(1:4, nrow = 2)
a
b <- makeCacheMatrix(a)
cacheSolve(b)
cacheSolve(b)
a %*% cacheSolve(b)