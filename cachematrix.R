## R Programming Programming Assignment 2: Lexical Scoping
## Assignment Title: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.

##Major assumption: The supplied matrix is always invertible.

##The following two functions are used to cache the inverse of a matrix:

## 'makeCacheMatrix' creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The 'cacheSolve' function returns the inverse of the matrix. If the inverse has already 
## been computed, it gets the result and skips the computation; else, it computes the inverse,
##  sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

##Sample run: 
##>m <- matrix(1:4,nrow = 2, ncol = 2)
##>x <- makeCacheMatrix(m)
##>cacheSolve(x)
##     [,1] [,2]            ##No cache in the first run
##[1,]   -2  1.5
##[2,]    1 -0.5	
##>cacheSolve(x)
##getting cached data.      ##Retrieving from the cache in the second run
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5	
