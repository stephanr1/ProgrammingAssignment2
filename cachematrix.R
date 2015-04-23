## Matrix conversions often will be computation intensive. The following 
## functions avoid repeated (identical) computations 
## by storing the result of the matrix computation in a cache.
## If done before, the result will be retrieved from cache instead of again.

## The functions are edited versions of the examples (makeVector & cachemean)
## in the assignement.

## makeCacheMatrix creates a list containing function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_m <<- inverse
        getinverse <- function() inv_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)    
}


## cacheSolve checks if the inverse of the matrix has been computed. If so, 
## it returns the value from cache. If not, it computes the inverse 
## of the matrix and a) returns it and b) stores it in the cache (inv_m).
## !! cacheSolve assumes that all matrices supplied are always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getinverse()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setinverse(inv_m)   
        inv_m
}
