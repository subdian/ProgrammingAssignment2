## These functions provide caching behaviour for the computing the inverse of a matrix
## makeCacheMatrix creates a special amtrix object with cacheable inverse
## cacheSolve does the work of computing the inverse, caching, and returning the result

## This function creates a special "Matrix" object that
## has functions to:
## set the value of the matrix
## get the value of the matrix
## get the inverse of the matrix
## set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
               x <<- y
        inv <<- NULL
        }
    
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" created with makeCacheMatrix
## It first checks if the inverse has been calculated. If it has been, it uses the getinverse function
## to return the value, otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
               message("getting cached inverse")
               return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
