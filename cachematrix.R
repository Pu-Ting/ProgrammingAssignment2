## Getting and Cashing the Inverse of a Matrix:
## Matrix inversion is usually a costly computational and they might be 
## beneficial to caching the inverse of a matrix rather than computing it repeatedly.
## Below are a pair of functions to achieve this purpose.
## This function creates a special 'matrix' object that can cache its inverse。

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y){
                x <<- y
                inv <<- NULL
                }
        get <- function()x
        setinverse <- function(inverse) inv<<-inverse
        getinverse <- function() inv
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)                
}


## This function is to solve and computes the inverse of matrix created
## This funciton will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv                   
}
