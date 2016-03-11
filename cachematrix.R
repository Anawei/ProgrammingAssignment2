## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.I wrote a pair of 
## functions that cache the inverse of a matrix.

## makeCacheMatrix function is used to create a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
               x <<- y
               m <<- NULL
       }
       get <- function()x
       setinv <- function(inverse) m<<- inverse
       getinv <- function() m
       list(set = set,get = get, setinv=setinv,getinv = getinv)
}


## CatheSolve function is used to compute the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## it’ll retrieves the inverse from the cache directly.

CacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m = x$getinv()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        mat.data = x$get()
        m = solve(mat.data, ...)
        
        x$setinv(m)
        
        return(m)
}
