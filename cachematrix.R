## These are two functions that create a special  object 
##that stores a matrix and caches its inverse

## The first function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <-function(y) {
        x <<- y
        inversa << - NULL
        }
        get <- function() x
        setInversa <- function(minversa) inversa <<- minversa
        getInversa <- function() inversa
        list(set = set,get = get,
             setInversa = SetInversa
             getInversa = getInversa)

}


## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix shown above 
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the cacheSolve should retrieve the inverse from the cache.
## The inverse (of a square matrix) is computed with the function solve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversa <- x$getInversa()
        if(!is.NULL(inversa)){
           message("getting cache data")
           return(inversa)
        }
        matinv <- x$get()
        inversa <- solve(matinv,...)
        x$setInversa(inversa)
        inversa
}
