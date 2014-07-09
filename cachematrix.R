## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.
 
makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inverse_x <<-inverse
    getinverse <- function() inverse_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
 
## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("Getting inverse matrix from cache")
        return(inverse_x)
    } else {
        inverse_x <- solve(x$get())
        x$setinverse(inverse_x)
        return(inverse_x)
    }
}


#I define a particular matrix A as for example:
A = matrix (c (2,2,2,3), nrow =2, ncol=2)

#When I call the function as follows it calculates an inverse for the matrix and uses cache to do it multiple times.

mat = makeCacheMatrix (A)
for (i in 1:1000) {
 inverse_matrix= cacheSolve (mat)
                  }
inverse_matrix
   
