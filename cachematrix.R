## Storing a result (cache) can save you and R a lot of time by allowing you to *skip*
## unnecessary computational steps that have already been done. In essence, the
## below functions are designed to optimize your code such that it runs faster.
## Here we split the process into two main steps. One is to create a "special"
## matrix with the the function makeCacheMatrix, that is primed to store the 
## values of its inverse. These results are cached and available for later 
## uses if needed. The second step involves the function cacheSolve, that calculates the 
## inverse of a matrix but before doing so, addresses the cached data from function 
## one and returns the saved results if available
## (i.e. if the inverse of that matrix has already been calculated). If no such
## data is available, it calculates the inverse of the matrix.        

## makeCacheMatrix takes an invertible matrix as its argument and can cache its 
## inverse by making a list of four values.
## The four values are obtained from four functions that do the following: 
## 1) sets the matrix value using the makeCacheMatrix environemnt; 2) gets the
## value of the matrix; 3) solve and set the inverse of the matrix;
## 4) gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
       matInv <- NULL 
       setmat <- function(y){ 
               x <<- y
               matInv <<- NULL
       } 
       getmat <- function() x
       setInv <- function(solve) matInv <<- solve
       getInv <- function() matInv
       list(setmat = setmat, getmat = getmat,
            setInv = setInv, getInv = getInv)
}
        

## The cacheSolve function takes a matrix that was made with makeCacheMatrix or not.
## It sifts through the cache data made from makeCacheMatrix and prints out the 
## value of the matrix from the cache if it is present (along with a message).
## If the inverse of the matrix is not stored it solves for it and sets it to 
## a blank variable and returns it.

cacheSolve <- function(x, ...) {
       matInv <- x$getInv()
       if(!is.null(matInv)) {
               message("retreived from cache data...")
               return(matInv)
       }
       mat <- x$getmat()
       matInv <- solve(mat, ...)
       x$setInv(matInv)
       matInv
}
