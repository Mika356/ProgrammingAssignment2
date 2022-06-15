## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A pair of functions that cache the inverse of a matrix

## This one creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL       ## Initialize the inverse property
        set <- function(y) {  #Setting the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x  ## Get the matrix, Returning the matrix
        setInverse <- function(inverse) inv <<- inverse  ## Set the inverse of the matrix
        getInverse <- function() inv  ## Get the inverse of the matrix, Returning the inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Write a short comment describing this function

##  The below function computes the inverse of the special matrix created above
##  using makeCacheMatrix.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if( !is.null(inv) ) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()  ## Getting the matrix
        inv <- solve(mat, ...)  ## Using matrix multiplication, calculate the inverse
        x$setInverse(inv)  ## Setting the inverse
        inv  ##Return the matrix
}

