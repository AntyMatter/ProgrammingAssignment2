## Anthony Matthews AntyMatter AntyMatter@quallo.com 818-634-0826

## Put comments here that give an overall description of what your
## functions do

## These functions together calculate and store the inverse of a
## square matrix. FUnction makeCacheMatrix creates and passes back four
## new functions set, get, setInverse and getInverse, and function
## uses these new functions to either pull the previously set value of
## the inverse, or to create a new value.
##
##
##

## this function takes an argument of a square matrix.  The internally
## created functions:
## set: set 
##
##
##
##

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




## Write a short comment describing this function
## this function looks to see if x$getInverse (i) is NULL
## if not, it reports that it is using the cached value of i
## and returns that.  Otherwise, it pulls the value of the matrix
## using function x$get and reevaluates i, returning that. 

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}

## This was a fun exercise

