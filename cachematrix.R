## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.Write a short comment describing this function

        makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setInverse <- function(Inverse) m <<- Inverse
                getInverse <- function() m
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        }


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
        cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                inv <- x$getInverse()
                if (!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                mat <- x$get()
                inv <- solve(mat, ...)
                x$setInverse(inv)
                inv
        }
## Return a matrix that is the inverse of 'x'

