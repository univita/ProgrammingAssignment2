## Put comments here that give an overall description of what your
## functions do
## A pair of functions that cache the inverse of a matrix.
## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL 					## Initially assigning 'NULL' to inverse
    set_matrix <- function(y) {
        x <<- y 					## Setting the matrix 'x'
        inverse <<- NULL
    }
    get_matrix <- function() x 				## Returning matrix 'x'
    set_inverse <- function(solve) inverse <<- solve 	## Cache the value of the inverse
    get_inverse <- function() inverse 			## Returning inverse
    list(set_matrix = set_matrix, get_matrix = get_matrix,
    set_inverse = set_inverse,
    get_inverse = get_inverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()				## Getting inverse
        if(!is.null(inverse)) {					## Checking for the presence of inverse
            message("getting cached data")			## Displaying message
            return(inverse)
        }
        data <- x$get_matrix()					## Getting Matrix
        inverse <- solve(data, ...)				## Using solve() to compute inverse
        x$set_inverse(inverse)					## To cache the inverse
        inverse 						## Returning the inverse
}
