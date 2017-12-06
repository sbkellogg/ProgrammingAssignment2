## The following pair of R functions address the costly computation of matrix inversion by caching the inverse of a matrix rather than computing it repeatedly.

## The first function is a slight tweak to the example function that creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set_matrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## The second function computes the inverse of a square matrix.

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get_matrix()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}
