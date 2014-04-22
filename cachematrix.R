## R programming, Assignment 2
## description: write a pair of functions that cache the inverse of a matrix 


##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse_matrix) m_inv <<- inverse_matrix
    get_inv <- function() m_inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m_inv <- x$get_inv()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data, ...)
    x$set_inv(m_inv)
    m_inv
}
