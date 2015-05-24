## This are functions written for Programming Assignment 2, 
## from 'R Programming' (May 4, 2015 - July 1, 2015).
#
# Together, they can be used to avoid computational cost of inversing the same matrix
# multiple times.
#
#
## Function makeCacheMatrix
# 
# This function takes matrix 'x' creatres special 'matrix-like' object, that stores both 
# the matrix and its inversion (technically it creates a list of four element).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                              # Cache initially set empty.
    set <- function(y) {                   # A 'subfunction' for creating a matrix.
        m <<- NULL                         #
    }
    get <- function() {                    # A 'subfunction' for seting values 
        x                                  # inside the matrix.
    }
    setinvert <- function(inverse) {       # A 'subfunction' for inverting the matrix
        m <<- inverse                      # and storing it in the cache.
    }
    getinvert <- function() {              # A 'subfunction' calls inverted matrix 
        m                                  # from the cache.
    }
    list(set = set,                        # List of all four 'sub-functions'.
         get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## Function cacheSolve
#
# Takes as input the output of function makeCacheMatrix and prints inversion of matrix 'x'

cacheSolve <- function(x , ...) {
    m <- x$getinvert()                      # Calls inverted matrix from the cache
    if(!is.null(m)) {                       # If cache is non empty...
        message("getting cached data")      # 
        return(m)                           # ...inverted matrix is taken from cache
    }
    data <- x$get()                         # If else, inverted matrix is calculated
    m <- solve(data, ...)
    x$setinvert(m)
    m
}

## Example 

samplematrix <- matrix(c(1,0,4, 1,3,4, 4,1,0), nrow = 3, ncol = 3, byrow = TRUE,
                        dimnames = list(c("row1", "row2", "row3"),
                                        c("C.1", "C.2", "C.3")))

sm <- makeCacheMatrix(samplematrix)

cacheSolve (sm)
