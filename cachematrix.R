## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setinvert <- function(inverse) {
        m <<- inverse
    }
    getinvert <- function() {
        m
    }
    list(set = set, 
         get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## Write a short comment describing this function

cacheSolve <- function(x , ...) {
    m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}

samplematrix <- matrix(c(1,0,4, 1,3,4, 4,1,0), nrow = 3, ncol = 3, byrow = TRUE,
                        dimnames = list(c("row1", "row2", "row3"),
                                        c("C.1", "C.2", "C.3")))

sm <- makeCacheMatrix(samplematrix)

cacheSolve (sm)
