## These functions find the inverse of a square matrix, but also 
## check if the result has already been obtained and cached, and 
## if it was, the solution is printed without doing repeated 
## computation. If not, the new result is hashed.

## "makeCacheMatrix" function creates a special "matrix" which
## is really a list containing a function to: 1) set the value
## of the matrix; 2) get the value of the matrix; 3) set the
## value of the inverse matrix; 4) get the value of the 
## inverse mtrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse)  {
        inv <<- inverse
    }
    getInverse <- function() inv
    list (set = set, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)
}


## "cacheSolve" function caluclates the inverse of the special
## "matrix" created with the above function, but first checks
## if the result was already cached, and if it was, it prints
## it out. If not, it does the calculation, caches the result
## and prints it out.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
