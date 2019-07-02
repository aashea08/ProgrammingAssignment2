## Below are two functions that 1) create an object that stores a matrix and
## its inverse and 2) retrieves the inverse matrix from the cached value that is
## stored in the first function.

## makeCacheMatrix contains four functions and two data objects. It clears
## any value of m that has already been set and sets the value for x in the parent
## environment. It also builds a set of functions and returns them within a list
## to the parent environment where they can be used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        inverse <- solve(x)
        get <- function() x
        setinverse <- function(solve) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve completes the makeCacheMatrix function by populating and/or
## retrieving the inverted matrix (it checks if there is a cached inverted matrix
## and if so, returns it to the parent environment. If not, it finds the inverse
## of the matrix using functions defined in makeCacheMatrix and returns it to the
## parent environment).

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
