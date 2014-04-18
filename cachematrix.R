## These functions create a special object that stores
## a matrix and chaches its inverse

## makeCacheMatrix creates a list containing 4 items,
## which are functions:
## set the value of matrix
## get the value of matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##CaschSolve calculates the "inverse" of a matrix. It first
##checks to see if the inverse has already been calculated. 
##If so, it "gets"  the inverse from the cache. If not, it 
##calcualtes the inverse using solve() and then "sets"
##the inverse using "setinverse"

cacheSolve <- function(x, ...) {
        cacheinverse <- function(x, ...) {
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m}
}