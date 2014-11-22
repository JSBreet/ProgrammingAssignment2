## The two functions below, makeCacheMatrix() and cacheSolve(), can be used to
## cache the inverse of a matrix. The first function, makeCacheMatrix(), creates
## a special "matrix" object that contains the matrix and can cache its inverse.
## The second function, cacheSolve(), computes the inverse of the special "matrix"
## as defined by makeCacheMatrix(). If the inverse has already been calculated
## and the matrix is still the same, cacheSolve retrieves the inverse from the cache. 

## makeCacheMatrix() takes as input a matrix (denoted by x) and creates the 
## special "matrix" object that is used to store two things: the original matrix 
## and the cache of its inverse. The cached value (denoted by i) is initiallly 
## set to NULL. makeCacheMatrix() contains four functions. The functions set and 
## get respectively set (change) and get (read) the value of the matrix x. The 
## functions setinverse and getinverse respectively set (change) and get (read) 
## the value of the inverse i. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() { x } 
        setinverse <- function(inverse) { i <<- inverse }
        getinverse <- function() { i }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() takes as input the special "matrix" object created by 
## makeCacheMatrix(). First, cacheSolve() accesses the special "matrix" object
## and retrieves the value of the inverse i via getinverse. If the inverse has 
## not been calculated (i is NULL), cacheSolve() computes the inverse, stores 
## it in the special "matrix' object via setinverse and returns the inverse. 
## If the inverse has been calculated earlier (i is not NULL), cacheSolve() 
## returns the message "getting cached data" and returns the inverse, thereby 
## saving computation time. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}