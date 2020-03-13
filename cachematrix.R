## In this project two functions are provided to store
## a matrix and also get its inverse quickly.

## Create a special "matrix" that is a list containing functions to
## set/get the value of a matrix
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Calculate the inverse of special "matrix" created by above function.
## It will return cached value if already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}

## Some testing

## > b <- matrix(1:4, nrow = 2, ncol = 2)
## > c <- makeCacheMatrix(b)
## > cacheSolve(c)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(c)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

