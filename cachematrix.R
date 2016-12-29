## Computing the inverse of a matrix can be a costly computation
## The below functions create a matrix for which its inverse can be cached in memory
## instead of compute it repeatedly
## exemple : myMatrix <- makeCacheMatrix(matrix(c(3,-7,5,2),2,2))
##           cacheSolve(myMatrix)

## The makeCacheMatrix function creates a special matrix with 4 set/get functions for values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function () inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function calculates the inverse of a matrix created with the makecacheMatrix function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## check if the inverse already exists
    inv <- x$getinv()
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    data <-x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
