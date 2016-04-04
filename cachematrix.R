## Function makeCacheMatrix:
## Takes a square matrix (if argument is not a square matrix, aborts). 
## Afterwards, constructs the special list containing a function built according to the 
## guidelines given in the sample function makeVector, making appropiate substitutions:
## minv instead of m, solve instead of mean, [setinv, getinv] instead of [setmean, getmean].
## Thus, the list is prepared to process the next call to function cacheSolve, doing searches
## through parent environments in order to determine if the answer is cached or it isn't.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    if (class(x) != "matrix") {
        print('Argument should be a matrix')
        return(minv)
    }
    else {
        if (dim(X)[1] != dim(X)[2]) { 
            print('Argument should be a square matrix') 
            return(minv)
        }
    }
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    
    setinv <- function(solve) minv <<- solve
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
 }

## Function cacheSolve:
## Using the special list built in the previous function as only argument,
## this function calculates the inverse of the matrix given originally as
## input argument to makeCacheMatrix, or recovers it from cache, if it is 
## the case..

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    minv <- x$getinv()
    if (!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    minv
}


## A basic example of using this two functions in tandem may be:
## L <- makeCacheMatrix(matrix(c(5,-4,5,-6), nrow=2, ncol=2))
## cacheSolve(L)
## Whose answer corresponds to matrix(c(0.6,-0.4,0.5,-0.5), nrow=2, ncol=2)), the inverse
## of the original square matrix given. 
## A second call to cacheSolve gives:
## "getting cached data" and the same answer