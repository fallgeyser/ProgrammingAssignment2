#############################################################
## makeCacheMatrix() and  cacheSolve() are used to cache the 
## inverse of an inversible matrix
##

## makeCacheMatrix() function gets an inversible matrix and retuens 
## a list containing functions to get/set the matrix itself and it's inverse
##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv 
        list(set = set, get = get,
             setInverse = setInverse ,
             getInverse = getInverse )
}


## cacheSolve() function gets a cached matrix (a list that is the retuen value of makeCacheMatrix())
## and returns the inverse of the matrix. if the inverse is calculated before
## just retuens it and does not recalculate it. 
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse ()
        if(!is.null(inv)) {
                message("Getting matrix inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
