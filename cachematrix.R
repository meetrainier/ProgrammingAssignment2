## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(soln) inv <<- soln
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
		   inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                
                
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
# Geberating a 3x3 matrix for testing 
# c1 <- rnorm(3)
# c2 <- rnorm(3)
# c3 <- rnorm(3)
# m1 <- cbind(c1,c2,c3)
