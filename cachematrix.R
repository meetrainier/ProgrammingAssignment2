## Goal: To facilitate efficient computation of inverse of matrix
## Description: This file has two functions, makeCacheMatrix() and cacheSolve(). 
## makeCacheMatrix() outputs the matrix that can be input  to cacheSolve().
## Thus using makeCacheMatrix() before cacheSolve() is a must.  The first use of 
## cacheSolve() computes the inverse. The second or subsequent call simply returns 
## the inverse and hence is efficient. 
## 
## Example use:
# Generating a 3x3 matrix for testing 
# >seq <- rnorm(9)
# >m1 <- matrix(seq,3,3)
# >m2 = makeCacheMatrix(m1)
# >cacheSolve(m2)
# m2inv <- cacheSolve(m2)  ###  we expect "cached data" message here.
# m1 %*% m2inv       ### we expect identity matrix here

## Input argument: a numeric square matrix 
## Output:  a special matrix that remembers its inverse

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

## Role: Finding inverse of a matrix when it is repeatedly needed.    
## input argument: a matrix that is output of makeCacheMatrix()  function
## output: the inverse of matrix that was input to makeCacheMatrix()  function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
		   inv <- x$getinv()
        if(!is.null(inv)) {
                message("Retreiving cached data.")
                return(inv)
                
                
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
# Geberating a 3x3 matrix for testing 
# seq <- rnorm(9)
# m1 <- matrix(seq,3,3)
# m2 = makeCacheMatrix(m1)
# cacheSolve(m2)
# m2inv <- cacheSolve(m2)  ###  we expect "cached data" message here.
# m1 %*% m2inv       ### we expect identity matrix here

