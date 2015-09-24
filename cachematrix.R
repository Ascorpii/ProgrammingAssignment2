## Put comments here that give an overall description of what your
## functions do

## Function creates matrix object to temporarily store our inverse matrix data

makeCacheMatrix <- function(x = matrix()) {

 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve= setsolve,
             getsolve = getsolve)
}


## Function calculates inverse matrix of the matrix object from function makeCacheMatrix. 
##If the inverse is already calculated, function returns inversed from cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
      
                message("getting cached data")
                return(m)
         
        }
        MX <- x$get()
        m <- solve(MX, ...)
        x$setsolve(m)
        m
        
}
