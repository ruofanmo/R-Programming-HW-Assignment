## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL ##create a vector
       set <- function(y) { ##create a new function
           x <<- y
           inv <<- NULL 
       }
       get <- function() x 
       setinverse <- function(inverse) inv <<- inverse
       getinverse <- function() inv ## use the function
       list(set = set, get = get, # set up a list
            setinverse = setinverse,
            getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse() ##set a new variable
        if(!is.null(j)) {
            message("getting cached data")
            return(j)
        }
        mat <- x$get()
        j <- solve(mat,...) #create a inverse matrix
        x$setInverse(j)
        j
}