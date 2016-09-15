## This code is going to write a pair of functions that cache the inverse of a matrix, in order to exploit the benefits of caching instead of computing it

## FIRST FUNCTION: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # objectives: set/get value of the matrix, set/get its inverse
    # set value of matrix
  m <- NULL
  setmatrix <- function(y)
  {
    x <<- y
    m <<- NULL
  }
    # get value of matrix
  getmatrix <- function() x
    # set inverse of the matrix
  setinverse <- function(solve) m <<- solve
    # get inverse of the matrix
  getinverse <- function() m
  # Finally, let's store our output in a list
  list(setmatrix=setmatrix, getmatrix=getmatrix, 
       setinverse=setinverse, getinverse=getinverse)
  
}

## SECOND FUNCTION: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Objective: Return a matrix that is the inverse of 'x'
  
            m <- x$getinverse
            # if the inverse is not null (has already been calculated)
            if(!is.null(m))
            {
              # let's calculate it
              message("getting cached data")
              return(m)
            }
            # otherwise we take it from the cache
            data <- x$getmatrix()
            m <- solve(data, ...)
            #set the value of the inverse
            x$setinverse(m)
            #return m
            return(m)
}
