## Sonia Baron 
## 06/25/2017

## makeCacheMatrix() will create a matrix and its inverse. The inverse of the matrix will be stored on cache memory 
## and can be called on using cacheSolve(). CacheSolve() will first attempt to retrive an existing inverse matrix 
## from the cached memory. If that's the case, it will message "getting cached data"; otherwise it will generate the
## inverse of a matrix. 

## makeCacheMatrix() stores a matrix and its inverse
## on chache memory. 

makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL 
    set <- function(y) {
        x <<- y 
        m <<- NULL
    }
      get <- function()x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
         list(set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)
}

## chacheSolve() will get the inverse of the matrix from 
## cache memory set in the environment makeCacheMatrix().

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)){
           message("getting cached data")
           return(m)
       }
       data <- x$get()
       m <- solve(data,...)
       x$setinverse(m)
       m
}
       

        ## Return a matrix that is the inverse of 'x'


