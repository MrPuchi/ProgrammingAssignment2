## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getinverse()
  if (!is.null(inv)){
    message("Retrieving cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinverse(inv)
  inv
}

#Testing with a new Matrix
Test <- matrix(c(1,2,3,4),2,2)
Test

#Creating Cached Test
Testinv <-makeCacheMatrix(Test)

#First Inverse of Matrix "Test"
cacheSolve(Testinv)

#Second Inverse of Matrix "Test", should retrieve from cache since it was already calculated.
cacheSolve(Testinv)



