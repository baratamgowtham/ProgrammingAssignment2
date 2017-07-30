## Functions to retrive a value from the cache if available.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  x <- matrix(4:7,2,2)
  y <- x
  
  set  <- function(y) x <<- y
  get  <- function() x
  
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  print(x)
  
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## If the inverse has already been calculated 
##(and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache ## else it should calculate it


cacheSolve <- function(xx=matrix(), ...) {
  inverse  <- xx$getinverse()
  
  print(x)
  data  <- xx$get()
  print(data)
  
  if (identical(x,data) == TRUE){
    if (!is.null(inverse)){
      message("getting cached data")
      return(inverse)
    }
  }
  else{
    message("Data of x has changed, so calculating it")
    x <- data
    xx$set(x)
  }
  
  i  <- solve(data, ...)
  xx$setinverse(i)
  i
}