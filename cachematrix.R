## This function creates a special "matrix" object that can cache its INVERSE

## This first function creates a special "matrix" with a list that tells the function to:
## 1. Set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the INVERSE
## 4. get the value of the INVERSE

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This second function calculates the INVERSE of the special "matrix" returned by the makeCacheMatrix above
## If the inverse has already been calculated, cachesolve should retrieve inverse from cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data,...)
  x$setinverse(m)
  m
}
