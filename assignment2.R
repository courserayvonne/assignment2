# from rstudio

#1
makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse.
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#2
cacheSolve <- function(x, ...) {
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}