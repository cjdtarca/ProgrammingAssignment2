makeCacheMatrix <- function(z = matrix()) {
  inv <- NULL
  set <- function(b){
    z <<- b
    inv <<- NULL
  }
  get <- function() {z}
  setInverse <- function(inversematrix) inv <<- inversematrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(z, ...) {
      
  inv <- z$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix <- z$get()
  inv <- solve(matrix, ...)
  z$setInverse(inv)
  inv   
}
