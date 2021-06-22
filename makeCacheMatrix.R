makeCacheMatrix <- function(k = matrix()){
  inv <- NULL
  set <- function(y){
    k <<- l
    inv <<- NULL
  }
  get <- function() {k}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get=get, setInverse = setInverse, getInverse =getInverse)
}
cacheSolve <- function(k, ...){
  inv <- k$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- k$get()
  inv <- solve(mat, ...)
  k$setInverse(inv)
  inv
}
