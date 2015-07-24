## Program to cache the inverse of a matrix and reuse it from cache 

## This function creates a special "matrix" object that can cache its inverse
 
makeCacheMatrix <- function(x = matrix()) {
  ss <-NULL
  set<-function(y){
  x<<-y
  ss <<-NULL
}
get<-function() x
setmatrix<-function(solve) ss <<- solve
getmatrix<-function() ss
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
##  inverse from the cache.
 
cacheSolve <- function(x, ...) {
  ss <-x$getmatrix()
  if(!is.null(ss)){
    message("getting cached data")
    return(ss)
  }
  matrix<-x$get()
  ss<-solve(matrix, ...)
  x$setmatrix(ss)
  ss
}
