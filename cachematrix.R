## The function MakeCacheMatrix creates an object of type list with 4 functions
## and two data variables that are the input matrix and the cached inverse.

## The input matrix and the stored inverse can be modified and retrieved via calls
## to the nested functions.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  setmat <- function(y){
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(setmat = setmat,getmat = getmat,setinv = setinv,getinv = getinv)

}


## The cacheSolve function takes the argument of the list created by makeCacheMatrix()
## It checks if an inverse is stored and returns it if it finds one. Else it calculates
## the inverse and stores it in the input argument list, for retrieval in future calls.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting inverse from cached data")
    return(inv)
  }
  ipmat <- x$getmat()
  inv <- solve(ipmat, ...)
  x$setinv(inv)
  inv
}
