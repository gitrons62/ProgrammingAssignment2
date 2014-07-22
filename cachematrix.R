## Put comments here that give an overall description of what your
## functions do

## makes an object containing referrences to the input matrix
## and its inverse.
## 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(mtrx) inv <<- mtrx
  getInvMatrix <- function() inv
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
  
}

## function to return the inverse of a cached matrix. If
## inverse has been cached it returns it else it calculates
## the inverse caches and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv   <- x$getInvMatrix()
  if(!is.null(inv)) {
    message("getting cached inverse")
  return(inv)
  }
  xdata <- x$get()  
  inv <- solve(xdata)
  x$setInvMatrix(inv)
  return(inv)
}

## test matrix
p=matrix(c(1,0,5,2,1,6,3,5,0),3,3)
k=makeCacheMatrix(p)
