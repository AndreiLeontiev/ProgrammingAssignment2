## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function defines matrix m and methods used for getting and setting it 
## along with getting and setting inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(InvMatrix) m <<- InvMatrix
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)  
}

## cacheSolve function defines whether to calculate an inverse matrix
## created with function makeCacheMatrix or take the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}
