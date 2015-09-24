## cachematrix.R consists of a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(originalMatrix = matrix()) {
  
  invertedMatrix <- NULL
  
  # set matrix value
  set <- function(y) {
    originalMatrix <<- y
    invertedMatrix <<- NULL
  }
  
  # get matrix value
  get <- function() originalMatrix
  
  # set inverted matrix
  setInverse <- function(solve) m <<- solve
  
  # get inverted matrix
  getInverse <- function() m
  
  # return list
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(cachedMatrix, ...) {
  
  invertedMatrix <- cachedMatrix$getInverse()
  
  # check if cached data is avaliable
  if(!is.null(invertedMatrix)) {
    message("getting cached data")
    return(invertedMatrix)
  }
  
  # created inverted matrix if nothing is cached
  data <- cachedMatrix$get()
  invertedMatrix <- solve(data, ...)
  cachedMatrix$setInverse(invertedMatrix)
  invertedMatrix
}
