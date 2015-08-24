# makeCacheMatrix creates a list that contains a function to:
# 1. set the value of a matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix = function(matrix=matrix()) {

   # holds the cached value or NULL if nothing is cached
   matrixInverse <- NULL

   # stores a matrix
   setMatrix <- function(newValue){
      matrix <<- newValue
      # clears the cache as the matrix is assigned a new value
      matrixInverse <<- NULL
   }

   # returns the stored matrix
   getMatrix <- function() matrix

   # caches the inverse of the matrix
   setInverse = function(solve) matrixInverse <<- solve

   # gets the cached value for the inverse of the matrix
   getInverse = function() matrixInverse

   # returns a list where each element of the list is a function
   list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}



# cacheSolve returns the inverse of the matrix. It first checks if the inverse 
# has already been computed. If so, it gets the result and skips the computation. 
# If not, it computes the inverse and sets the value in the cache via the 
# setInverse function.

# This function assumes that the matrix is always invertible.

cacheSolve = function(matrix, ...) {
   # get the inverse of the cached matrix
   matrixInverse = matrix$getInverse()
   # return the cached value is it exists
   if(!is.null(matrixInverse)){
      message("Retrieving cached data...")
      return(matrixInverse)
   }
   # calculates the inverse of the cached matrix if it was not already stored
   # in the cache
   cachedMatrix = matrix$getMatrix()
   matrixInverse = solve(cachedMatrix)
   matrix$setInverse(matrixInverse)

   # return the value of the inverse
   matrixInverse
}
