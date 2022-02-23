##Jonathan's Coursera R Programming Assignemnt 2 Lexial Scoping
##Assignment: Caching the Inverse of a Matrix - write a pair of functions that 
##cache the inverse of a matrix
##Write the following functions:
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: This function computes the inverse of the special "matrix" returned by make CacheMatrix above. 
##   if the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##   retrieve the inverse from the cache. 

##Using the vector example to help guide us on how to solve the assignment. We need to set up the values of the matrix
##and then get the values of the matrix. Then after that get and set the values of the inverse. 

## 1st part is to set up the values of the matrix
## Use makeCacheMatrix to cache its inverse
## This allows the user to set the list of values and the # rows and # columns they want for it
makeCacheMatrix <- function(x = matrix()){
  N <- NULL
  set <- function(y){
    x <<- y
    N <<- NULL
  }
  ## 2nd part is to get the values of the matrix
  get <- function()x
  setInverse <- function(inverse)N <<- inverse
  getInverse <- function()N
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
  
## 3rd part is to set and get the values of the Inverse Matrix
## Use cacheSolve to finalize the matrix
cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        N <- x$getInverse()
        if(!is.null(N)){
          message("getting cached data")
          return(N)
        }
        values <- x$get()
        N <- solve(values, ...)
        x$setInverse(N)
        N
}





