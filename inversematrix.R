  ## The following functions can cache the inverse of a matrix.  
  ## They are very similar to the functions given in the example.

  ## The first function creates a special "matrix" object that can cache its inverse.
  ## The function can 
  ## 1. set the matrix
  ## 2. get the matrix
  ## 3. set the inverse value of the matrix
  ## 4. get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix) {
  inv <- NULL
  
  ## set the value of the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse value of the matrix
  setInverse <- function(inverse) inv <<-inverse
  
  ## get the inverse value of the matrix
  getInverse <- function() inv
  
  ## return available functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  ## Return the cached inverse matrix if it's already been computed
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  ## compute inverse of matrix
  mat <- x$get()
  inv <- solve(mat, ...)
  
  ## cache inverse
  x$setInverse(inv)
  
  ## return inverse of matrix
  inv
}
  ## -----------test the function-------------
  ## m <- matrix(rnorm(25),5,5)
  ## m1 <- makeCacheMatrix(m)
  ## cacheSolve(m1)
  ##             [,1]       [,2]        [,3]       [,4]       [,5]
  ## [1,] -0.88440584 -0.8330480  0.76917855  0.9948458 -0.7249480
  ## [2,] -0.03501747 -0.3181369 -0.27863369  0.2512291 -0.2165067
  ## [3,]  2.83195222  3.5446589 -2.31337233 -3.8115585  1.7369951
  ## [4,] -0.89020671  0.2166472  0.01509881  0.1355653  0.5700177
  ## [5,] -0.28103544 -0.5434581 -0.24544398 -0.1629645  0.2403202
  ## cacheSolve(m1)
  ## inverse is cached
  ##             [,1]       [,2]        [,3]       [,4]       [,5]
  ## [1,] -0.88440584 -0.8330480  0.76917855  0.9948458 -0.7249480
  ## [2,] -0.03501747 -0.3181369 -0.27863369  0.2512291 -0.2165067
  ## [3,]  2.83195222  3.5446589 -2.31337233 -3.8115585  1.7369951
  ## [4,] -0.89020671  0.2166472  0.01509881  0.1355653  0.5700177
  ## [5,] -0.28103544 -0.5434581 -0.24544398 -0.1629645  0.2403202
