## Calculate the Inverse of a Matrix
## Provide a caching mechanism to reduce computation costs

## `makeCacheMatrix`: Create a matrix with a memoizable solution
##
## args:: *theMatrix* is just a matrix
##
## methods::
## $setMatrix - will (re)set the original matrix and clear the memoized solution
## $getMatrix - returns the original matrix
## $setSolution - sets the calculated solution
## $getSolution - returns the calculated solution
## $cacheHit - increment the cache hit counter
## $getHits - returns the number of times there was a cache "hit"
##            and we skipped recomputing the solution
makeCacheMatrix <- function(theMatrix = matrix()) {
  cachedSolution <- NULL
  cacheHits <- 0
  
  setMatrix <- function(newMatrix) {
    theMatrix <<- newMatrix
    cachedSolution <<- NULL
    cacheHits <<- 0
  }
  getMatrix <- function() theMatrix
  
  setSolution <- function(solution) cachedSolution <<- solution
  
  getSolution <- function() cachedSolution
  
  cacheHit <- function() {
    cacheHits <<- cacheHits + 1
  }
  
  getHits <- function() cacheHits
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setSolution = setSolution, getSolution = getSolution,
       cacheHit = cacheHit, getHits = getHits)
}


## `cacheSolve`: Return a matrix that is the inverse of 'x'
##
## args:: *x* was created using `makeCacheMatrix`
##        The original matrix must be solveable, or else an error will raise
##
## Use the cached results if possible
cacheSolve <- function(x, ...) {
  solution <- x$getSolution()
  if(!is.null(solution)) {
    x$cacheHit()
    return(solution)
  }
  solution <- solve(x$getMatrix(), ...)
  x$setSolution(solution)
  solution
}
