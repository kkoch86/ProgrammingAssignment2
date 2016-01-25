## The first function makes a list with methods that set and get a matrix and its inverse in an intrinsic environment variable

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL ## initialize inverse
  
  ## set x in parent env with the desired value, if inverse is already set, get rid of it!
  set <- function(userValue = matrix()) {
    x <<- userValue 
    cachedInv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## given the list variable from the first function, will first check to see if there's already a cached inverse and return
## otherwise will attempt to solve its inverse and set/return it

cacheSolve <- function(x = matrix(), ...) {
  
  ## let's see if there's something there already
  calculatedInverse <- x$getInverse() 
  
  ##check if there's a cached value AND it's a matrix
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    message("Returned Catched Data")
    return(calculatedInverse)
  }
  
  ## otherwise get the matrix
  matrixToSolve <- x$get()  
  
  ## try to solve the matrix and catch errors and warnings
  calculatedInverse <- solve(matrixToSolve)
  message("Setting the value of inverse to:") 
  x$setInverse(calculatedInverse)
}