## These functions cache the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with the default mode of "matrix"
  inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) {                    ## define the set function to assign new 
    x <<- y                             ## value of matrix in parent environment
    inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                     ## define the get fucntion - returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
  ## to the functions with the $ operator
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() ## return a matrix that is inverse of x
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } ## just return the inverse if its already set
  data <- x$get() ## get matrix from our object
  inv <- solve(data, ...) ## calculate the inverse
  x$setinverse(inv) ##set the inverse to the object
  inv ##return the matrix
}

