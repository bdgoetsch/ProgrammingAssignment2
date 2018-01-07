
## The following function creates a list in a new environment containing 4 functions (set, get, setsolve, getsolve).
## First, x and m are assigned as the input argument and NULL in the parent environment. 
## x is a matrix, and get(x) retrieves x from the parent environment. setsInverse() calculates the inverse of the matrix
## and stores it in m. getsolve retrieves this inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve first retrieves the inverse of x and stores it in m. It then checks to see if m is equal to null -- if not,
## then the function returns m, the inverse of x. If m is equal to null, then the function retrives x using the get()
## function and stores it in data. It then uses the solve() function to solve for the inverse of x and store it in m.
## The function then sets m as the inverse of x and returns m. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
