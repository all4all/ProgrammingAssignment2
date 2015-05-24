## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) { ## This function, makeCacheMatrix creates 
  ## a special "matrix" object that can cache its inverse and save us time of extra
  ## computaion.
  ## x - a square invertible matrix
  ## This returns a list of functions to
  ## 1. set the value of the matrix
  ## 2. get the value of the matrix
  ## 3. set the value of the inverse
  ## 4. get the value of the inverse
  m <- NULL ## sets the value of the variable m (the matrix inverse) to zero to let us
  ## start next calculation. This sets the default. 
  set <- function(y) { ## assigns 'set' its meaning, i.e. sets the value of the matrix
      x <<- y ## caches the input matrix and let cacheSolve check whether it has changed
      m <<- NULL ## sets the value of m to zero 
  }
  ## we use '<<-' to assign a value to our objects in an environment different from the 
  ## current environment
  get <- function() x ## gets the value of the matrix 
  setmx <- function(solve) m <<- solve ## sets the value of the inverse
  getmx <- function() m ## gets the inverse of the matrix
  list(set = set, get = get,
       setmx = setmx, 
       getmx = getmx) ## returns a list of arguments 
} 


## cacheSolve computes the inverse of the "matrix" object returned by makeCacheMatrix function
cacheSolve <- function(x, ...) { ## takes output of the makeCacheMatrix (x)
  m <- x$getmx() ## takes the inverse of the matrix from makeCacheSolve
  if(!is.null(m)){ ## checks if the inverse has already been calculated and the matrix has not
    ## changed
    message("getting cached data") ## tells that cached data will be used
    return(m) ## takes the cached data and skips the computation
  }
  ## this is 'else' that calculates the inverse
  md <- x$get()
  m <- solve(md, ...)
  x$setmx(m) ## sets the value of the inverse in the cache via setmx function
  m
}