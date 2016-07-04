## My two functions are designed to first make and cache the inverse of a matrix
## and to retrive it from the cache

## This function will create a matrix and cache it using alist to set it, retrive it, set
## the inverse of it and then retrive the inverse.

makeCacheMatrix <- function(x = matrix()) {
  mnorm <- NULL
  set <- function(y) {
    x <<- y
    mnorm <<- NULL
  }
  get <- function() x
  setinverse <- function(solut) mnorm <<- solut
  getinverse <- function() mnorm
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function retrieves the inverse matrix provided by the makeCacheMatrix
## above. If it was already calculated, it returns it with the first if statement
## and it not it will do the calculation and then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mnorm <- x$getinverse()
  if(!is.null(mnorm)) {
    message("getting cached data")
    return(mnorm)
  }
  data <- x$get()
  mnorm <- solve(data)
  x$setinverse(mnorm)
  return(mnorm)
  }
