## In order to save the time and energy it may take to calculate the inverse
## of a given matrix, the following function pair use lexical scoping to cache
## the inverse of a matrix and output the inverse value without calculating it
## again if it is not the first time calling for the action.

## makeCacheMatrix function takes in a matrix that is defaulted to be empty, 
## assigning NULL to its inverse, uses sub-function set to set a new value of 
## both the matrix and its inversevia the <<- sign to assign parameters to its 
## parent environment ("makeCacheMatrix()"), get function to get the new matrix,
## getinv function to get the new inverse. Finally the function returns a list
## with these four subfunction with named indices.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() {inv}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function calls the get function in makeCacheMatrix function. If 
## inv turns out to not be NULL, it print the inverse matrix directly out of 
## cache memory. If it is not the case, it calls get sub-function to retrieve
## the new matrix, use solve() to compute the inverse and put it back to inv
## and calls setinv to store the data back to makeCacheMatrix. Finally, it print
## the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

