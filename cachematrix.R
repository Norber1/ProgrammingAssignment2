## The functions below calculate (if it is not calculated and cahced yet)  and cache the inverse of a given matrix


## The first function creates a list containing the function to
## 1 set the value of a matrix
## 2 get the value of a matrix
## 3 set the value of an invertible matrix's inverse
## 4 get the value of an invertible matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL                 ## Set 's' to be empty 
  set <- function(y) {      ## The function will set the value of a matrix and store it in 'set'     
    x <<- y
    s <<- NULL
  }
  get <- function() x       ## The above function will be called with parameter 'x' and will be stored in 'get'
  setsolve <- function(solve) s <<- solve  ## 'setsolve' will call the solve function (calculates the inverse of an invertable matrix) and 's' is equeal to 'solve' 
  getsolve <- function() s               ## 'getsolve' will call the solve function
  list(set = set, get = get,            ## The above mentioned four new functions are stored in a list
       setsolve = setsolve,
       getsolve = getsolve)
}


## The second function creates/retrieves a matrix's inverse, but first checks if the inverse is already cahced (in this case it doesnt calculate, but retrieves from the cache)

cacheSolve <- function(x, ...) {
  s <- x$getsolve()                ## try to get the cached inverse of matrix 'x' 
  if(!is.null(s)) {                ## we check if we already have the inverse cached  
    message("getting cached data") ## we send a message, that we got the cached data
    return(s)                      ## and return the cached inverse
  }
  matrix <- x$get()                  ## we get 'x' from the list and put it to 'matrix'
  s <- solve(matrix, ...)            ## we calculate the inverse of 'matrix' and store it to 's'
  x$setsolve(s)                      ## we set 'setsolve' with the 
  s       
}
