## Below is a function that takes a matrix and output a list of 4 functions
## One of the 4 functions of the list get() carries the matrix data
## We can replace the old matrixcreated at the time of creation of list with new marix using set() function
## We can cache the inverse using setInverse function
## We can retrieve the cached inverse using getInverse() function

makeCacheMatrix <- function(x = matrix()) {
  
  I<-NULL               ## taking I as the inverse matrix
  
  set<-function(y)      ## taking new matrix to replace old one
  {
    x <<- y             ## replacing old matrix
    I <<- NULL          ## reseting the cached Inverse(if available) to ull as a new matrix is taken as input
  }
  
  get <- function() x    ## x is a free variable to function get & hence x 
                         ## take the value available for it in makeCacheMatrix envir
  
  setInverse <- function(Inverse) I <<- Inverse  ## function to cache the Inverse
  
  getInverse <- function() I  ## function to retrieve the cached inverse
  
  list(set = set,             ## making a list with 4 functions created
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Below function first checks weather any cached Inverse is available.
## If cached invers is available it returns the cached Inverse with a message
## If cached Inverse is not available, Inverse is calculated and stored.


cacheSolve <- function(x, ...) {
  
  I <- x$getInverse()   ## getting catched inverse
  
  if(!is.null(I))       ## checking if there is a cached inverse
  {
    message("getting cached Inverse")
    return(I)           ## returning the already available cache
  }
  
  data <- x$get()       ## retrieving the matrix if there is no existing cache
  I <- solve(data)      ## calculating inverse
  
  x$setInverse(I)       ## making cache of the newly calculated inverse
  I                     ## returninng the newly calculated inverse
}
