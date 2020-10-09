makeCacheMatrix  <-  function ( x= matrix()) {  # parent function
  inv <- NULL  #notes  sets an initial value for inv as null
  set <- function (y){
    x <<- y
    inv <<- NULL
  }    
  get <- function () {x}
  setInverse <- function (inverse) {inv <<- inverse}
  getInverse <- function (){inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

cacheSolve <- function(x, ... ) {   # parent function
  inv <- x$getInverse ()
  if(!is.null(inv)){            # if inv is not null
    message( "obtaining cache data")
    return (inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
} 