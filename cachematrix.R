## makeCacheMatrix will find the inverse of a matrices that is input and then save it for
## later use.  Therefore, the inverse of a specific matrix doesn't need to be found again
## and again.  

## m and x are assigned in the parent environment.  The set, get, setmatrix and getmatrix 
## functions are defined.  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y          ## x is assigned the matrix that is passed
    m <<- NULL       ## m is assigned NULL
  }
  get <- function() x                          ## returns the matrix x
  setmatrix <- function(solve) m <<- solve(x)  ## returns the inverse matrix of x
  getmatrix <- function() m                    ## returns the matrix m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve will check if we have already found the inverse of matrix x.  If so it will 
## return that matrix.  If not, it will find the inverse matrix of X, set that matrix to m.  
## cacheSolve will return the matrix m.

cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix() ## Gets the matrix stored as m or NULL
  
  if(!is.null(m)){   ## Checking if m has a matrix stored there
    message("getting cached matrix")
    return(m)        ## If m is not NULL, returns m
  }
  given_matrix <- x$get()      ## If m was NULL from above, gets matrix x
  m <- solve(given_matrix,...) ## Finds the inverse of matrix x
  x$setmatrix(m)               ## Sets the inverse of matrix x to m
  m                            ## returns m
        
}

