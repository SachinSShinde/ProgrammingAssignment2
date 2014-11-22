## Goal of below functions is to cache the inverse of matrix

## makeCacheMatrix creates Inverse of matrix and it creates list of functions 
# which can be used to fetch inverse which is cached i.e. already created

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                 ## Assigns NULL value to m
  set <- function(y){                       ## set function sets value
    x <<- y                                 ## assigns value of Y to x
    m <<- NULL                              ## assigns NULL value to m
  }
  get <- function() x                       ## gets value of x
  setInv <- function(solve) m<<-solve       ## sets inverse of matrix 
  getInv <- function() m                    ## gets inverse of matrix 
  list(set=set,                             ## it is list with named values where the values are the functions
       get=get,
       setInv=setInv,
       getInv=getInv)
}


## cacheSolve takes square matrix as an argument gives inverse of that matrix
## it uses above function to check if inverse is already present
## if inverse is present then it gets it from cache
## if inverse is not present it creates it using solve() function 

cacheSolve <- function(x, ...) {
  m <- x$getInv()                    ## gets inverse of an matrix
  if(!is.null(m)){                   ## if inverse of matrix is present then it fetches from cache and return inverse matrix
      message("getting cached data") ## message to confirm inverse matrix is cached
      return(m)                      ## returns inverse of matrix
  }
  data <- x$get()                    ## if inverse of matrix not present then it gets original matrix
  m <- solve(data, ...)              ## creates inverse of it using solve() function
  x$setInv(m)                        ## It sets inverse of matrix to feetch next time
  return(m)                          ## returns inverse of matrix
}
