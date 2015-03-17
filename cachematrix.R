## this function builds the CacheMatrix "class"
## it holds both original matrix and inverse matrix as members
## exporting get/set functions for accessing them.
## When new different matrix is added then previous 
## inverse matrix will be deleted
makeCacheMatrix <- function(m_matrix = matrix()) {
  m_inverseMatrix <- NULL #Holds inverse matrix
  
  # sets a new matrix into the cache
  set <- function(matrix) {
    # Check if new matrix is similar to matrix in m
    # This is to avoid unneaded calculations
    if(identical(x,matrix)){
      #do nothing
      return
    }
    m_matrix <<- matrix
    #clear the inverse matrix after setting a new matrix
    m_inverseMatrix <<- NULL
  }
  
  # gets matrix from Cache
  get <- function() m_matrix
  
  # sets new inverse matrix into the Cache
  setinv <- function(inv) m_inverseMatrix <<- inv
  
  # Gets cache inverse matrix if exists
  getinv <- function() m_inverseMatrix
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## cacheSolve - calculates an inverse of a matrix. It accepts CacheMatrix objects
## which if already have the solution in cache then just returns it otherwise 
## calculates the inverse and stores the result in the cache 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
  # retrieves the inverse matrix in the cache
  m <- x$getinv()
  # checks if inverse already exists
  if(!is.null(m) ) {
    #already exists just return it without calculating
    message("getting cached data")
    return(m)
  }
  # nothing in cache so get the original matrix
  data <- x$get()
  # calculate the inverse
  m <- solve(data, ...)
  # set inverse inside the cache
  x$setinv(m)
  #return the inverse matrix
  m
}
