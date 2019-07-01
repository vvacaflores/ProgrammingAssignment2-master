## Functions acording to example given.
## 
## makeCacheMatrix <- function(x = matrix()) { }

makeCacheMatrix <- function(x = matrix()) {
        ## Creation of object
         m_inv <- NULL
         set <- function(y) {
                 x <<- y
                 m_inv <<- NULL
         }
         get <- function() x
         setInverse <- function(inver) m_inv <<- inver
         getInverse <- function() m_inv
         list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}
##
## cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'}

cacheSolve <- function(x, ...) {
         ## This function return a matrix that is the inverse of 'x'
         m_inv <- x$getInverse()
         if (!is.null(m_inv)) {
                 message("getting cached data")
                 return(m_inv)
         }
         matrix <- x$get()
         m_inv <- solve(matrix, ...)
         x$setInverse(m_inv)
         m_inv
}