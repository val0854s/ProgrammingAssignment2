## makeCacheMatrix: Stores different functions within a list. If called on a matrix, 
# set(), redefine the variable with a new matrix
# get(), allows you to get the matrix data
# set_inverse(), caches the inversed matrix
# get_inverse(), gets the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      set_inverse <- function(inversed_matrix) m <<- inversed_matrix
      get_inverse <- function() m
      list(set = set, get = get, set_inverse = set_inverse, 
           get_inverse = get_inverse)
}


## cacheSolve: X needs to be the result of makeCacheMatrix. 
# This function will first check, weather the inverse of the matrix has already been stored inside x. If thats the case, this inversed matrix will be returned.
# If not, than it will get the uninversed matrix, that has been stored within x, using the get function, and inverse it, using solve(). 
# Afterwards, it will cache the inversed matrix within x, by setting it, using the set_inverse() function.
# The set() function could be removed, since it isn't used for this particular case. However, you can manually change the original matrix stored behind x, if you need to.

cacheSolve <- function(x, ...) {
      m <- x$get_inverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$set_inverse(m)
      m
      ## Return a matrix that is the inverse of 'x'
}

my_Matrix <- matrix(1:4, 2, 2)
x <- makeCacheMatrix(my_Matrix)
cacheSolve(x)
cacheSolve(x)

