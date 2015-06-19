
## Returns special matrix object. The object caches its inverse.  
makeCacheMatrix <- function(x = matrix()) {
   
   # when matrix object is created, inverse matrix not defined
   inv_mat <- NULL
   
   # gets matrix value
   get_matrix <- function(){
      x
   }
   
   # sets matrix value
   #  - use of "<<-" to assign variables in the parent environment (the makeCacheMatrix)
   #  - a new matrix will require a new calculation of inverse matrix 
   set_matrix <- function(y){
      x <<- y
      inv_mat <<- NULL
   }
   
   # gets matrix inverse value
   get_inv_matrix <- function(){
      inv_mat
   }
   
   # sets matrix inverse value
   #  - use of "<<-" to assign variable in the parent environment (the makeCacheMatrix)
   set_inv_matrix <- function(i_mat){
      inv_mat <<- i_mat
   }
   
   # object (an R list) returned by the function
   list(set_matrix = set_matrix,
        get_matrix = get_matrix,
        set_inv_matrix = set_inv_matrix,
        get_inv_matrix = get_inv_matrix)
}


## Takes a matrix object created by "makeCacheMatrix" and returns its inverse.
## If inverse of matrix has been calculated, the value is returned from the cache. 
cacheSolve <- function(x, ...) {
   
   # gets inverse of matrix
   inv_m <- x$get_inv_matrix()
   
   # checks if inverse has been computed. If yes, returns value in the cache
   if (!is.null(inv_m)) {
      message("Getting cached matrix")
      return(inv_m)
   }
   
   # calculate inverse of matrix
   matrix_data <- x$get_matrix()
   inv_m <- solve(matrix_data, ...)
   
   # sets inverse of matrix in the cache
   x$set_inv_matrix(inv_m)
   
   #return value (inverse of matrix)
   inv_m
}
