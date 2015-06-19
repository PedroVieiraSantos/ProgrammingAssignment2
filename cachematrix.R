
## Returns special matrix object. The object caches its inverse.  
makeCacheMatrix <- function(x = matrix()) {
   inv_mat <- NULL
   
   get_matrix <- function(){
      x
   }
   
   set_matrix <- function(y){
      x <<- y
      inv_mat <<- NULL
   }
   
   get_inv_matrix <- function(){
      inv_mat
   }
   
   set_inv_matrix <- function(i_mat){
      inv_mat <<- i_mat
   }
   
   list(set_matrix = set_matrix,
        get_matrix = get_matrix,
        set_inv_matrix = set_inv_matrix,
        get_inv_matrix = get_inv_matrix)
}


## Takes a matrix object created by "makeCacheMatrix" and returns its inverse.
## If inverse of matrix has been calculated, the value is returned from the cache. 
cacheSolve <- function(x, ...) {
   
   inv_m <- x$get_inv_matrix()
   if (!is.null(inv_m)) {
      message("Getting cached matrix")
      return(inv_m)
   }
   
   matrix_data <- x$get_matrix()
   inv_m <- solve(matrix_data, ...)
   x$set_inv_matrix(inv_m)
   inv_m
}
