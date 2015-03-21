## There are two functions in this routine
##    1. makeCacheMatrix - Cache a matrix that is passed in.
##    2. cacheSolve      - Invert athe cached matrix made by makeCacheMatrix

## make_CacheMatrix
##
##   Cache the matrix that is passed in.
##     set() - This the routine that caches the matrix. It can be
##             implicitly or explicitly called.
##     get() - Get the cached matrix

makeCacheMatrix <- function(x = matrix()) {
	
	# get method to cache the matrix

	set <- function(y) {
	    x <<- y
	}

	# get method to retrieve the value
	
	get <- function() x

	# display the methods when makeCacheMatrix is called
	
	list (set = set, get = get)
}


## cacheSolve
##
## There are not any separate methods as part of cacheSolve. cacheSolve will
## return the inverse of the new "matrix" object
##

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'cached_matrix'

	## Make sure that the inv_mat value exists. It won't be the first
	## time. 

	if (exists("inv_mat")) {
	   if (!is.null(inv_mat)) {
	      orig_mat <<- x$get()

	      ## I check to see ivf the passed in matrix object is the
	      ## same as the matrix object that created the cached
	      ## matrix. If it is the same, then you can return inv_mat
	      ## Otherwise you will have to calculate  inv_mat again.

	      if (exists("new_mat") && new_mat == orig_mat) { 
	      	 message("getting cached inverted data")	    
	      	 return(inv_mat)
	      }		 
	   }
	}

	# Get the matrix from the cached value  and get the inverse of it
	
	new_mat <<- x$get()
	inv_mat <<- solve(new_mat)
}
