

## Takes one matrix argument and holds the matrix as object "x", which is available in the parent env
## Initializes the myInv object (also available in the parent env), which will hold 
## the inverse matrix after it is calculated
## Creates a list containing functions for internal use to set the matrix, return the matrix,
## calculate the inverse, and return the inverse


makeCacheMatrix <- function(x=matrix()) {

	myInv <- NULL
	set <- function(y) {
	x <<- y
	myInv <<- NULL
	}

	get <- function() x
	setInv <- function(Inverse) myInv <<- Inverse
	getInv <- function() myInv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## Calculates the inverse of the matrix contained in 
## element 1 of the list argument, if it has
## not already been calculated and stored in "myInv"
## Returns matrix that is the inverse of x. x must be a list object
## of the type returned by makeCacheMatrix().

cacheSolve <- function(x) {
	myInv <- x$getInv()
	if(!is.null(myInv)) {
		message("getting cached data")
		return(myInv)
		}

	data <- x$get()
	myInv <- solve(data)
	x$setInv(myInv)
	myInv

}


     
		
		
		
}
