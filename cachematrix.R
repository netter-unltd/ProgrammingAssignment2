## This file contains functions for calculating inverse of a square matrix.


## Function 'makeCacheMatrix' creates a list of objects which is to be used
## as an input for cacheSolve() function.
## The produced list has the following structure:
## ($set(), $returnValue(), $setInverse(), $getInverse)
##
## $set(): a function for initialising data.
### Example:
### > some_vector <- makeCacheMatrix(NULL)
##### creates some_vector; variable 'x' inside the parent function (makeCacheMatrix)
##### is set to NULL.
### > some_vector$set(some_matrix)
##### assigns variable 'x' to the value of some_matrix. However, if x was already
##### assigned same value, i.e. x == some_matrix, a message
##### "Data seems to be redundant" is produced.
## $returnValue(): a function that returns variable 'x'
## $setInverse(): a function for storing calculated inverse of a matrix 'x'.
##### This function assigns its input to variable 'CachedMatrix'
## $getInverse(): this function returns variable 'CachedMatrix'

makeCacheMatrix <- function(x = matrix()) {
		CachedMatrix <- NULL
		set <- function(y){
			if(all.equal(x,y)==TRUE){
				message("Data seems to be redundant")
			}
			else {
				x <<- y
				CachedMatrix <<- NULL
			}
		}
		returnValue <- function() x
		setInverse <- function(inverse) CachedMatrix <<- inverse
		getInverse <- function() CachedMatrix
		list(set = set, returnValue = returnValue,
			 setInverse = setInverse,
			 getInverse = getInverse)
}


## Function cacheSolve takes as an argument a list produced by a function
## 'makeCacheMatrix()'. It then calls argument's function '$getInverse()'
## and, based on its output, either returns it (if the output was not 'NULL'),
## or it calculates an inverse of a matrix that is being stored in argument's
## variable 'x' (returned via call to $returnValue()), then rewrites argument's
## variable 'CachedMatrix' (via call to $setInverse()) and then returns an inverse
## of the matrix.

cacheSolve <- function(x, ...) {
        CachedMatrix <- x$getInverse()
		if(!is.null(CachedMatrix)){
			message("getting cached data")
			return(CachedMatrix)
		}
		data <- x$returnValue()
		CachedMatrix <- solve(data,...)
		x$setInverse(CachedMatrix)
		CachedMatrix
}
