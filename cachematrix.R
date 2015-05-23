## These functions compute the matrix inverse storing it in cache so that 
## next reference to the  matrix inverse will get it from cache rather than
## recomputing the value again



##########################################################################
##      AUthor: Eric H. Griffin 
##  Assignment: #2 
##    Function: makeCacheMatrix
## Description: This function creates a special "matrix" object 
##              that can cache it inverse
##        Date: 5/21/15
##########################################################################

makeCacheMatrix <- function(matx = matrix()) 
                     {
	
	              invMatx <- NULL 
	              setMatx <- function(maty)
                                    {
		                      matx <<- maty
		                      invMatx <<- NULL
	                            }
	
	              getMatx <- function() matx
	              setInvMatx <- function(inverseMatx) invMatx <<- inverseMatx
	              getInvMatx <- function() invMatx
	
	              list(setMatx = setMatx, getMatx=getMatx,
		                     setInvMatx = setInvMatx,
		                     getInvMatx = getInvMatx)
                     }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
