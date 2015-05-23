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



##########################################################################
##      AUthor: Eric H. Griffin 
##  Assignment: #2 
##    Function: cacheSolve
## Description: This function Calculate the inverse of the matrix, if the
##              inverse of the matrix has not already been calculated. If
##              it has been cached alredy, use the cached matrix instead
##        Date: 5/22/15
##########################################################################

cacheSolve <- function(matrx, ...)
                {
		 
                 invMatx <- matrx$getInvMatx()

                 #checking whether it is cached already
                 if (!is.null(invMatx))
                   {
        	     message("getting cached matrix")
        	     return (invMatx)
                   }

                 #else not previously cached so calculate
                 data <- matrx$getMatx()
                 inv <- solve(data, ...)

                 # save result
                 matrx$setInvMatx(inv) 
                 inv
                }