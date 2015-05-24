#functions cache the inverse of an invertible matrix
#such that complicated computations do not have to be performed repeatedly
#instead the value of inverse is already calculaed and R will refer to 
#the alreday computed value of inverse potentially reducing computation time

#This function stores a list of functions to set and get the inverse of the matrix
#It creates an object that can cache the inverse of a given matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}

#This function computes the value of inverse of a matrix returned by makeCacheMatrix
#It will retrieve the inverse from makeCacheMatrix if it is already calcukated

cacheSolve<-function(x,...)
{
	inv=x$getinv()
	if(!is.null(inv))	
	{ 
	 message("Getting cached data")
	 return(inv)	
	}
	data=x$get()
	inv=solve(data)  
	x$setinv(inv)
	return(inv)
}