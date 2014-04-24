## Put comments here that give an overall description of what your
## functions do

## the makeCacheMatrix function creates an object which contains a
## matrix, the inverse (if it is known), and methods to get and set the matrix
## and to get and set the inverse. 


makeCacheMatrix <- function(x = matrix()) {

	
	inverse <-NULL

	set<-function(y){
		x<<-y
		inverse<<-NULL

	}
	
	get<-function() x
	
	setinv <- function(inv) inverse <<- inv
	
	getinv <- function() inverse

	list(set = set, get = get, setinv = setinv, getinv = getinv)

}



## Returns the inverse of the cacheMatrix object. Returns the cached value 
## if it is known, else the value is calculated, cached and then returned.

cacheSolve <- function(x, ...) {
       
	inv <- x$getinv()

	if( is.null(inv)){
		
		x$setinv(solve(x$get(),...))
		inv <- x$getinv()
	}
	else{
		message("getting cached data")
	}
	
	inv

}
