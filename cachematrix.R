## Functions makeCacheMatrix and cacheSolve take the inverse of a matrix 
## and store it for future use so that the inverse does not have to be
## calculated multiple times.

## The function makeCacheMatrix is a list of functions that allows a matrix
## and its inverse to be stored.

makeCacheMatrix <- function(x = matrix()) {
	m <-NULL
	set <-function(y){
		x <<-y
		m <<-NULL
	}
	get <-function()x
	setinverse <-function(solve)m<<-solve
	getinverse <-function()m
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function cacheSolve takes a matrix and looks to see if the inverse
## has already been calculated and stored. If so, it returns the stored 
## value. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
      m<-x$getinverse()
	if (!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data, ...)
	x$setinverse(m)
	m
}
