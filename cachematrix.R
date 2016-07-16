## Code for caching inverse of a matrix. 

## Creates a matrix which can be cached. It has 4 functions. 1. setMatrix 2. getMatrix 3. setInverse 4. getInverse

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL

	
	setMatrix<-function(mat){
		x<<-(mat)
		inv<<-NULL
	}

	getMatrix<-function() x
	
	setInverse<-function(inver){
		inv<<-inver
	}
	
	getInverse<-function() inv
	
	list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
}


## gets inverse of matrix. Checks if the inverse is already calculated. if not, then calculates the inverse and stores it into
## the parent variable

cacheSolve <- function(x, ...) {
	inver<-x$getInverse()
	
	if(!is.null(inver)){
		message("getting inverse from cache")
		return (inver)
	}
	
	inver<-solve(x$getMatrix())
	x$setInverse(inver)
	inver
	
}
