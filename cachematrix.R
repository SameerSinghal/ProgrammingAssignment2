## Two functions defined here
## makeCacheMatrix function defines a list of 4 functions to set, get matrices and also setInverse and getInverse of the matrices
## makeCacheMatrix function sets the matrix using a "super assignment" operator doing the assignment in the global environment
## makeCacheMatrix defines getInverse function using solve to compute martix inverse using super assignment operator

## cacheSove function returns the inverse of the passed matrix either from cached global variable if it exists there and if not then computes the
## inverse


makeCacheMatrix <- function(mtx = matrix()) {
        
	inv <- NULL

        setMatrix <- function(y) {
               mtx <<- y						## sets value in global cache
                inv <<- NULL						
       	}

        getMatrix <- function() mtx
	
	setInverse <- function(inverse) inv <<- solve(mtx)               ## Use sovle() to compute inverse

        getInverse <- function() inv <<- solve(mtx)

        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse,
	     getInverse = getInverse)
}


cacheSolve <- function(x, ...) {

        m <- x$getInverse()   

        if(!is.null(m)) {
                message("getting cached data")   ## if inverse exists then retrieved from global cache saving time
                return(m)
        }

        inverse <- x$getMatrix()
        m <- solve(inverse, ...)                 ## if inverse does not exist then computed using the solve() function
        x$setMatrix(m)
        m

}
