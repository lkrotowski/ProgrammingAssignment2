## This module consists of two functions used for optimizing matrix inversion.
## Optimization is done using caching inverted matrix so two consecutive
## inversions will, in fact, only execute expensive solve() only once.

## Example use (second invocation of cacheSolve() uses cached data and set()
## clears cache):
## > m <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
## > m$get()
##            [,1]        [,2]       [,3]
## [1,] -0.9696007 -0.06085835 -0.1990363
## [2,]  0.7854633  1.22633029 -0.2622124
## [3,]  0.9477693 -1.25206000 -0.2501930
## > cacheSolve(m)
##             [,1]       [,2]       [,3]
## [1,] -0.60715949  0.2236767  0.2485921
## [2,] -0.04970987  0.4122402 -0.3924988
## [3,] -2.05124621 -1.2156843 -1.0909980
## > cacheSolve(m)
## getting cached data
##             [,1]       [,2]       [,3]
## [1,] -0.60715949  0.2236767  0.2485921
## [2,] -0.04970987  0.4122402 -0.3924988
## [3,] -2.05124621 -1.2156843 -1.0909980
## > m$set(matrix(rnorm(9), 3, 3))
## > cacheSolve(m)
##             [,1]     [,2]        [,3]
## [1,] -0.02968293 1.084872 -0.69776274
## [2,]  0.54497130 1.289198 -0.30262746
## [3,] -3.00452803 4.343065 -0.09247003
## > cacheSolve(m)
## getting cached data
##             [,1]     [,2]        [,3]
## [1,] -0.02968293 1.084872 -0.69776274
## [2,]  0.54497130 1.289198 -0.30262746
## [3,] -3.00452803 4.343065 -0.09247003


## makeCacheMatrix() returns list object containing matrix passed as x
## parameter, cache (initially NULL) and functions used for accessing both
## values.
makeCacheMatrix <- function(x = matrix()) {
	c <- NULL
	## Take note that setting new matrix clears cache as it was
    ## possibly filled with inversion of previous matrix.
	set <- function(y) {
		x <<- y
		c <<- NULL
	}
	get <- function() x
	setcache <- function(new) c <<- new
	getcache <- function() c

	list(set = set, get = get, setcache = setcache, getcache = getcache)
}


## cacheSolve() uses object created by makeCacheMatrix() and computes it's
## inversion. If this object contains cache it is used instead of solve().
cacheSolve <- function(x, ...) {
	c <- x$getcache()
	if (!is.null(c)) {
		message("getting cached data")
		return(c)
	}
	data <- x$get()
	c <- solve(data, ...)
	x$setcache(c)
	c
}
