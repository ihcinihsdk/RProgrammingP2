## These functions cache an invertable matrix x,
## compute its inverse and cache its inverse in
## the variable ``cachedInv''. Once the inverse has
## been calculated and matrix x remains unchanged,
## we can always use the cached inverse when we
## need.

## Given a matrix, this function makes a special object,
## which caches the matrix itself. Once its inverse is
## calculated with the function ``cacheSolve'' the first
## time, its inverse also gets cached and is ready for use.
makeCacheMatrix <- function(x = matrix()) {
	cachedInv <- NULL
	set <- function(y) {
		x <<- y
		cachedInv <<- NULL
	}
	get <- function() x
	setInv <- function(myinv) cachedInv <<- myinv
	getInv <- function() cachedInv
	list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## This function calculates the inverse of a matrix
## `x'. If the inverse of x has already been calculated and
## x itself remains unchanged, the cached version of its
## inverse is directly returned to save computation time.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	tmp_inv <- x$getInv()
	if (!is.null(tmp_inv)) { ## Directly return the cached inverse of x, if it is calculated and up-to-date.
		message ("Use cached data")
		return (tmp_inv)
	}
	data <- x$get()
	tmp_inv <- solve(data)
	x$setInv(tmp_inv)
	tmp_inv
}
