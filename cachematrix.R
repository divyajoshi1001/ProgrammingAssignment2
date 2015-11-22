  makeCacheMatrix
function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
cachesolve <- function(x, ...) {
 inv <- x$getInverse()
         if(!is.null(inv)) {
	     	    message("getting cached data")
                return(inv)
         }
         mat <- x$get()
         inv <- solve(mat, ...)
         x$setInverse(inv)
         inv
 }
## To Test:

my_matrix$get()
my_matrix$getInverse()
cachesolve(my_matrix)
my_matrix$getInverse()
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))