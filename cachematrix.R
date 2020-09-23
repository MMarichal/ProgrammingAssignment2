## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

akeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        getInverse <- function(){
                inv
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

#Run matrix
pmatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
pmatrix$get()

#Run inverse matrix
pmatrix$getInverse()
cacheSolve(pmatrix)

#Run matrix cache
cacheSolve(pmatrix)

#Run inverse of matrix cache
pmatrix$getInverse()
