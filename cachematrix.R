##### Exercise week 3 - Coursera Data Science: R Programming
# Matrix function (and it inverse) stored in cache memory
# Goal: optimize computing time to get the inverse matrix result 

# makeCacheMatrix make an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
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


## "cacheSolve" make the inverse of makeCacheMatrix function

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
