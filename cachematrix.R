##Assignment: Caching the Inverse of a Matrix

##Matrix inversion is usually a costly computation and their may be 
##some benefit to caching the inverse of a matrix rather than compute it 
##repeatedly (there are also alternatives to matrix inversion that we will not 
##discuss here). Your assignment is to write a pair of functions that cache
##the inverse of a matrix.

##The following functions will be used

##makeCacheMatrix: This function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(u = numeric()) {
        m <- NULL
        set <- function(y) {
                u <<- y
                m <<- NULL
        }
        get <- function() u
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve will
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}


##sample Run
## u=cbind(c(1,-1/4),c(-1/4,1))
## > a=makeCacheMatrix()
## > a$set(u)
## > a$get()
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > a$setInv(solve(u))
## > a$getInv()
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(a)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

##The above two functions can be combined in the following manner for performing the same task

makeCacheMatrix <- function(u = numeric()) {
		m <- NULL
        set <- function(y) {
                u <<- y
                m <<- NULL
        }
        get <- function() u
        setInv <- function(solve) m <<- solve

        getInv <- function(...) {
            if(!is.null(m)) {
                message("getting cached data")
                return(m)
            }
            m <<- solve(u,...)
            m
        }
        list(set = set, get = get,
        setInv = setInv,     
	  getInv = getInv)
}

## sample Run
## > u=cbind(c(1, -1/4), c(-1/4, 1))
## > a=makeCacheMatrix()
## > a$set(u)
## > a$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > a$setInv(solve(u))
## > a$getInv()
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > a$getInv()
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
