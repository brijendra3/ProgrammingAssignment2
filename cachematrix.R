##Assignment: Caching the Inverse of a Matrix

##Matrix inversion is usually a costly computation and their may be 
##some benefit to caching the inverse of a matrix rather than compute it 
##repeatedly (there are also alternatives to matrix inversion that we will not 
##discuss here). Your assignment is to write a pair of functions that cache
##the inverse of a matrix.

##The following function will be used

##makeCacheMatrix: This function creates a special "matrix" object that can 
##cache its inverse. This function also computes the inverse of the 
##special "matrix". If the inverse has already been 
##calculated (and the matrix has not changed), then it will
##retrieve the inverse from the cache.

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
