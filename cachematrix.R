## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inversed matrix
# get the value of the inversed matrix


makeCacheMatrix <- function(mat = matrix()) {
        invmat <- NULL
        set <- function(y) {
                matr <<- y
                invmat <<- NULL
        }
        get <- function() mat
        setinverse <- function(inv) invmat <<- inv
        getinverse <- function() invmat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the 
## special "matrix" created with the above function. 
## However, it first checks to see if the inversedMatrix has already been 
## calculated. 
## If so, it gets the inverted matrix from the cache and skips the computation. 
## Otherwise, it calculates the inversedMatrix of the originalMatrix and sets the value of the 
## inversedMatrix in the cache via the setmean function.

cacheSolve <- function(cachedMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversedMatrix <- cachedMatrix$getinverse()
        if(!is.null(inversedMatrix)) {
                message("getting cached data")
                return(inversedMatrix)
        }
        orginialMatrix <- cachedMatrix$get()
        inversedMatrix <- solve(orginialMatrix, ...)
        cachedMatrix$setinverse(inversedMatrix)
        inversedMatrix
}
