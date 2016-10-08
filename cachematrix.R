## The cacheSolve function calculates the inverse of a "cache" matrix. 
## For a cache matrix, if the inverse has never been calculated before, its 
## inverse is then calculated inside cacheSolve with a call to the solve() 
## function, and the result is saved (cached) in the enviroment of function 
## makeCacheMatrix.
## For any future inverse of the same cache matrix, the saved inverse is
## returned and solve() is never called again. 

## The makeCacheMatrix function takes a R matrix object as the argument and 
## return a list of 4 functions to represent the same matrix as a "cache"
## matrix

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<-y
                xinv <<- NULL
        }
        get <- function() x
        setxinv <- function(xi) xinv <<-xi
        getxinv <- function() xinv
        list(set = set, get = get,
             setxinv = setxinv,
             getxinv = getxinv)
}


## The cacheSolve function calculates the inverse of the cache matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getxinv()
        if(!is.null(xinv)) {
                message("getting cached data -- faster")
                return(xinv)
        }
        data <- x$get()
        message("calculating the matrix inverse by calling solve()")
        xinv <- solve(data)
        x$setxinv(xinv)
        xinv
}


## Test of the functions

m <- matrix(rnorm(1000000),1000,1000)
mcached <- makeCacheMatrix(m)
t1 <- system.time(m_inv1 <- cacheSolve(mcached))
print(t1)
print(m_inv1[1:5,1:5])

t2 <- system.time(m_inv2 <- cacheSolve(mcached))
print(t2) 
print(m_inv2[1:5,1:5])

