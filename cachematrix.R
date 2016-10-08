## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getxinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data)
        message("calculating the matrix inverse")
        x$setxinv(xinv)
        xinv
}

m <- matrix(rnorm(1000000),1000,1000)
mcached <- makeCacheMatrix(m)
t1 <- system.time(m_inv1 <- cacheSolve(mcached))
print(t1)
print(m_inv1[1:5,1:5])

t2 <- system.time(m_inv2 <- cacheSolve(mcached))
print(t2)
print(m_inv2[1:5,1:5])

