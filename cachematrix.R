## two functions, one create the matrix and the other first look if there is any cached
## if no it calculate the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        # first function, four functions inside and two variables
        m <- NULL
        # set the value of x and m
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get the variable object 
        get <- function() x
        # set inv value to the variable m
        setinv <- function(inv) m <<- inv
        # get the value of m
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv= getinv)
}
## it does not calculate the inv here, it only creates an special matrix


## the second function test if there is any cached value, if no it calculates the inverse
cacheSolve <- function(x, ...) {
        # brings the value of m to its own environment to check if there is any value stored
        m <- x$getinv()
        # If there is any value stored the function displays it 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #if there is no value it brings the matrix through x$get
        data <- x$get()
        # here is where the inverse is calculated and displayed
        m <- solve(data, ...)
        x$setinv(m)
        m
}
