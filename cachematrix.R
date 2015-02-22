# Write an R function that is able to cache potentially time-consuming computations. 
#Take advantage of the scoping rules of the R language and how they can be manipulated 
#to preserve state inside of an R object.

#       Example: Caching the Mean of a Vector

#       In this example we introduce the <<- operator which can be used to assign a value 
#       to an object in an environment that is different from the current environment. 
#       Below are two functions that are used to create a special object that stores a numeric 
#       vector and caches its mean.

#       The first function, makeVector creates a special "vector", which is really a list containing a function to

#       a) set the value of the vector
#       b) get the value of the vector
#       c) set the value of the mean
#       d) get the value of the mean

#makeVector <- function(x = numeric()) {
#        m <- NULL
#        set <- function(y) {
#                x <<- y
#                m <<- NULL
#        }
#        get <- function() x
#        setmean <- function(mean) m <<- mean
#        getmean <- function() m
#        list(set = set, get = get,
#             setmean = setmean,
#             getmean = getmean)
#}

#       The following function calculates the mean of the special "vector" created with the above function. 
#       However, it first checks to see if the mean has already been calculated. If so, it gets the mean 
#       from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets 
#       the value of the mean in the cache via the setmean function.

#cachemean <- function(x, ...) {
#        m <- x$getmean()
#        if(!is.null(m)) {
#                message("getting cached data")
#                return(m)
#        }
#        data <- x$get()
#        m <- mean(data, ...)
#        x$setmean(m)
#        m
#}

makeCacheMatrix <- function(x = matrix()) {
        ## create initial matix with value of NULL
        final_matrix <- NULL
        ## create funtion that creates the first occurance of the matrix or updates the matrix if a change occurs
        set <- function(y) {
                x <<- y
                final_matrix <<- NULL
        }
        ## get/ calculate the inverse of he matirx and pass the value of the function
        get <- function() x
        setinverse <- function(inverse) final_matrix <<- inverse
        getinverse <- function() final_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## get the cache required
cacheSolve <- function(x, ...) {
        final_matrix <- x$getinverse()
        ## if the cache exists retrieve it
        if(!is.null(final_matrix)) {
                message("getting cached data")
                return(final_matrix)
        }
        ## if the inverse doesn't exist calculate it
        data <- x$get()
        final_matrix <- solve(data)
        x$setinverse(final_matrix)
        final_matrix
}
