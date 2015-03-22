# This function is a collection of two main functions: makeCacheMatrix & cacheSolve
# It also defines the subfunctions set, get, set_inverse and get_inverse
# such that they never do not take values but can be accessed from outside
#environments.
# cachematrix computes a matrix's inverse (InvMatrix) and caches it for later 
#usage
#------------------------------------------------------------------------------#

# makeCacheMatrix caches an InvMatrix computed by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL                       #init InvMatrix, later 
                                                #stores computed InvMatrix
        set <- function(y) {                    #sets input matrix
                x <<- y
                inverse_x <<- NULL               #init InvMatrix
        }
        get <- function() x                      #accesses input matrix
        set_inverse <- function(inverse) inverse_x <<- inverse # sets InvMatrix
        get_inverse <- function() inverse_x                    # gets InvMatrix      
        list(set = set, get = get,      #returns a list of these functions
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


#Returns x's InvMatrix
cacheSolve <- function(x, ...) {
        m <- x$get_inverse()            #calling cache
        if(!is.null(m)) {               #checking if cache is empty
                message("getting cached data")#if not, lets us know
                return(m)                     #and returns the cached matrix
        }
        data <- x$get()                  #otherwise takes the input matrix
        m <- solve(data, ...)            #computes the inverse
        x$set_inverse(m)                 #mounts the InvMatrix in the cache
        m                                #and prints the InvMatrix
}
