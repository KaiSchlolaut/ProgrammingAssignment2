## The function "makeCacheMatrix" takes a matrix as input, shows it or sets it new; also takes the inverse and shows it.
## The cacheSolve function calculates the inverse (if not already existing) and assignes it back to the input.


## This function takes a matrix as input, and makes a list with 4 variables where the inputed matrix is shown, set new, the inverse from "outside" is taken as input or also set new

makeCacheMatrix <- function(x = matrix()) {		# matrix as input parameter 
        m <- NULL 					# assign NULL to variable m
        set <- function(y) {				# function "set" with input parameter y 
                x <<- y					# super-assign variable x value of y
                m <<- NULL				# super assign NULL to variable m (a new matrix also gets a new inverse)
        }
        get <- function() x				# function "get" means call x 
        setinverse <- function(inverse) m <<- inverse	# super-assign m value of inverse which is input parameter of function "setinverse"
        getinverse <- function() m			# function "getinverse" means call m 
        list(set = set, get = get,			# list with 4 variables: 
             setinverse = setinverse,
             getinverse = getinverse)
}



## Take from a matrix x (which is a list) the inverse and return it if it is set, otherwise take the matrix, calculate the inverse, set it to the "matrix-list" and show it


cacheSolve <- function(x, ...) {			# x as input parameter 
        m <- x$getinverse()				# assign "list-variable" "getinverse" from input x
        if(!is.null(m)) {				# if m has a value it's "chached" and return m
                message("getting cached data")
                return(m)
        }
        data <- x$get()					# local variable "data" is assigned the "list-variable" "get"
        m <- solve(data, ...)				# inverse is calculated and assigned to m
        x$setinverse(m)					# "list-variable" "setinverse" is assigned 
        m						# m is shown
}