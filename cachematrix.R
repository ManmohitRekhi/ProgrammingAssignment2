## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This is a function that makes a chached matrix, it can set a matrix in x
#And set the inverse in m. It returns a list containg of
#set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) 
{
	#The variable m which conatins the inverse is NULL
	m <- NULL
	
	#This is the set function, it sets the value of matrix x to the matrix y and also m(inverse) to null 
	set <- function(y)
	{
		x <<- y
		m <<- NULL
	}
	
	#This function return the orignal matrix
	get <- function() x
	
	#This sets the valriable m to the inverse of the matrix
	setinverse <- function(solve) m <<- solve
	
	#This function returns the inverse stored in m
	getinverse <- function() m
	
	#This creates a list of (set, get, setinverse, getinverse)
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Write a short comment describing this function
#This is a function which checks whether the inverse is cashed or not
#If it is cached, it returns the inverse
#Else it calculates it and then caches it

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
	#Here we set the variable m with the inverse
	m <- x$getinverse()
	
	#This cchecks if the m is NULL or not.
	#If it is not null, means that the inverse is cached
	#Else it needs to calculate the inverse
	if(!is.null(m))
	{
		message("getting cached data")
		return(m)
	}
	
	#The inverse is calculated and cached
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	
	#This is the inverse m
	m
}
