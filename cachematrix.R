makeCacheMatrix <- function(x = matrix()) 
{

####    creating a vector-object with 4 methods - set, get, setInverse and getInverse
####    initialising inv

        inv <- NULL


####    set function sets the value of the matrix at first 
####    setting inv, the value that contains the invese of the matrix as NULL 
####    if it is called for the first time, NULL will be returned as inv value

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }


####    get function returns the value of the function


        get <- function() x


####    sets the inverse to the inverse matrix variable, inv


        setInverse <- function(invse) inv <<- invse

####    returns the value of the inverse matrix variable, inv

        getInverse <- function() inv

####    creates a list with each of the names associated with the fns

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



cacheSolve <- function(x,...) 
{

####    gets the inv from x using the getInverse function

	inv <-x$getInverse()

####    checks if inv is null ie called for first time. If not null, returns the inv
####    with a comment so that you know that it is not calculating the value

	if(!is.null(inv))
	{
                message("getting cached inverse")
                return(inv)
	}


####    if null, gets the matrix in data and solves for inverse 
	
	data<-as.matrix(x$get())
	inv <- solve(data)

####    sets inverse in x

	x$setInverse(inv)
	inv
}
