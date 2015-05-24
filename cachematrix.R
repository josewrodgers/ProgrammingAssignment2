## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function:/Descripcion
## makeCacheMatrix is a function of a matrix x that stores the cached value. / esta funcion almacena en cache la funcion
makeCacheMatrix <- function(x = matrix()) {
####### The cache always stats  as NULL / El cache debe empezar en Nulo siempre
		ma <- NULL
####### Creates the matrix in the working environment / Creamos la matriz en el espacio actual de trabajo
        set <- function(y) {
        		x <<- y
        		ma <<- NULL
}
######## Retrieves the value of the matrix/devuelvo el valor de la matriz    
    get <- function() x
######## Calculates the inverse and stores it in cache // calculo la inversa
    
    setInvMatrix <- function(inverse) ma <<- inverse
    
######## Get the inverted matrix // me traigo la inversa de la matriz

    getInvMatrix <- function() ma
    
######## return the four functions to the working environment // me traigo en una lista la serie de 4 funciones que arme:
######## set, get, setInvMatrix y getInvMatrix
    list(set = set, get = get,
        setInvMatrix = setInvMatrix,
        getInvMatrix = getInvMatrix)
}

#################################hasta aca lo que tengo es funcion que devuelve funciones, ahora necesito meterlo en el cache

## cacheSolve calcluates the inverse of the matrix stored with makeCacheMatrix // CacheSolve es lo que me va a devolver la matriz
## dentro de la cache

cacheSolve <- function(x, ...) {
######## Get the inverse of the matrix stored in the cache // obtengo la inversa de la matriz guardada en cache	
        ma <- x$getInvMatrix()
######## If the cache is empty, it creates the matrix // aca veo si la cache viene vacia o no 
########si no esta vacia, devuelvo esos datos con la funcion return
        if(!is.null(ma)) {
                message("getting cached data/obtiendo los datos desde cache")
                return(ma)
        }
######## si viene vacia que seria el caso de la salida por el negativo, la calcula con la funcion SOLVE
######## else it calculates the inverse matrix and return it as a result
        matrix <- x$get()
        ma <- solve(matrix, ...)
        x$setInvMatrix(ma)
        return (ma)
}

