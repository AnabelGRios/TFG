# Cargamos las dos librerías necesarias para hacer las pruebas
library(bmp)
library(gdpc)

# Función para hacer las pruebas. En ella se eligen los parámetros con los que se va a realizar la prueba indicada en el
# parámetro tipo, que puede ser con series generadas con un método de Monte Carlo, generadas aleatoriamente o
# utilizando como series las columnas de una imagen.
hacerPruebas <- function(m = c(32,256,512), T = c(32,256,512), k_max = c(3,7,10), expl_var = c(0.6,0.8,0.95), ncores = c(1,2,4), tipo = "MonteCarlo", semilla = 31415) {
	if (tipo != "MonteCarlo" && tipo != "Aleatorio" && tipo != "Imagen") {
		print("El tipo debe ser uno entre MonteCarlo, Aleatorio e Imagen")
	}
	else {
		# Creamos el grid con los parámetros pasados por argumento y se lo pasamos al método concreto.
		grid <- expand.grid(ncores = ncores, expl_var = expl_var, k_max = k_max, T = T, m = m)
		if (tipo == "MonteCarlo") {
			hacerPruebasMC(grid, semilla)
		}
		else if (tipo == "Aleatorio") {
			hacerPruebasA(grid, semilla)
		}
		else {
			hacerPruebasI(grid)
		}
	}
}

# Función para hacer las pruebas con series generadas por un método de Monte Carlo. Cogemos cada fila del grid, que
# serán los parámetros concretos, y generamos la prueba con esos parámetros. Posteriormente escribe el tiempo
# que ha tardado en hacer la prueba en un fichero .csv
hacerPruebasMC <- function(grid, semilla) {
	apply(grid, 1, function(x){
		t <- getTiempoMC(x, semilla)
		write(t, "salidaTiempoMC.csv", append=TRUE)
	})
}


# Función para hacer las pruebas con series generadas con números aleatorios. Cogemos cada fila del grid, que
# serán los parámetros concretos, y generamos la prueba con esos parámetros. Posteriormente escribe el tiempo
# que ha tardado en hacer la prueba en un fichero .csv
hacerPruebasA <- function(grid, semilla) {
	apply(grid, 1, function(x){
		t <- getTiempoA(x, semilla)
		write(t, "salidaTiempoA.csv", append=TRUE)
	})
}

# Función para hacer las pruebas con series obtenidas de las columnas de una imagen. Cogemos cada fila del grid, que
# serán los parámetros concretos, leemos la imagen que tenga T filas y m columnas (T está en la cuarta posición del
# grid y m en la quita), que se llama "Tm.bmp" y hacemos la prueba con esa imagen. Posteriormente escribe el tiempo
# que ha tardado en hacer la prueba en un fichero .csv
hacerPruebasI <- function(grid) {
	apply(grid, 1, function(x){
		nombre <- paste(x[4], x[5], ".bmp", sep="")
		img <- read.bmp(nombre)
		t <- getTiempoI(img, x)
		write(t, "salidaTiempoI.csv", append=TRUE)
	})
}

# Función para generar series por el método de Monte Carlo dada una semilla utilizando los parámetros indicados en param
# y devolver el tiempo que tarda en encontrar las componentes principales dinámicas.
# param[1] guarda el número de cores a utilizar
# param[2] guarda la varianza a explicar
# param[3] guarda el k máximo a utilizar
# param[4] guarda T, el número de valores en cada serie
# param[5] guarda m, el número de series
getTiempoMC <- function(param, semilla) {
	# Fijamos la semilla
	set.seed(semilla)

	T <- param[4]
	m <- param[5]
	x <- matrix(0, T, m)

	# Generamos los números aleatorios necesarios para el proceso
	f <- rnorm(T+2)
	u <- matrix(rnorm(T*m), T, m)

	# Obtemos la serie con un método de Monte Carlo
	for (i in 1:m) {
		x[ ,i] <- 10*sin(2*pi*(i/m))*f[1:T] + 10*cos(2*pi*(i/m))*f[2:(T+1)] +
				10*(i/m)*f[3:(T+2)] + u[ ,i]
	}

	# Generamos las componentes principales y nos quedamos con el tiempo
	# que tarda en hacerlo
	tiempo <- system.time(auto.gdpc(x, k_max = param[3], expl_var = param[2], ncores = param[1]))

	# Devolvemos el tiempo que ha tardado en finalizar
	return(tiempo[3])
}

# Función para generar series aleatoriamente dada una semilla utilizando los parámetros indicados en param
# y devolver el tiempo que tarda en encontrar las componentes principales dinámicas.
# param[1] guarda el número de cores a utilizar
# param[2] guarda la varianza a explicar
# param[3] guarda el k máximo a utilizar
# param[4] guarda T, el número de valores en cada serie
# param[5] guarda m, el número de series
getTiempoA <- function(param, semilla) {
	# Fijamos la semilla
	set.seed(semilla)

	T <- param[4]
	m <- param[5]

	# Generamos series de números aleatorios
	x <- matrix(rnorm(T*m), T, m)

	# Generamos las componentes principales y nos quedamos con el tiempo
	# que tarda en hacerlo
	tiempo <- system.time(auto.gdpc(x, k_max = param[3], expl_var = param[2], ncores = param[1]))

	# Devolvemos el tiempo que ha tardado en finalizar
	return(tiempo[3])
}

# Función para devolver el tiempo que tarda en encontrar las componentes principales dinámicas en una imagen
# pasada por el argumento img.
# param[1] guarda el número de cores a utilizar
# param[2] guarda la varianza a explicar
# param[3] guarda el k máximo a utilizar
# param[4] guarda T, el número de valores en cada serie
# param[5] guarda m, el número de series
getTiempoI <- function(img, param) {
	# Generamos las componentes principales y nos quedamos con el tiempo
	# que tarda en hacerlo
	tiempo <- system.time(auto.gdpc(img, k_max = param[3], expl_var = param[2], ncores = param[1]))

	# Devolvemos el tiempo que ha tardado en finalizar
	return(tiempo[3])
}
