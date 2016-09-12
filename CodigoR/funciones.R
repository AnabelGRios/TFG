# Función para escribir una imagen en gris BMP
escribirBMP <- function(nombreArchivo, imagen) {
        # Para que image no rote la imagen
        img <- apply(imagen, 2, rev)
        # Abrimos canal bmp
        bmp(nombreArchivo)
        # Lo dibujamos sin márgenes
        par(mar = rep(0,4))
        image(t(img), col = grey(seq(0, 1, length = 256)))
        # Cerramos canal
        dev.off()
}

# Función para escribir una imagen en gris en un fichero PDF
escribirPDF <- function(nombreArchivo, imagen) {
        # Para que image no rote la imagen
        img <- apply(imagen, 2, rev)
        # Abrimos canal pdf
        pdf(nombreArchivo)
        # Lo dibujamos sin márgenes
        par(mar = rep(0,4))
        image(t(img), col = grey(seq(0, 1, length = 256)))
        # Cerramos canal
        dev.off()
}

# Función para escribir una imagen en color en un fichero PDF
escribirPDFcolor <- function(nombreArchivo, imagen) {
        # Abrimos canal pdf
        pdf(nombreArchivo)
        # Lo dibujamos sin márgenes
        plot.new()
        par(mar = rep(0,4))
        plot.window(xlim=c(0,1), ylim=c(0,1))
        rasterImage(imagen, 0, 0, 1, 1)
        # Cerramos canal
        dev.off()
}

# Función para reconstruir una matriz de series originales
# dada una única componente principal, que estará en un
# objeto GDPC
reconstruir <- function(T, m, obj.gdpc) {
        f <- c(obj.gdpc$initial_f, obj.gdpc$f)
        k <- obj.gdpc$k
        z <- matrix(0, T, m)
        for (j in 1:m) {
                for (t in 1:T) {
                        suma <- 0
                        for (l in 0:k) {
                                suma <- suma + obj.gdpc$beta[j,(l+1)]*f[t-l+k]
                        }
                        z[t,j] <- suma + obj.gdpc$alpha[j]
                        suma <- 0
                }
        }
        return(z)
}

# Función para reconstruir una matriz de series originales
# dadas varias componentes principales, que estarán en una
# lista de objetos GDPC
reconstruirVariasF <- function(T, m, list.gdpc) {
        z <- matrix(0, T, m)
        for (i in 1:length(list.gdpc)) {
                z <- z + reconstruir(T, m, list.gdpc[[i]])
        }
        return(z)
}

# Función para reescalar los valores de una matriz y llevarlos
# al intervalo [0,1]
reescalar <- function(matriz) {
        M <- max(matriz)
        m <- min(matriz)
        for (i in 1:nrow(matriz)) {
                for (j in 1:ncol(matriz)) {
                        matriz[i,j] <- (matriz[i,j]-m)/(M-m)
                }
        }
        return(matriz)
}

# Función para reconstruir una matriz de series originales más un
# número determinado de valores predichos dada una componente
# principal dinámica predicha
reconstruirPrediccion <- function(T, m, obj.gdpc, f_predecida) {
        f <- c(obj.gdpc$initial_f, f_predecida)
        k <- obj.gdpc$k
        z <- matrix(0, T, m)
        for (j in 1:m) {
                for (t in 1:T) {
                        suma <- 0
                        for (l in 0:k) {
                                suma <- suma + obj.gdpc$beta[j,(l+1)]*f[t-l+k]
                        }
                        z[t,j] <- suma + obj.gdpc$alpha[j]
                        suma <- 0
                }
        }
        return(z)
}
