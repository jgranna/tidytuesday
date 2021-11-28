# Mandelbrot.R
# Myles Harrison
# everydayanaltics.ca
# -------------------

# "Naive" version

mandelbrot_naive <- function(xmin=-2, xmax=2, nx=500,
                             ymin=-1.5, ymax=1.5, ny=500,
                             n=1000, showplot=TRUE,
                             cols=colorRampPalette(c("blue","yellow","red","black"))(11)) 
{
  
  # variables
  x <- seq(xmin, xmax, length.out=nx)
  y <- seq(ymin, ymax, length.out=ny)
  c <- outer(x,y*1i,FUN="+")
  z <- matrix(0.0, nrow=length(x), ncol=length(y))
  k <- matrix(0.0, nrow=length(x), ncol=length(y))
  
  for (rep in 1:n) { 
    for (i in 1:nx) { 
      for (j in 1:ny) { 
        if(Mod(z[i,j]) < 1000) {
          z[i,j] <- z[i,j]^2 + c[i,j]
          k[i,j] <- k[i,j] + 1
        }
      }
    }
  }
  
  if (showplot==TRUE) { image(x,y,k,col=cols,xlab="Re(c)",ylab="Im(c)")}
  
  return(k)
  
}


# Vectorized version
mandelbrot_vectorized <- function(xmin=-2, xmax=2, nx=500,
                                  ymin=-1.5, ymax=1.5, ny=500,
                                  n=1000, showplot=TRUE,
                                  cols=colorRampPalette(c("blue","yellow","red","black"))(100)) 
{
  
  # variables
  x <- seq(xmin, xmax, length.out=nx)
  y <- seq(ymin, ymax, length.out=ny)
  c <- outer(x,y*1i,FUN="+")
  z <- matrix(0.0, nrow=length(x), ncol=length(y))
  k <- matrix(0.0, nrow=length(x), ncol=length(y))
  
  for (rep in 1:n) { 
    index <- which(Mod(z) < 1000)
    z[index] <- z[index]^2 + c[index]
    k[index] <- k[index] + 1
  }
  
  if (showplot==TRUE) { image(x,y,k,col=cols, xlab="Re(c)", ylab="Im(c)")}
  
  return(k)
  
}

# Vectorized version
julia_vectorized <- function(xmin=-1, xmax=1, nx=500,
                                  ymin=-1, ymax=1, ny=500,
                                  n=1000, showplot=TRUE,
                                  cols=colorRampPalette(c("blue","yellow","red","black"))(100)) 
{
  
  # variables
  x <- seq(xmin, xmax, length.out=nx)
  y <- seq(ymin, ymax, length.out=ny)
  c <- -0.8 + 0.2i #-1.0877 + 0.3062i
  z <- outer(x,y*1i,FUN="+")
  k <- matrix(0.0, nrow=length(x), ncol=length(y))
  
  for (rep in 1:n) { 
    index <- which(Mod(z) < 1000)
    z[index] <- z[index]^2 + c
    k[index] <- k[index] + 1
  }
  
  if (showplot==TRUE) { image(x,y,k,col=cols, xlab="Re(c)", ylab="Im(c)")}
  
  return(k)
  
}

##
youtube
The Mathemagicians' Guild
watch https://www.youtube.com/watch?v=dctJ7ISkU-4