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
library(wesanderson)
source("./multihue.R")
julia_vectorized <- function(xmin=0.30782799, xmax=0.30782801, nx=500,
                                  ymin=0.22938586, ymax=0.22938588, ny=500,
                                  n=2024, showplot=TRUE,
                                  cols=colorRampPalette(c("blue","yellow","red","black"))(1000)) 
{
  # variables
  x <- seq(xmin, xmax, length.out=nx)
  y <- seq(ymin, ymax, length.out=ny)
  c <- -0.76 + 0.08i #-1.0877 + 0.3062i
  z <- outer(x,y*1i,FUN="+")
  k <- matrix(0.0, nrow=length(x), ncol=length(y))
  
  for (rep in 1:n) { 
    index <- which(Mod(z) < 1500)
    z[index] <- z[index]^2 + c
    k[index] <- k[index] + 1
  }
  
  #if (showplot==TRUE) { image(x,y,k,col=cols, xlab="Re(c)", ylab="Im(c)")}
  stops <- c('#82ebff', '#00fb2f', '#fbb300', "#0333ca", "#a98209",'#ff0505')
  #palette <- multiHue(200,colors=stops)
  leftstops <- c("#3136AA","#3980B0","#00C5C0","#8EFDFD")
  rightstops <- c("#F3F300","#FF8CB4","#CD4C4C","#8F0000")
  palette <- multiHue.diverge(128,leftstops,rightstops,continuous=FALSE)
  if (showplot==TRUE) { image(x,y,k,col=palette, xlab="Re(c)", ylab="Im(c)")}
  return(k)
}

## schoene kombos
# xmin=0.3075, xmax=0.308, nx=500,ymin=0.229, ymax=0.2295
# xmin=0.307828 - 2e-07, xmax=0.307829 - 2e-07, nx=500, ymin=0.229385, ymax=0.229386

## youtube
# The Mathemagicians' Guild
# watch https://www.youtube.com/watch?v=dctJ7ISkU-4