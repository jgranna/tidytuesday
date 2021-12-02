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
library(showtext)
font_add_google("Dancing Script", "lato")
showtext_auto()
julia_vectorized <- function(xmin=0.30782799, xmax=0.30782801, nx=5000,
                                  ymin=0.22938586, ymax=0.22938588, ny=5000,
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
  
  colfunc <- colorRampPalette(c('#c8faff', "#ffe266", '#3a80f8', "#d47e14", "#f2c8a1", "#307fc4", '#fbb300', "#ff2000", "#0333ca", "#a98209",'#ff0505', "#000000"))
  palette <- colfunc(128)
  if (showplot==TRUE) { 
    par(bg = "#00225c", mar = c(2, 2, 2, 2))
    image(x,y,k,col=palette, xlab="", ylab="", xaxt = "n", yaxt = "n")
    mtext("Julia Set: c = -0.76 + 0.08i", col = "white", side = 1, at = 0.307828006, padj = 0.6, adj = -0.07, family = "lato")
    }
  return(k)
}

pdf(file = "~/Desktop/js.pdf", width = 10, height = 8)
julia_vectorized()
dev.off()

## schoene kombos
# xmin=0.3075, xmax=0.308, nx=500,ymin=0.229, ymax=0.2295
# xmin=0.307828 - 2e-07, xmax=0.307829 - 2e-07, nx=500, ymin=0.229385, ymax=0.229386

## js_mosaic: at = 0.307828006, padj = 0.6, adj = -0.07, family = "Dancing Script", xmin=0.30782799, xmax=0.30782801, nx=100, ymin=0.22938586, ymax=0.22938588, ny=100


## youtube
# The Mathemagicians' Guild
# watch https://www.youtube.com/watch?v=dctJ7ISkU-4