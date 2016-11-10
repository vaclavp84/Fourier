library(png)
library(ripa)
library(waved)

generate.gaus2D <- function(size = 500, variance = 0.25)
{
  X0 = (1:size/size) - 0.5;  
  Xm <- matrix(nrow = size, ncol = size)
  Ym <- matrix(nrow = size, ncol = size)
  for (i in 1:size)
  {
    Xm[1:size,i] <- X0
    Ym[i,1:size] <- X0
  }
  mask = exp(-((Xm)^2 + (Ym)^2)/(2*variance^2))
}


size <- 286 
X0 = (1:size/size) - 0.5;  
Xm <- matrix(nrow = size, ncol = size)
Ym <- matrix(nrow = size, ncol = size)
for (i in 1:size)
{
  Xm[1:size,i] <- X0
  Ym[i,1:size] <- X0
}
mask1 = generate.gaus2D(size = size, variance = 0.75)
mask3 = generate.gaus2D(size = size, variance = 0.15)
mask2 = generate.gaus2D(size = size, variance = 0.1)
mask2 <- mask3 - mask2

windows(width = 500, height = 500)
image2D(mask2, col = gray.colors(256, start = 0 , end = 1, gamma = 1, alpha = NULL), scale = FALSE)

#read file
original <-readPNG("Medicem.png")

#get size
r = original[,,1]
g = original[,,2]
b = original[,,3]
original_bw = (0.21*r+0.71*g+0.07*b)*mask1
#nrow = nrow(img)
#ncol = ncol(img)


#Imcropped <- matrix(0,nrow,nrow)
#Imcropped <- matrix(0,nrow,nrow)


#image2D(gray1)
img_fft <- fft(original_bw)

img_fft_shift <- matrix(0,286,286)
img_fft_shift[1:143,1:143]     <- img_fft[144:286,144:286]
img_fft_shift[1:143,144:286]   <- img_fft[144:286,1:143]
img_fft_shift[144:286,1:143]   <- img_fft[1:143,144:286] 
img_fft_shift[144:286,144:286] <- img_fft[1:143,1:143]

#mask <- generate.mask(15, 286)
img_fft_shift <- img_fft_shift*mask2
windows(width = 500, height = 500)
image2D(log(abs(img_fft_shift)), col = gray.colors(256, start = 0 , end = 1, gamma = 1, alpha = NULL), scale = FALSE)

#image2D(log(abs(img_fft_shift)))

img_ifft_shift <- matrix(0,286,286)
img_ifft_shift[1:143,1:143]     <- img_fft_shift[144:286,144:286]
img_ifft_shift[1:143,144:286]   <- img_fft_shift[144:286,1:143]
img_ifft_shift[144:286,1:143]   <- img_fft_shift[1:143,144:286] 
img_ifft_shift[144:286,144:286] <- img_fft_shift[1:143,1:143]

img_back <- fft(img_ifft_shift, inverse = TRUE)


windows(width = 500, height = 500)
image2D(abs(img_back),286:1, 286:1, col = gray.colors(256))
