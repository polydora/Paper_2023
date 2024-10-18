library(ggplot2)
library(jpeg)
library(grid)
library(gridExtra)
library(cowplot)

files <- sample(list.files(path = "images/"), 4)





##############################
library(magick)
#> Linking to ImageMagick 6.9.12.3
#> Enabled features: cairo, freetype, fftw, ghostscript, heic, lcms, pango, raw, rsvg, webp
#> Disabled features: fontconfig, x11

imgs_url <- c('https://drive.google.com/file/d/1VnwYoYS_WY4kJ_bNBlc6H9EmViK6jxln/view?usp=drive_link',
              'https://drive.google.com/file/d/1Ey_DR79UPqx15YlcNJXfOOOYcrVzps0d/view?usp=drive_link',
              'https://drive.google.com/file/d/1jhfAuPtB5zpmB5BtEjeOPU3FDtGUHFp8/view?usp=drive_link',
              'https://drive.google.com/file/d/1FlMFu38qNhBr-DSekLXx3MTxkj16QSr0/view?usp=drive_link')

imgs_url <- c("images/i (26) (1).jpg",
              "images/i (27) (1).jpg",
              "images/i (28) (1).jpg",
              "images/i (29).jpg")

files <- sample(list.files(path = "images/"), 16)

files <- paste("images/", files, sep ="")

imgs <- image_read(files)

plot(image_montage(imgs, tile = '4x4', geometry = "x200+3+5"))

library(ggplotify)


pl <- as.ggplot(scale = 1.2, function() plot(image_montage(imgs, tile = '4x4', geometry = "x200+1+1")))

pl + theme(aspect.ratio = 1)



xy <- locator(n=3)
xy <- as.data.frame()

points(xy$x, xy$y)

points <- data.frame(x=c(0, 0.5, 1), y = c(0, 0.5, 1))

library(grid)


pl + theme(aspect.ratio = 1.) +
  geom_point(data = points, aes(x, y), size = 4, color = "blue")

xy <- data.frame(x = NULL, y = NULL)

library(ggmap)

gglocator(n = 3)

gglocator(n = 3, mercator = F)


####################################

if (interactive()) {

  # only run for interactive sessions
  df <- expand.grid(x = 0:-5, y = 0:-5)

  ggplot(df, aes(x, y)) + geom_point() +
    annotate(geom = "point", x = -2, y = -2, colour = "red")

  (pt <- gglocator(mercator = FALSE)) # click red point

  last_plot() +
    annotate("point", pt$x, pt$y, color = "blue", size = 3, alpha = .5)

  hdf <- get_map("houston, texas")
  ggmap(hdf, extent = "normal")
  (pt <- gglocator(mercator = TRUE))
  last_plot() +
    annotate("point", pt$lon, pt$lat, color = "blue", size = 3, alpha = .5)

}
