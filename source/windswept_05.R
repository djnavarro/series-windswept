
library(ggforce)
library(jasmines)
library(here)

seeds <- 150:169

blend <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255, 
           green = z[2, ]/255, 
           blue = z[3, ]/255)
  return(z)
} 

windswept <- function(seed, dpi = 600, width = 10, height = 10, grain = 5000,
                      iterations = 50, scale = .003) {
  
  version <- 5
  file <- make_filename("windswept", version, seed, ".jpg")
  cat("making:", file, "\n")
  
  set.seed(seed)
  bg <- rscico()
  pal <- make_palette(pin = "black", mix = bg)
  
  dat <- use_seed(seed) %>% 
    scene_sticks(n = 6, grain = 1000) %>%
    unfold_breeze(40, drift = 0, scale = .0005) %>%
    dplyr::filter(time > 3) %>%
    dplyr::mutate(x = x * 2, y =y * 2) %>%
    dplyr::mutate(ind = 1:dplyr::n()) %>%
    unfold_warp(iterations = iterations, scale = scale) %>% 
    prepare_data(
      palette = pal,
      colour = "time", 
      alpha = c(0.15, scale)
    ) %>%
    dplyr::sample_frac(.5)

  pic <- ggplot2::ggplot(
    data = dat, 
    mapping = ggplot2::aes(
      x = x, 
      y = y, 
      xend = xend, 
      yend = yend, 
      alpha = al, 
      colour = factor(order))
  ) + 
    ggplot2::scale_color_manual(
      values = colours_from(pal, dat$order)
    ) + 
    ggplot2::scale_alpha_identity() + 
    jasmines:::theme_mono(blend(bg, "white", .3)) + 
    ggplot2::coord_equal(
      xlim = c(.15, .85), 
      ylim = c(.15, .85)
    ) + 
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    NULL
  pic <- pic + 
    # ggforce::geom_voronoi_tile(
    #   data = dat_bg,
    #   mapping = aes(x, y, fill = f),
    #   inherit.aes = FALSE,
    #   colour = NA,
    #   radius = .01, 
    #   #expand = -.0001,
    #   show.legend = FALSE
    # ) + 
    ggplot2::geom_segment(
      show.legend = FALSE,
      size = .1,
      alpha = 1
    )
  
  pic %>% export_image(
    filename = here("image", file), 
    dpi = dpi, 
    width = width, 
    height = height 
  )
  return(invisible(NULL))
}

rscico <- function() {
  pal <- sample(scico::scico_palette_names(), 1)
  sample(scico::scico(n=256, palette=pal), 1)
}

make_palette <- function(pin = "black", mix = "black") {
  base <- sample(colours(distinct=TRUE), 3)
  base[1] <- pin
  blends <- purrr::map_chr(base, ~blend(.x, mix, .5))
  colorRampPalette(blends)
}

make_filename <- function(prefix, sys_num, seed, suffix, 
                          sys_digits = 2, seed_digits = 3, sep = "_") {
  seed <- as.character(seed)
  sys_num <- as.character(sys_num)
  while(nchar(seed) < seed_digits) seed <- paste0("0", seed)
  while(nchar(sys_num) < sys_digits) sys_num <- paste0("0", sys_num)
  filename <- paste0(prefix, sep, sys_num, sep, seed, suffix)
  return(filename)
}

colours_from <- function (palette, order, ...) {
  palette(n = length(unique(order)))
}


prepare_data <- function(
  data, 
  palette = "viridis", 
  colour = "order", 
  alpha = c(0.3, 0)
){
  
  ribbon <- data
  ribbon$order <- ribbon[[colour]]
  if (is.character(palette)) {
    palette <- palette_named(palette)
  }
  alpha_init <- alpha[1]
  if (length(alpha) > 1) {alpha_decay <- alpha[2]}
  else {alpha_decay <- 0}
  if (!("order" %in% names(ribbon))) {
    ribbon$order <- 1:nrow(ribbon)
  }
  
  xmin <- min(ribbon$x)
  xmax <- max(ribbon$x)
  ymin <- min(ribbon$y)
  ymax <- max(ribbon$y)
  xmin <- min(xmin, ymin)
  xmax <- max(xmax, ymax)
  ymin <- xmin
  ymax <- xmax
  
  ribbon <- ribbon %>% 
    dplyr::mutate(
      x = (x - xmin)/(xmax - xmin), 
      y = (y - ymin)/(ymax - ymin), 
      al = alpha_init * (1 - alpha_decay)^(time - 1)
    )
  
  ribbon2 <- ribbon %>% 
    dplyr::rename(xend = x, yend = y) %>% 
    dplyr::mutate(time = time - 1) %>% 
    dplyr::filter(time > 0)
  ribbon <- ribbon %>% 
    dplyr::filter(time < max(time))
  ribbon$xend <- ribbon2$xend
  ribbon$yend <- ribbon2$yend
  ribbon$order <- ribbon2$order
  
  return(ribbon)
}


for(s in seeds) {
  windswept(s)
  
  # ---- high resolution ----
  # night_sky(
  #   seed = s, 
  #   width = 100/3, 
  #   height = 100/3, 
  #   dpi = 600, 
  #   grain = 15000, 
  #   iterations = 200,
  #   scale = .005
  # )
}