# README:
# HOW TO USE
# you might need to install: tidyverse, ggpmisc, gganimate, ggstance (and maybe some more)
# use source("quick_n_clean_plots.R")
# run one of the following plots
  # plot_dens(x) # plots density plot of x, where x is your sample
  # plot_scatter(x, y) # plots scatterplot
  # plot_actual_predicted(actual, predicted) # plot actual vs. predicted
# feel free to add new plots to the mix
# if you want to load with documentation I suggest you check klmr/modules on github
  

#'@title log sequence
#'@description
#'
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return 
#'
#'
#'@references
#'
#'@export
lseq <- function(from, to, length_out, round_output = F) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  x <- exp(seq(log(from), log(to), length.out = length_out))
  return(x)
}



#'@title plots a gif of the density distribution of x
#'@description
#'trivial from title
#'
#'@param x
#'@param x
#'
#'@author
#'K. Enevoldsen
#'
#'@return 
#'ggplot object
#'
#'@references
#'
#'@export
plot_dens_gif <- function(x, from = 2, to = NULL, length_out = 100, scale = "log", caption = T){
  if (is.null(to)){
    to <- nrow(d)
  }
  d <- data.frame(x = x)
  
  if (caption){
    c_text = paste("\nMade using Quick 'n' Clean by K. Enevoldsen", sep = "")
  } else {c_text = ""}
  
  if (scale == "log"){
    s <- round(lseq(from, to, length_out), 0)
  } else {
    s <- round(seq(from, to, length.out = length_out), 0)
  }
  
  d2 <- tibble(x = d[1:start_n,]) %>% mutate(t = start_n)
  for (i in s){
    tmp <- tibble(x = d[1:i,]) %>% mutate(t = i)
    if (i == s[1]){
      d2 <- tmp
    } else {
      tmp <- tibble(x = d[1:i,]) %>% mutate(t = i)  
    }
    d2 <- rbind(d2, tmp)
  }
  
  ggplot2::ggplot(data = d2, aes(x = x)) + 
    ggplot2::geom_density(alpha = 0.9, color = NA, fill = "lightsteelblue") +
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.border = element_blank()) + 
    ggplot2::labs(title = 'Density plot of x with {closest_state} samples', x = 'x', y = 'Density', caption = c_text) + 
    gganimate::transition_states(states = t, transition_length = 10) + 
    ggplot2::xlim(0,1)
}

#'@title plots scatterplot
#'@description
#'trivial from title
#'
#'@param x
#'
#'@author
#'K. Enevoldsen
#'
#'@return 
#'ggplot object
#'
#'@references
#'
#'@export
plot_scatter <- function(x, y = NULL, add_fit = T, add_formula = T, formula = y ~ x, caption = T, ci = 0.95){
  if (caption){
    c_text = paste("\nMade using Quick 'n' Clean by K. Enevoldsen", sep = "")
    if (add_fit){
      c_text = paste("The shaded interval indicate the ", ci*100, "% CI", c_text , sep = "")
    }
  } else {c_text = ""}
  
  d <- data.frame(x = x) 
  if (is.null(y)){
    d <- data.frame(y = x) 
    d$x <- 1:nrow(d)
    y_lab = "x"
    x_lab = "Index"
  } else {
    d <- data.frame(x = x, y = y) 
    x_lab = "x"
    y_lab = "y"
  }
  
  p <- ggplot2::ggplot(data = d, aes(x = x, y = y)) 
  if (add_fit){
    p <- p + 
      ggplot2::geom_smooth(method = "lm", formula = formula, level = ci, color = alpha("black", .8), alpha = 0.7)
  } 
  if (add_formula){
    p <- p + 
      ggpmisc::stat_poly_eq(formula = formula,
                            aes(label = paste(..eq.label.., "\n\n", ..rr.label.., "\n\n",..adj.rr.label.., sep = "~~~")), 
                            parse = TRUE)
  }
  p <- p + 
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::labs(title = ' ', x = x_lab, y = y_lab, caption = c_text) + 
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.border = element_blank()) 
  return(p)
}


#'@title plots density distribution of x
#'@description
#'trivial from title
#'
#'@param x
#'
#'@author
#'K. Enevoldsen
#'
#'@return 
#'ggplot object
#'
#'@references
#'
#'@export
plot_dens <- function(x, add_map = T, add_box = T, ci = 0.95, caption = T){
  d <- data.frame(x = x)
  # map
  dens <- density(x)
  map_ <- dens$x[dens$y == max(dens$y)]
  max_dens <- max(dens$y)
  offset_y <- (max_dens - min(dens$y)) /  25
  offset_x <-  (max(x)-min(x))/ 10
  
  if (caption){
    c_text = paste("Shaded interval indicate ", ci*100, "% CI","\nMade using Quick 'n' Clean by K. Enevoldsen", sep = "")
  } else {c_text = ""}
  
  p <- ggplot2::ggplot(data = d, aes(x)) + 
    ggplot2::geom_density(alpha = 0.9, color = NA, fill = "lightsteelblue") 
  
    # add shaded ci
    epsilon <- (1-ci)/2
    q1 <- quantile(x,0+epsilon)
    q2 <- quantile(x,1-epsilon)
    df.dens <- data.frame(x = density(x)$x, y = density(x)$y)
    p <- p + 
      ggplot2::geom_area(data = subset(df.dens, x >= q1 & x <= q2),
                       aes(x=x,y=y), color = 'lightsteelblue', alpha = 0.1) +
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.border = element_blank()) + 
    ggplot2::labs(title = ' ', x = 'x', y = 'Density', caption = c_text)
  if (add_map){
    p <- p + 
      ggplot2::annotate("point", x = map_, y = max(dens$y)) + 
      ggplot2::geom_vline(xintercept = map_, linetype = "dashed", alpha = 0.4) + 
      ggplot2::annotate("label", x = map_ + offset_x, y = max_dens + offset_y, label.size = 0, label = paste("(", round(map_, 2),", ", round(max_dens, 2), ")", sep = ""))
  }
  if (add_box){
     p <- p + 
      ggstance::geom_boxploth(aes(y = -0.5), outlier.shape = NA)
  } 
  return(p)
}

#'@title plots the predicted vs actual
#'@description
#'trivial from title
#'
#'@param x
#'
#'@author
#'K. Enevoldsen
#'
#'@return 
#'ggplot object
#'
#'@references
#'
#'@export
plot_actual_predicted <- function(actual, predicted, add_rmse = T, add_r2 = T, caption = T){
  d <- data.frame(actual = actual, predicted = predicted)
  
  if (caption){
    c_text = paste("\nMade using Quick 'n' Clean by K. Enevoldsen", sep = "")
  } else {c_text = ""}
  
  
  p <- ggplot2::ggplot(data = d, aes(x = actual, y = predicted)) + 
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1) + 
    ggplot2::labs(title = ' ', y = 'Predicted', x = 'Actual', caption = c_text) + 
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.border = element_blank())
  
  if (add_rmse  | add_r2){
    e <- Metrics::rmse(d$actual, d$predicted)
    r2 <- cor(d$actual, d$predicted)
    # placement
    yc <- min(predicted) + (max(predicted)-min(predicted))/8
    xc <- max(actual) - (max(actual)-min(actual))/8
    lab_text = c()
    if (add_rmse){
      lab_text = c(lab_text, paste0("RMSE:", signif(e, 2)))
    } 
    if (add_r2){
      lab_text = c(lab_text, paste0("R^2:", signif(r2, 2)))
    }
    p <- p + 
      ggplot2::annotate("text", x = xc, y = c(yc, yc*0.85), label = lab_text, parse = T)
  }
  return(p)
}


