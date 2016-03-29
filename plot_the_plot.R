plot_ze_plot <- function() {

  mean1=0;
  mean2=4;
  std1=1;
  std2=1.5;
  THRESHOLD=2;

  STEP = 0.01;

  tp_col = 'pink';
  tn_col = 'cyan';
  fp_col = 'brown1';
  fn_col = 'aquamarine1';

  THR_X = THRESHOLD;

  X_MIN = mean1 - 4 * std1;
  X_MAX = mean2 + 4 * std2;

  x_min1 = X_MIN;
  x_max1 = mean1 + 4 * std1;

  x_min2 = mean2 - 4 * std2;
  x_max2 = X_MAX;

  # Assume that Density1 represents null-hypothesis
  t2_err_x_min = x_min2;
  t2_err_x_max = THR_X;

  t1_err_x_min = THR_X;
  t1_err_x_max = x_max1;

  x1 = seq(x_min1, x_max1, by = STEP);
  y1 = dnorm(x1, mean = mean1, sd = std1);

  x2 = seq(x_min2, x_max2, by = STEP);
  y2 = dnorm(x2, mean = mean2, sd = std2);

  x_full_range = seq(X_MIN, X_MAX, by = STEP);

  # To make a polygon grow from X-axis upwards, we need to add additional boardering values to our x/y sets
  t1_err_x = seq(t1_err_x_min, t1_err_x_max, by = STEP);
  t1_err_y = c(0, dnorm(t1_err_x, mean = mean1, sd = std1), 0);
  t1_err_x = c(THR_X, t1_err_x, x_max1);

  # To make a polygon grow from X-axis upwards, we need to add additional boardering values to our x/y sets
  t2_err_x = seq(t2_err_x_min, t2_err_x_max, by = STEP);
  t2_err_y = c(0, dnorm(t2_err_x, mean = mean2, sd = std2), 0);
  t2_err_x = c(x_min2, t2_err_x, THR_X);

  lim_xl = X_MIN * 1.1;
  lim_xr = X_MAX * 1.1;

  lim_yu = max(c(y1,y2)) * 1.1;
  lim_yd = -lim_yu*0.1;

  THR_Y = lim_yu * 1.2;

  # plot(x1, y1, pch = '.', xlim = c(lim_xl, lim_xr) , ylim = c(lim_yd, lim_yu), xlab = 'random variable value',
  #   ylab = 'probability density value');
  # dev.new(width=5, height=4)
  plot(x1, y1, pch = '.', xlim = c(lim_xl, lim_xr), xlab = 'random variable value',
    ylab = 'probability density value');
  polygon(x1, y1, col = tn_col);
  polygon(x2, y2, col = tp_col);
  polygon(t1_err_x, t1_err_y, col = fp_col);
  polygon(t2_err_x, t2_err_y, col = fn_col);
  lines(c(THR_X, THR_X), c(0, THR_Y), col = 'purple', lty = 1, lwd = 2);
  legend("topright", c('True negative', 'True positive', 'False positive', 'False negative'),
    col = c(tn_col, tp_col, fp_col, fn_col), lty=c(1, 1, 1, 1), lwd=c(6, 6, 6, 6) );

}

find_cross_root <- function (mean1, sd1, mean2, sd2, x) {

    y1 <- function(x) ( dnorm(x, mean = mean1, sd = sd1) )
    y2 <- function(x) ( dnorm(x, mean = mean2, sd = sd2) )
    g <- function(x) y1(x) - y2(x);

    xx = uniroot(g, c(min(x),max(x)) );

    return(xx$root);
}
