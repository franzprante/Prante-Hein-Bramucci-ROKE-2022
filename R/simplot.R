#' Plot of a variable of DESFC model
#' 
#' This function returns a base plot of a variable of a DESFC model.
#' @param scen An DESFC model object created with sfcr::sfcr_baseline(). Defaults to sim.
#' @param var The variable to plot as character. Defaults to "u".
#' @param hr A value (double) for a horizontal dashed line. Defaults to NULL.
#' @param tex The figure mtext as input for latex2exp::TeX(). Defaults to var.
#' @param start The start period for the plot. Defaults to the first period of the simulation.
#' @param end The end period for the plot. Defaults to the last period of the simulation.
#' @param save Logical. If TRUE, a PDF is stored at path.
#' @param path Character. The relative path in the project directory. Defaults to "output/baseline/simplot.pdf".
#' @param col Select the color of the plotted line. Calls base plot col argument.
#' @param tc Logical. If TRUE, two country scenario: both variables are plotted.
#' @return A plot of a variable
#' 
#' @example simplot(sim, u, tex = "Utilization")
simplot <- function (var = "u", scen = sim, hr = NULL, tex = var, start = 1, end = length(scen[[var]]), save = F, path = "output/baseline/simplot.pdf", col = 1, tc = F, autscal = T) {
  if (save == T) {
    pdf(
      file = here::here(paste(path)),
      width = 8,
      height = 5 )
  }
  if ( tc == F & autscal == T ) {
    plot(scen[[var]][start:end], type = "l", lwd = 3, ylab = "", xlab = "", col = col)
  } else if (tc == T & autscal == F) {
    plot(scen[[var]][start:end], type = "l", lwd = 3, ylab = "", xlab = "", col = col)
    varx <- paste0(var, "x")
    lines(scen[[varx]][start:end], col = "red", lwd = 3, lty = 3)
  } else {
    varx <- paste0(var, "x")
    plot(scen[[var]][start:end], ylim = c(min(scen[[varx]],scen[[var]]), max(scen[[varx]],scen[[var]])), type = "l", lwd = 3, ylab = "", xlab = "", col = col)
    lines(scen[[varx]][start:end], col = "red", lwd = 3, lty = 3)
  }
  if (!(is.null(hr))) abline(h = hr, lty = 2)
  mtext(latex2exp::TeX(tex))
  if (var == "u") {
    lines(scen$u_n, lty = 3)
    lines(scen$u_n - scen$u_r, lty = 4)
    lines(scen$u_n + scen$u_r, lty = 4)
  }
  if (save == T) {
    dev.off()
    browseURL(here::here(paste(path)))
  }
}


