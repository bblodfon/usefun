# 100 as-much-as-possible distinct colors
colors.100 = c("#000000", "#0089A3", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941",
               "#006FA6", "#A30059", "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC",
               "#B79762", "#004D43", "#8FB0FF", "#997D87", "#5A0007", "#809693",
               "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
               "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9",
               "#B903AA", "#D16100", "#DDEFFF", "#000035", "#7B4F4B", "#A1C299",
               "#300018", "#0AA6D8", "#013349", "#00846F", "#372101", "#FFB500",
               "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
               "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68",
               "#7A87A1", "#788D66", "#885578", "#FAD09F", "#FF8A9A", "#D157A0",
               "#BEC459", "#456648", "#0086ED", "#886F4C", "#34362D", "#B4A8BD",
               "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
               "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757",
               "#C8A1A1", "#1E6E00", "#7900D7", "#A77500", "#6367A9", "#A05837",
               "#6B002C", "#772600", "#D790FF", "#9B9700", "#549E79", "#FFF69F",
               "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
               "#5B4534", "#FDE8DC", "#404E55", "#FFFF00")

#' Make a color bar plot
#'
#' Use this function when you want to visualise some numbers and their
#' respective color values. Note that more than 42 colors won't be nice
#' to see (too thin bars)!
#'
#' @param color.vector vector of color values
#' @param number.vector vector of numeric values (same length with
#'   \code{color.vector})
#' @param title string. The title of the barplot
#' @param x.axis.label string. The x-axis label. Default value: empty string
#'
#' @examples
#' color.vector = rainbow(10)
#' number.vector = 1:10
#' title = "First 10 rainbow() colors"
#' make_color_bar_plot(color.vector, number.vector, title)
#'
#' @importFrom graphics barplot axis
#' @export
make_color_bar_plot = function(color.vector, number.vector, title,
                               x.axis.label = "") {
  bp = barplot(rep(1,length(color.vector)), col = color.vector,
               axes = FALSE, xlab = x.axis.label, main = title, border = NA)
  axis(1, bp, number.vector)
}

#' Multiple densities plot
#'
#' Combine many density distributions to one common plot.
#'
#' @param densities a list, each element holding the results from executing
#' the \code{\link{density}} function to a (different) vector. Note that you
#' need to provide a name for each list element for the legend (see example).
#'
#' @param legend.title string. The legend title.
#' @param title string. The plot title.
#' @param x.axis.label string. The x-axis label.
#' @param legend.size numeric. Default value: 1.
#'
#' @examples
#' mat = matrix(rnorm(60), ncol=20)
#' densities = apply(mat, 1, density)
#' names(densities) = c("1st", "2nd", "3rd")
#' make_multiple_density_plot(densities, legend.title = "Samples",
#'   x.axis.label = "", title = "3 Normal Distribution Samples")
#'
#' @importFrom graphics legend lines plot
#' @export
# make_multiple_density_plot =
  function(densities, legend.title, title, x.axis.label, legend.size = 1) {
    stopifnot(length(densities) <= 100)

    # take colors from the 100 distict color set
    color.palette = colors.100[1:length(densities)]

    plot(NA, xlim = range(sapply(densities, "[", "x")),
             ylim = range(sapply(densities, "[", "y")),
             main = title, xlab = x.axis.label, ylab = "Density")
    mapply(lines, densities, col = color.palette)

    legend("topright", legend = names(densities), fill = color.palette,
           title = legend.title, cex = legend.size)
}

#' Plot string to output format
#'
#' Execute a plot string expression and output the result to the specified
#' file format.
#'
#' @param file string. The name of the file, can be a full path.
#' @param file.format string. The output file format. Can be one of these:
#' pdf, svg, png or tiff.
#' @param plot.string string. The plot string expression.
#'
#' @examples
#' x = 1:10
#' y = 1:10
#' plot_string_to_file("my_file.pdf", "pdf", "plot(x,y)")
#'
#' @importFrom grDevices dev.off pdf png svg tiff
#' @export
plot_string_to_file =
  function(file, file.format = c("pdf", "png", "svg", "tiff"), plot.string) {
    file.format = match.arg(file.format)

    if (file.format == "pdf")
      pdf(file)
    else if (file.format == "svg")
      svg(file, width = 7, height = 7)
    else if (file.format == "png")
      png(file, width = 7, height = 7, units = 'in', res = 300)
    else if (file.format == "tiff")
      tiff(file, width = 7, height = 7, units = 'in', res = 300)

    eval(parse(text = plot.string))
    dev.off()
}
