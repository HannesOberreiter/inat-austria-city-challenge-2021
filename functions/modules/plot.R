# Saving Plots
fSaveImages <- function(currentplot, filename, path = "output/figs/", w = 7.5, h = 4, ...) {
    ggplot2::ggsave(paste0(path, filename, ".pdf"), currentplot, width = w, height = h, ...)
    ggplot2::ggsave(paste0(path, filename, ".png"), currentplot, width = w, height = h, dpi = 320, ...)
    invisible(currentplot)
}