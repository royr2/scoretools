suppressPackageStartupMessages({
  library(data.table)
  library(dtplyr)
  library(dplyr)
})

c_width <- cli::console_width()
msg <- "[scoretools v0.0.1 loaded]"
msg <- paste("\n", paste0(rep("-", c_width - nchar(msg)), collapse = ""), msg, "\n")
cat(crayon::yellow(msg))



