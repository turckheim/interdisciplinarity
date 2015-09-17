# Common functions and parameters for all the interdisciplinary program.

if (!exists("paste0")) {
  paste0 = function(...) {
    paste(..., sep="")
  }
}

if (!exists("fracMode")) {
  fracMode = FALSE
}

if (fracMode) {
  cat("FRACTIONNAL count mode\n")
  dataDir = path.expand("../data_frac")
} else {
  cat("WHOLE count mode\n")
  dataDir = path.expand("../data_integer")
}

options(stringsAsFactors = FALSE)

# For the current plot, takes a position as a number in [0,1]
# and return the position as units of the current plot.
# Example: if range is [100,200], passing 0.5 gives 150.
# Also work for <0 and >1 for out-of-plot things.
usrFromRelativeX = function(x) {
  usr = par("usr")
  return(x * (usr[2] - usr[1]) + usr[1])
}

# Same for Y
usrFromRelativeY = function(y) {
  usr = par("usr")
  return(y * (usr[4] - usr[3]) + usr[3])
}

# Divide each row (article) by the sum of the row
# such that each article has an equal weight.
normalizeCountMatrix = function(M) {
  return(M / rowSums(M))
}

# Compute the ST for each article
computeSTa = function(M) {
  STa = rep(NA, nrow(M))
  for (a in 1:nrow(M)) {
    STa[[a]] = (M[a,,drop=FALSE] %*% catDist) %*% t(M[a,,drop=FALSE])
  }
  names(STa) = rownames(M)
  return(STa)
}


# Functions to display progress bars
progressBarInit = function() {
  for (i in 1:10) {
    cat(" ")
  }
  for (i in 1:50) {
    cat("_")
  }
  cat("\n")
  for (i in 1:10) {
    cat(" ")
  }
  .progressBarLast <<- 0
}

# x is in [0,1]
progressBarNext = function(x) {
  if (length(x) == 0) {
    stop("progressBarNext: invalid argument")
  }
  if (x > 1) x = 1
  while (50*x >= .progressBarLast + 1) {
    cat("X")
    .progressBarLast <<- .progressBarLast + 1
  }
}

progressBarEnd = function() {
  while (.progressBarLast < 50) {
    cat("X")
    .progressBarLast <<- .progressBarLast + 1
  }
  cat(" done.\n")
}


rangeContainsZero = function(a, b) {
  if (a == 0 || b == 0) {
    return(TRUE)
  } else {
    return(sign(a) != sign(b))
  }
}

correctForFileName = function(x) {
  return(sub("^\\s+", "", sub("\\s+$", "", x)))
}


