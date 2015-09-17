# Load category similarity matrix.
# This script is not meant to be used directly, but is called by other scripts.

catSim = as.matrix(read.table(paste0(dataDir, "/category_similarity_matrix.txt"), stringsAsFactors = FALSE))

# Verify that the matrix has the right properties
if (!(all(abs(diag(catSim) - 1) < 1E-10))) stop("diagonal must contain ones")
if (!all(t(catSim) == catSim)) stop("matrix must be symmetric")
if (!all(rownames(catSim) == colnames(catSim))) stop("mismatch between row and column names")
if (any(catSim < 0)) stop("matrix contains negative values")
if (any(catSim > 1)) stop("matrix contains values higher than 1")

# Translate similarity matrix to a distance matrix
catDist = 1 - catSim

# Make the list of categories
categories = colnames(catSim)

