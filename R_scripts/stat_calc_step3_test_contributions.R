# Compute the contributions of categories(classes) for the selected corpora
# on the selected institutions and their z-scores and p-values.

# Inputs: directory "input" (in data directory)
#         directory "stat_results_world" (in data directory)
#         category_similarity_matrix.txt (in data directory)
# Output: directory "stat_results" (in data directory)

# Parameters "corpus" and "institutions" must be defined.
stopifnot(exists("corpus"))
stopifnot(exists("institutions"))

# Probability threshold used to define the confidence intervals.
alpha = 0.01

source("common.R")

options(stringsAsFactors = FALSE)

while(length(dev.list()) > 0) {
  dev.off()
}

source("load_similarity_matrix.R")

computationTime = (system.time({

cat("Loading world statistics...\n")
worldCatDir = paste0(dataDir, "/stat_results_world/", corpus)

contribWorld = read.table(paste0(worldCatDir, "/contributions-world.txt"))
stopifnot(nrow(contribWorld) == nrow(catDist))

for (institutionRaw in institutions) {
  institution = correctForFileName(institutionRaw)
  inputFile = paste0(dataDir, "/input/", corpus, "/", institutionRaw, ".txt")
  
  cat("Corpus:", corpus, " - Institution:", institution, "...\n")

  freqMatrix = normalizeCountMatrix(as.matrix(read.table(inputFile)))
  stopifnot(ncol(freqMatrix) == ncol(catDist))
  stopifnot(colnames(freqMatrix) == colnames(catDist))
  
  Q = colSums(freqMatrix)/nrow(freqMatrix)
  gamma = Q %*% catDist
  C = as.vector(gamma * Q)
  
  D = catDist
  for (i in 1:nrow(catDist)) {
    D[,i] = Q[[i]] * catDist[,i]
  }
  diag(D) = gamma
  F = freqMatrix %*% D
  sigma = apply(F, 2, sd)
  #mettre warning si sigma ==0
  N = nrow(freqMatrix)
  score = (C - contribWorld$C)*sqrt(N)/sigma
  pvalue = 2*pnorm(abs(score), lower.tail = FALSE)

  outdir = paste0(dataDir, "/stat_results/", corpus, "/", institution)
  if (!file.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  
  contributions = data.frame(Q=Q, C=C, Qworld=contribWorld$Q, Cworld=contribWorld$C, sigma=sigma, N=N, score=score, pvalue=pvalue)
  write.table(contributions, file = paste0(outdir, "/", "contributions-score.txt"))
}

}))

#print(computationTime)
