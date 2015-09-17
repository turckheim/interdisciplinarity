# Compute the interdisciplinary statistics for the institutions.

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

tmp = read.table(paste0(worldCatDir, "/ST.txt"), header = TRUE)
ST.world = tmp$ST.world
STW.world = tmp$STW.world
STB.world = tmp$STB.world

for (institutionRaw in institutions) {
  institution = correctForFileName(institutionRaw)
  inputFile = paste0(dataDir, "/input/", corpus, "/", institutionRaw, ".txt")
  
  cat("Corpus:", corpus, " - Institution:", institution, "...\n")

  freqMatrix = normalizeCountMatrix(as.matrix(read.table(inputFile)))
  stopifnot(ncol(freqMatrix) == ncol(catDist))
  stopifnot(colnames(freqMatrix) == colnames(catDist))
  
  
  resTable = as.data.frame(matrix(NA, nrow=3, ncol=7,
                                   dimnames=list(c("ST","STW","STB"),
                                                 c("value", "epsilon", "sigma", "Zscore", "world", "pvalue", "N"))))
  resTable["ST","world"] = ST.world
  resTable["STW","world"] = STW.world
  resTable["STB","world"] = STB.world
  resTable$N = nrow(freqMatrix)
  
  #cat("Computing STas...\n")
  STa = computeSTa(freqMatrix)
  resTable["STW","value"] = mean(STa)
  Q = colSums(freqMatrix)/nrow(freqMatrix) 
  gamma = Q %*% catDist
  resTable["ST","value"] = gamma %*% Q
  resTable["STB","value"] = resTable["ST","value"] - resTable["STW","value"]
  
  resTable["STW", "sigma"] = sd(STa)
  
  H_a = 2*(freqMatrix %*% t(gamma))
  
  resTable["ST", "sigma"] = sd(H_a)
  resTable["STB", "sigma"] = sd(H_a - STa)
  
  resTable$Zscore = sqrt(length(STa))/resTable$sigma*(resTable$value - resTable$world)
  
  resTable$epsilon = -qnorm(alpha/2)*resTable$sigma/sqrt(nrow(freqMatrix))
  
  resTable$pvalue = 2*pnorm(abs(resTable$Zscore), lower.tail = FALSE)
  
  outdir = paste0(dataDir, "/stat_results/", corpus, "/", institution)
  if (!file.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  write.table(data.frame(STa=STa), file = paste0(outdir, "/", "STa.txt"))
  write.table(resTable, paste0(outdir, "/", "ST.txt"))
  
  print(resTable)
  
  #contributions = data.frame(Q=Q, C=as.vector(gamma * Q))
  #write.table(contributions, file = paste0(outdir, "/", "contributions.txt"))
}

}))

#print(computationTime)
