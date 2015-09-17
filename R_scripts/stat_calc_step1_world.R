# Compute the interdisciplinary statistics for the world reference.

# Inputs: directory "world" (in data directory)
#         category_similarity_matrix.txt (in data directory)
# Output: directory "stat_results_world" (in data directory)

# Parameter "corpus" must be defined.
stopifnot(exists("corpus"))

source("common.R")

options(stringsAsFactors = FALSE)

while(length(dev.list()) > 0) {
  dev.off()
}

source("load_similarity_matrix.R")

computationTime <- (system.time({

cat("Loading world matrix for corpus", corpus, "...\n")
worldFile <- paste0(dataDir, "/world/", corpus, ".txt")
worldFreqMatrix <- normalizeCountMatrix(as.matrix(read.table(worldFile)))
stopifnot(ncol(worldFreqMatrix) == ncol(catDist))
stopifnot(colnames(worldFreqMatrix) == colnames(catDist))

cat("Computing world STas...\n")
STa.world <- computeSTa(worldFreqMatrix)
Q.world <- colSums(worldFreqMatrix)/nrow(worldFreqMatrix)
gamma.world <- Q.world %*% catDist
ST.world <- gamma.world %*% Q.world
STW.world <- mean(STa.world)
STB.world <- ST.world - STW.world
contributions.world <- data.frame(Q=Q.world, C=as.vector(gamma.world * Q.world))
centiles.STa.world <- quantile(STa.world, seq(0, 1, by=0.01))

outdirCat <- paste0(dataDir, "/stat_results_world/", corpus)
if (!file.exists(outdirCat)) {
  dir.create(outdirCat, recursive = TRUE)
}
write.table(contributions.world, file = paste0(outdirCat, "/", "contributions-world.txt"))
write.table(data.frame(centile=unname(centiles.STa.world), row.names = seq(0, 1, by=0.01)), file = paste0(outdirCat, "/", "centiles-STa-world.txt"))
write.table(data.frame(ST.world, STW.world, STB.world), paste0(outdirCat, "/", "ST.txt"), row.names = FALSE)

}))

print(computationTime)
