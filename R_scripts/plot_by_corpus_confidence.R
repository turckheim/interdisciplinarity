# Make a plot of institutions for one corpus with ellipses
# representing confidence regions.

# Input: directory "stat_results" (in data directory)
# Output: directory "plots"

rm(list=ls()[! ls() %in% c("fracMode")])

# Probability threshold used to define the confidence regions.
alpha <- 0.02

library(shape)

source("common.R")
options(stringsAsFactors = FALSE)

tmp <- read.table(paste0(dataDir, "/category_titles.txt"), header=TRUE)
categoryTitles <- tmp[,2]
names(categoryTitles) <- tmp[,1]

tmp2 = read.table(paste0(dataDir, "/corpus_titles.txt"), header=TRUE)
corpusTitles = tmp2[,2]
names(corpusTitles) = tmp2[,1]

while(length(dev.list()) > 0) {
  dev.off()
}

if (!file.exists(paste0(dataDir, "/plots"))) {
  dir.create(paste0(dataDir, "/plots"))
}

pdf(paste0(dataDir, "/plots/plot_by_corpus_confidence.pdf"))

for (corpus in list.files(paste0(dataDir, "/stat_results"))) {
  cat(corpus, "\n")

  resultList <- data.frame()
  results <- list()
  
  allData <- list()
  
  #institutionDirs <- list.files(paste0(dataDir, "/stat_results/", corpus), ignore.case = TRUE)
  institutionDirs = c("NANTES", "PARIS5", "PARIS7", "PARIS6","BOURGOGNE", "GRENOBLE1", "STRASBOURG", "U_AIX_MARSEILLE")
  

  for (institution in institutionDirs) {
    resultFile <- paste0(dataDir, "/stat_results/", corpus, "/", institution, "/ST.txt")
    if (file.exists(resultFile)) {
      resTable <- read.table(resultFile)
      allData$institution <- c(allData$institution, institution)
      allData$N <- c(allData$N, resTable$N[[1]])
      allData$STW <- c(allData$STW, 100*(resTable[["STW","value"]] - resTable[["STW","world"]]))
      allData$STB <- c(allData$STB, 100*(resTable[["STB","value"]] - resTable[["STB","world"]]))
      allData$ST.var <- c(allData$ST.var, (100*resTable[["ST","sigma"]])^2/resTable$N[[1]])
      allData$STW.var <- c(allData$STW.var, (100*resTable[["STW","sigma"]])^2/resTable$N[[1]])
      allData$STB.var <- c(allData$STB.var, (100*resTable[["STB","sigma"]])^2/resTable$N[[1]])
      allData$STW.epsilon <- c(allData$STW.epsilon, resTable[["STW","epsilon"]]*100)
      allData$STB.epsilon <- c(allData$STB.epsilon, resTable[["STB","epsilon"]]*100)
    }
  }
  allData <- as.data.frame(allData)
  allData$STWB.covar <- (allData$ST.var - allData$STW.var - allData$STB.var)/2
  
  xmax <- max(abs(allData$STW)) + 1
  ymax <- max(abs(allData$STB)) + 1

  
  plot(allData$STW, allData$STB, xlim=c(-xmax, xmax), ylim=c(-ymax, ymax), pch=".", xlab="Interdisciplinarity (STW - STW.world)", ylab="Multidisciplinarity (STB - STB.world)")
  usr <- par("usr")
  title(main=paste0(corpusTitles[corpus], " (", corpus, ")"))
  abline(h=0, col="grey")
  abline(v=0, col="grey")
  abline(b=-1, a=0, col="#555555")
  mycolors <- hsv(seq(0, 0.9, length.out = nrow(allData)), alpha = 0.5)
  mycolors.dark <- hsv(seq(0, 0.9, length.out = nrow(allData)), v=0.3)
  maxRadius <- sqrt(median(allData$N))
  for (i in 1:nrow(allData)) {
    SIGMA <- matrix(c(allData$STW.var[[i]], allData$STWB.covar[[i]], allData$STWB.covar[[i]], allData$STB.var[[i]]), 2)
    tmp <- eigen(SIGMA, symmetric=TRUE)
    e <- tmp$vectors
    lambda <- tmp$values
    c <- sqrt(qchisq(alpha, 2, lower.tail = FALSE))
    r1 <- c*sqrt(lambda[[1]])
    r2 <- c*sqrt(lambda[[2]])
    angle <- atan2(e[[2,1]], e[[1,1]])*180/pi
    plotellipse(rx=r1, ry=r2, angle=angle, mid=c(allData$STW[[i]], allData$STB[[i]]), lcol=mycolors[[i]])
    #rect(xleft = allData$STW[[i]]-allData$STW.epsilon[[i]], xright = allData$STW[[i]]+allData$STW.epsilon[[i]],
    #     ybottom = allData$STB[[i]]-allData$STB.epsilon[[i]], ytop = allData$STB[[i]]+allData$STB.epsilon[[i]],
    #     lty=2)
  }
  for (i in 1:nrow(allData)) {
    text(x=allData$STW[[i]], y=allData$STB[[i]]+0.02*(usr[[4]]-usr[[3]]), labels = allData$institution[[i]], cex=0.7, col=mycolors.dark[[i]])
  }
  
}

dev.off()

