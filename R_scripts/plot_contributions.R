# Make contributions plots.

# Input: directory "stat_results" (in data directory)
# Output: directory "plots"

rm(list=ls()[! ls() %in% c("fracMode")])

# You can either specifiy "all" or a list of institutions
institutions = "all"
#institutions = c("U_AIX_MARSEILLE", "BESANCON")

# You can either specifiy "all" or a list of corpus
corpuss = "all"
#corpuss = c("AA", "EC")

# applies to difference *100
contributionMinimalDifference = 100 * 2E-3

# maximum p-value of displayed points
pvalueThreshold = 0.01

library(shape)

source("common.R")
options(stringsAsFactors = FALSE)

plotCircleOutsidePlot = function(x0, y0, r, col, border=NA) {
  theta = seq(0, 2*pi, length.out = 100)
  usr = par("usr")
  pin = par("pin")
  polygon(x=x0 + r*cos(theta), y=y0 + r*sin(theta)*(usr[4] - usr[3])/(usr[2] - usr[1])*pin[1]/pin[2], border=border, col=col, xpd=TRUE)
}

tmp = read.table(paste0(dataDir, "/category_titles.txt"), header=TRUE)
categoryTitles = tmp[,2]
names(categoryTitles) = tmp[,1]

tmp2 = read.table(paste0(dataDir, "/corpus_titles.txt"), header=TRUE)
corpusTitles = tmp2[,2]
names(corpusTitles) = tmp2[,1]


while(length(dev.list()) > 0) {
  dev.off()
}

outDirGeneral = paste0(dataDir, "/plots/contributions_test")

for (corpus in list.files(paste0(dataDir, "/stat_results"))) {
  
  if (corpuss[[1]] == "all" || corpus %in% corpuss) {

    institutionDirs = list.files(paste0(dataDir, "/stat_results/", corpus), ignore.case = TRUE)
    for (institution in institutionDirs) {
      
      if (institutions[[1]] == "all" || institution %in% institutions) {
      
        resultFile = paste0(dataDir, "/stat_results/", corpus, "/", institution, "/contributions-score.txt")
        if (file.exists(resultFile)) {
          cat(corpus, institution, "\n")
          
          contributions = read.table(resultFile)
          contributions[,1:4] = 100 * contributions[,1:4]
          
          diffContributions = contributions$C - contributions$Cworld
          selection = which(abs(diffContributions) >= contributionMinimalDifference & contributions$pvalue <= pvalueThreshold)
          selection = selection[ order(diffContributions[selection], decreasing = TRUE) ]
          
          if (length(selection) > 0) {
            
            outdir = paste0(outDirGeneral, "/", corpus)
            if (!file.exists(outdir)) {
              dir.create(outdir, recursive = TRUE)
            }
            outfile = paste0(outdir, "/", institution, ".pdf")
            
            pdf(outfile, width=10)
          
            l = length(selection)
            #maxRadius = sqrt(median(contributions$Q[selection]))
            sizeConst = 0.01
            
            ymin = min(diffContributions[selection])
            ymax = max(diffContributions[selection])
            if (ymin > 0) {
              ymin = 0
            }
            if (ymax < 0) {
              ymax = 0
            }
            plot(diffContributions[selection], pch=".", col="darkred", main=paste(institution,
                                  "-", corpusTitles[corpus]), ylim=c(ymin, ymax), xlab="Category of references", ylab="Contribution of category (difference with world)")
            abline(h=0, col="grey")
            points(rep(0, l), pch=".", col="darkblue")
            
            usr = par("usr")
            plotWidth = usr[[2]] - usr[[1]]
            plotHeight = usr[[4]] - usr[[3]]
          
            radius = sqrt(contributions$Q[selection])*sizeConst*plotWidth
            radiusWorld = sqrt(contributions$Qworld[selection])*sizeConst*plotWidth
            for (i in 1:l) {
              plotcircle(r=radius[[i]], mid=c(i, diffContributions[selection][[i]]), type="n", col="#FF000099")
              plotcircle(r=radiusWorld[[i]], mid=c(i, 0), type="n", col="#0000FF99")
            }

            text(x = 1:l - 0.2, y = ymin, categoryTitles[rownames(contributions)[selection]], srt=90, pos=4, adj=0.5, offset=0, cex=0.7, col="#444444")
            legend(x=usrFromRelativeX(0), y=usrFromRelativeY(-0.1), fill=c("#FF000099", "#0000FF99"), legend = c(institution, "World"),bty="n", xpd=TRUE, horiz=TRUE)
            
            plotCircleOutsidePlot(x0=usrFromRelativeX(0.8), y0=usrFromRelativeY(-0.13), r=sqrt(5)*sizeConst*plotWidth, col="grey")
            text(usrFromRelativeX(0.78), usrFromRelativeY(-0.13), xpd=TRUE, labels = "5% of references", pos=4, cex=0.8)
            
            dev.off()
          }
        }
      }
    }
  }
}


