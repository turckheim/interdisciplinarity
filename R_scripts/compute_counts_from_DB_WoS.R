# This script computes from the Oracle OST db the counts of references 
# by category(class) for each article of the db.
# Result: for each category(corpus), a matrix where rows are the articles
# of the corpus and columns are the categories(classes) of references.
# The values in the matrix are the numbers of references of each article in the
# different categorie(classes).
# Whole counts version


rm(list=ls())
invisible(gc())

anneeMin = 2008
anneeMax = 2012

library(ROracle)

options(stringsAsFactors = FALSE)

source("common.R")
source("load_similarity_matrix.R")

print(system.time({

  outdir = paste0(dataDir, "/world")
  if (!file.exists(outdir)) {
  dir.create(outdir, recursive = TRUE)
  }

  drv = dbDriver("Oracle")
  
  cat("Connecting to database...\n")
  con = dbConnect(drv, dbname="GINKGO.BUP1", username="interdis", password="XXX")
  
  for (category in categories) {
    cat("Category:", category, "\n")
    
    cat("Sending query...\n")
    rawdata = dbGetQuery(con, paste0("
    WITH tmp_wos AS (
      SELECT DISTINCT
      CODE_DOC_CITANT AS CLEUT_CITANT,
      CODE_DOC_CITE AS CLEUT_CITE, 
      CODE_SPE_CITE AS CAT_COD_CITE
      
      FROM ginkgo_dtm_qry.interd_doc_cit_spe_str
      WHERE CODE_TYPE_DOC_CITANT IN ('@','L','N','R')
      AND CODE_SPE_CITANT = '", category, "'
      AND ANNEE_P_CITANT >= ", anneeMin, "
      AND ANNEE_P_CITANT <= ", anneeMax, "
    ),
    tmp_articles_references AS (
      SELECT DISTINCT CLEUT_CITANT, CLEUT_CITE
      FROM tmp_wos
    ),
    tmp_nb_references AS (
      SELECT CLEUT_CITANT, count(*) AS nbReferences
      FROM tmp_articles_references
      GROUP BY CLEUT_CITANT
    ),
    tmp_wos_valides AS (
      SELECT tmp_wos.*
      FROM tmp_wos, tmp_nb_references
      WHERE tmp_wos.CLEUT_CITANT = tmp_nb_references.CLEUT_CITANT
      AND tmp_nb_references.nbReferences >= 3
    )
    SELECT CLEUT_CITANT, CAT_COD_CITE , count(*) AS count
    FROM tmp_wos_valides
    GROUP BY (CLEUT_CITANT, CAT_COD_CITE)
    ORDER BY CLEUT_CITANT, CAT_COD_CITE
    "))
    
    cat("Converting the data...\n")
    articles = unique(rawdata$CLEUT_CITANT)
    countMatrix <- matrix(0, nrow=length(articles), ncol=length(categories), dimnames=list(articles, categories))
    for (j in 1:length(categories)) {
      citedCategory <- categories[[j]]
      indices <- which(rawdata$CAT_COD_CITE == citedCategory)
      countMatrix[rawdata$CLEUT_CITANT[indices], citedCategory] <- rawdata$COUNT[indices]
    }
    
    cat("Writing results...\n")
    write.table(countMatrix, file=paste0(outdir, "/", category, ".txt"))
  }
  
  dbDisconnect(con)
}))

cat("Done.\n")
