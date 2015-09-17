# Compute the similarity matrix.

rm(list=ls())
invisible(gc())

anneeMin = 2008
anneeMax = 2012

library(ROracle)

options(stringsAsFactors = FALSE)

fracMode = TRUE
source("common.R")

print(system.time({
  
  drv = dbDriver("Oracle")
  
  cat("Connecting to database...\n")
  con = dbConnect(drv, dbname="GINKGO.BUP1", username="interdis", password="XXX")
  
    
  cat("Sending query...\n")
  rawdata = dbGetQuery(con, paste0("
    WITH tmp_wos AS (
      SELECT DISTINCT
      CODE_DOC_CITANT AS CLEUT_CITANT,
      CODE_SPE_CITANT AS CAT_COD_CITANT,
      CODE_DOC_CITE AS CLEUT_CITE,
      CODE_SPE_CITE AS CAT_COD_CITE
      
      FROM ginkgo_dtm_qry.interd_doc_cit_spe_str
      WHERE CODE_TYPE_DOC_CITANT IN ('@','L','N','R')
      AND ANNEE_P_CITANT >= ", anneeMin, "
      AND ANNEE_P_CITANT <= ", anneeMax, "
    ),
    tmp_wos_nbcat AS (
      SELECT CAT_COD_CITANT, CLEUT_CITANT, CLEUT_CITE, count(*) AS nbCat
      FROM tmp_wos
      GROUP BY CAT_COD_CITANT, CLEUT_CITANT, CLEUT_CITE
    )
    SELECT tmp_wos.CAT_COD_CITANT, tmp_wos.CAT_COD_CITE, sum(1/nbCat) AS count
    FROM tmp_wos, tmp_wos_nbcat
    WHERE tmp_wos.CLEUT_CITANT = tmp_wos_nbcat.CLEUT_CITANT
    AND tmp_wos.CLEUT_CITE = tmp_wos_nbcat.CLEUT_CITE
    AND tmp_wos.CAT_COD_CITANT = tmp_wos_nbcat.CAT_COD_CITANT
    GROUP BY tmp_wos.CAT_COD_CITANT, tmp_wos.CAT_COD_CITE
    ORDER BY tmp_wos.CAT_COD_CITANT, tmp_wos.CAT_COD_CITE
    "))

  cat("Converting the data...\n")
  categoriesInWorld = sort(unique(rawdata$CAT_COD_CITANT))
  
  # Remove cited categories that are never citing.
  rawdata = rawdata[ rawdata$CAT_COD_CITE %in% categoriesInWorld, ]
  
  M = matrix(0, nrow=length(categoriesInWorld), ncol=length(categoriesInWorld), dimnames=list(categoriesInWorld, categoriesInWorld))
  
  for (catCitant in categoriesInWorld) {
    w = rawdata$CAT_COD_CITANT == catCitant
    M[ catCitant, rawdata$CAT_COD_CITE[w] ] = rawdata$COUNT[w]
  }
  
  simMat = M * 0
  for (i in 1:nrow(simMat)) {
    for (j in 1:nrow(simMat)) {
      simMat[[i,j]] = (M[i,] %*% M[j,]) / (sqrt(sum(M[i,]^2)) * sqrt(sum(M[j,]^2)))
    }
  }
  
  cat("Writing results...\n")
  write.table(simMat, file=paste0(dataDir, "/category_similarity_matrix.txt"))
  
  dbDisconnect(con)
}))

cat("Done.\n")
