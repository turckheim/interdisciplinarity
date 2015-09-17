# This script computes from the Oracle OST db the counts of references 
# by category(class) for each article of an institution.
# Result: for each institution, for each category(corpus), a matrix where rows  
# are articles in corpus and columns are categories(classes) 
# of references.
# The values in the matrix are the numbers of references of each article 
# in the different categories(classes).
# Whole counts version

rm(list=ls())
invisible(gc())

library(ROracle)

options(stringsAsFactors = FALSE)

source("common.R")
source("load_similarity_matrix.R")

anneeMin = 2008
anneeMax = 2012

categoriesToConsider = "all"
#categoriesToConsider = c("AA", "BU", "DW", "DY", "EA", "EC")

minimumNumberOfArticles = 50

print(system.time({

  generalOutDir = paste0(dataDir, "/input")

  drv = dbDriver("Oracle")
  
  cat("Connecting to database...\n")
  con = dbConnect(drv, dbname="GINKGO.BUP1", username="interdis", password="XXX")
  
  cat("Making list of institutions...\n")
  rs = dbGetQuery(con, paste0("
  SELECT libelle_institution
  FROM interdis.view_institutions_a_considerer
  ORDER BY libelle_institution"))
  institutions = rs[,1]
  
  for (institution in institutions) {
    cat("Institution:", institution, "\n")
      
    cat("Sending query...\n")
    rawdataAllCat = dbGetQuery(con, paste0("
    WITH tmp_wos AS (
      SELECT DISTINCT
      CODE_DOC_CITANT AS CLEUT_CITANT,
      CODE_SPE_CITANT AS CAT_COD_CITANT,
      CODE_DOC_CITE AS CLEUT_CITE,
      CODE_SPE_CITE AS CAT_COD_CITE
      
      FROM ginkgo_dtm_qry.interd_doc_cit_spe_str, interdis.snap_articles_institutions
      WHERE CODE_TYPE_DOC_CITANT IN ('@','L','N','R')
      AND interd_doc_cit_spe_str.code_doc_citant = snap_articles_institutions.cleut
      AND snap_articles_institutions.institution = '", institution, "'
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
    SELECT CAT_COD_CITANT, CLEUT_CITANT, CAT_COD_CITE , count(*) AS count
    FROM tmp_wos_valides
    GROUP BY (CAT_COD_CITANT, CLEUT_CITANT, CAT_COD_CITE)
    ORDER BY CAT_COD_CITANT, CLEUT_CITANT, CAT_COD_CITE
    "))
    
    categoriesInInstitution = unique(rawdataAllCat$CAT_COD_CITANT)
    
    for (category in categoriesInInstitution) {
      if (categoriesToConsider[[1]] != "all" && ! category %in% categoriesToConsider) next
      
      rawdata = rawdataAllCat[ rawdataAllCat$CAT_COD_CITANT == category, ]
      articles = unique(rawdata$CLEUT_CITANT)
      
      if (length(articles) >= minimumNumberOfArticles) {
        cat("Converting data for category:", category, " - Institution:", institution, "\n")
        
        countMatrix = matrix(0, nrow=length(articles), ncol=length(categories), dimnames=list(articles, categories))
        for (j in 1:length(categories)) {
          citedCategory = categories[[j]]
          indices = which(rawdata$CAT_COD_CITE == citedCategory)
          countMatrix[rawdata$CLEUT_CITANT[indices], citedCategory] = rawdata$COUNT[indices]
        }
      
        #cat("Writing results...\n")
        outdir = paste0(generalOutDir, "/", category)
        if (!file.exists(outdir)) {
          dir.create(outdir, recursive = TRUE)
        }
        write.table(countMatrix, file=paste0(outdir, "/", institution, ".txt"))
        
        #oldMatrix = as.matrix(read.table(paste0(dataDir, "/input - Copie/", category, "/", institution, ".txt")))
        #stopifnot(oldMatrix == countMatrix)
        
      } else {
        #cat("Not enough articles, therefore I am not storing the matrix.\n")
      }
    }
  }
  
  dbDisconnect(con)
}))

cat("Done.\n")
