export.qm <- function(qmobject, file, style="R") {
  if (style=="R") capture.output(qmobject, file=file)
  else if (style=="PQMethod") {
    pqmethod.output <- function(qmobject) {
      pqout <- as.list(rep(NA, 15))
      # Correlation Matrix Between Sorts
      names(pqout)[[1]] <- "Correlation Matrix Between Sorts"
      pqout[[1]] <- round(cor(qmobject[[2]]), digits=2)
      # Unrotated Factor Matrix
      names(pqout)[[2]] <- "Unrotated Factor Matrix"
      pqout[[2]] <- principal(qmobject[[2]], nfactors=8, rotate="none")
      # Cumulative Communalities Matrix
      names(pqout)[[3]] <- "Cumulative Communalities Matrix"
      comm <- data.frame(matrix(as.numeric(NA), ncol=8, 
                                nrow=length(qmobject[[2]])))
      for (i in 1:8) {
        comm[[i]] <- principal(qmobject[[2]], nfactors=i, 
                               rotate="none")$communality
      }
      pqout[[3]] <- round(comm, digits=2)
      # Factor Matrix with an X Indicating a Defining Sort
      names(pqout)[[4]] <- "Factor Matrix and Defining Sorts"
      pqout[[4]] <- list(round(qmobject$"Q-sort factor loadings", digits=2),       qmobject$"Flagged Q-sorts")
      names(pqout[[4]]) <- c("Q-sort factor loadings", "Flagged Q-sorts")
      # Free Distribution Data Results
      names(pqout)[[5]] <- "Free Distribution Data Results -- not calculated"
      # Factor Scores with Corresponding Ranks
      names(pqout)[[6]] <- "Factor Scores (z-scores)"
      pqout[[6]] <- round(qmobject$"Statement z-scores", digits=2)
      # Correlations Between Factor Scores
      names(pqout)[[7]] <- "Correlations Between Factor Scores"
      pqout[[7]] <- round(qmobject$"Factor characteristics"[[2]], digits=2)
      # Factor Scores -- For Factor *
      names(pqout)[[8]] <- "Factor Scores "
      nfactors <- length(qmobject$"Statement rounded scores")
      pqout[[8]] <- as.list(1:nfactors)
      for (i in 1:nfactors) {
        names(pqout[[8]])[i] <- paste0("-- For Factor ", i)
        pqout[[8]][[i]] <- round(qmobject$"Statement z-scores"[order(qmobject$"Statement z-scores"[i], decreasing=T), c(i, i)][1], digits=2)
      }
      # Descending Array of Differences Between Factors x and y
      names(pqout)[[9]] <- "Descending Array of Differences Between Factors "
      comparisons <- combn(nfactors, 2, simplify=F)
      pqout[[9]] <- as.list(1:length(comparisons))
      for (i in 1:length(comparisons)) {
        names(pqout[[9]])[i] <- paste(comparisons[[i]], collapse=" and ", sep="")
        zsc <- qmobject$"Statement z-scores"[comparisons[[i]]]
        dif <- qmobject[[8]][c(names(qmobject[[8]])[grep(paste(paste(comparisons[[i]][1], comparisons[[i]][2], sep=".*"), paste(comparisons[[i]][2], comparisons[[i]][1], sep=".*"), sep="|"),names(qmobject[[8]]))],"dist.and.cons")]
        dad <- cbind(zsc, dif)
        pqout[[9]][[i]] <- format(dad[order(dad[3], decreasing = T), ], digits=2)
      }
      # Factor Q-Sort Values for Each Statement
      names(pqout)[[10]] <- "Factor Q-Sort Values for Each Statement"
      pqout[[10]] <- qmobject$"Statement rounded scores"
      # Factor Q-Sort Values for Statements sorted by Consensus vs. Disagreement (Variance across Factor Z-Scores)
      names(pqout)[[11]] <- "Factor Q-Sort Values for Statements sorted by Consensus vs. Disagreement (Variance across Factor Z-Scores)"
      zsc <- qmobject$"Statement z-scores"
      zsc.ord <- order(apply(zsc, 1, var))
      pqout[[11]] <- qmobject$"Statement rounded scores"[zsc.ord,]
      # Factor Characteristics
      names(pqout)[[12]] <- "Factor Characteristics"
      fch <- t(qmobject$"Factor characteristics"[[1]])
      rownames(fch) <- c(
        "Average reliability coefficient", 
        "Number of loading Q-sorts", 
        "Eigenvalues", 
        "Percentage of explained variance", 
        "Composite reliability", 
        "Standard error of factor scores")
      pqout[[12]] <- round(fch, digits=2)
      # Standard Errors for Differences in Factor Z-Scores
      names(pqout)[[13]] <- "Standard Errors for Differences in Factor Z-Scores"
      pqout[[13]] <- qmobject$"Factor characteristics"[[3]]
      # Distinguishing Statements for Factor *
      names(pqout)[[14]] <- "Distinguishing Statements "
      dc <- qmobject$"Distinguishing and consensus statements"
      zsc <- qmobject$"Statement z-scores"
      pqout[[14]] <- as.list(1:nfactors)
      for (i in 1:nfactors) {
        names(pqout[[14]])[i] <- paste0("for Factor ", i)
        d <- grep(paste0("f",i, "|all"), dc$dist.and.cons)
        pqout[[14]][[i]] <- cbind(round(zsc[d, ], digits=2),dc[d, c(1,1+(2*(1:length(comparisons))))])
      }
      # Consensus Statements  --  Those That Do Not Distinguish Between ANY Pair of Factors.
      names(pqout)[[15]] <- "Consensus Statements  --  Those That Do Not Distinguish Between ANY Pair of Factors."
      dcon <- which(dc$dist.and.cons == "Consensus")
      pqout[[15]] <- cbind(round(qmobject$"Statement z-scores"[dcon, ], digits=2),dc[dcon, c(1,1+(2*(1:length(comparisons))))])
      return(pqout)
    }
    capture.output(pqmethod.output(qmobject), file=file)
  }
}