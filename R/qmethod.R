qmethod <- function(dataset, nfactors, nstat, nqsorts, rotation="varimax", forced=T, distribution=NA, cor.method="pearson") {
  #threshold for significant values at p-value=.01 and p-value=.05
  thold.01 <- 2.58/sqrt(nstat)
  thold.05 <- 1.96/sqrt(nstat)
  #check that the input data is correct
  if (nqsorts!=ncol(dataset)) stop("Q method input: The number of Q sorts introduced does not match with the number of columns of the data frame or matrix") else if (nstat!=nrow(dataset)) stop("Q method input: The number of statements introduced does not match with the number of rows of the data frame or matrix.") else if (!is.integer(as.matrix(dataset))) stop("Q method input: The data frame or matrix entered has non numeric values.") else {
    cor.data <- cor(dataset, method=cor.method)
    loa <- as.data.frame(unclass(principal(cor.data, nfactors=nfactors, rotate=rotation)$loadings)) #PCA from {psych} for factor loadings
    names(loa) <- paste0("f", 1:length(loa))
    # The following depends on the qmethod functions: qflag, qzscores, qfcharact, qdc
    flagged <- qflag(nqsorts, nstat, loa=loa)
    qmethodresults <- qzscores(dataset, nfactors, nstat, nqsorts, flagged=flagged, loa=loa, forced=forced, distribution=distribution)
    brief <- paste0("Q-method analysis performed on ", date(), ". Original data: ", nstat, " statements, ", nqsorts, " Q-sorts. Number of factors: ",nfactors,". Rotation: ", rotation, ". Automatic flagging. (Correlation coefficient: ", cor.method, ").")
    qmethodresults[[1]] <- brief
    qmethodresults[[8]] <- qdc(dataset, nfactors, zsc=qmethodresults[[5]], sed=as.data.frame(qmethodresults[[7]][[3]]))
    names(qmethodresults)[8] <- "Distinguishing and consensus statements"
  }
  print(qmethodresults$Summary)
  return(qmethodresults)
}