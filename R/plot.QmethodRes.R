plot.QmethodRes <- function(x, 
                            xlab='z-scores', ylab='statements',
                            pchlist=NULL, colours=NULL,
                            fnames=NULL, legend=T, ...) {
  lowlim <- floor(min(x$"Statement z-scores"[[1]]))
  highlim <- ceiling(max(x$"Statement z-scores"))
  if (is.null(pchlist)) pchlist <- c(1, 2, 0, 5, 6, 16, 17, 15, 18, 21, 24, 23, 22, 3, 4, 7, 8, 9)
  nfactors <- length(x$"Statement z-scores")
  if (is.null(colours)) colours <- rainbow(length(x$"Statement z-scores"))
  if (is.null(fnames)) fnames <- paste0("Factor ", 1:nfactors)
  dotchart(x$"Statement z-scores"[[1]], lcolor=grey(0.4),
           xlim=c(lowlim, highlim),
           ylab=ylab, xlab=xlab, axis=NULL,
           pch=pchlist[[1]], color=colours[[1]], ...)
  for (i in 2:nfactors){
    points(x=x$"Statement z-scores"[[i]], 1:length(x$"Statement z-scores"[[i]]), pch = pchlist[i], type = "p", col=colours[[i]], ...)
  }
  axis(side=2, at=1:nrow(x$"Statement z-scores"), 
       labels=rownames(x$"Statement z-scores"), 
       las=1, tick=F, line=-0.5, ...)
  abline(v=seq(from=lowlim, to=highlim, by=0.5), col=grey(0.6), lty=3)
  if (legend) {
    legend('bottomright', 
           legend=fnames, 
           col=colours[1:nfactors], 
           pch=pchlist[1:nfactors], 
           bty="n")
  }
}