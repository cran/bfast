seasonal <- function (x,out,sim = NULL, labels = colnames(X), set.pars = list(mar = c(0,
    6, 0, 6), oma = c(6, 0, 4, 0), tck = -0.01, mfrow = c(nplot,
    1)), main = NULL, range.bars = FALSE, ..., col.range = "light gray")
{
    sers <- x$time.series
    ncomp <- ncol(sers)
    data <- drop(sers %*% rep(1, ncomp))
    X <- cbind(data, sers)
    colnames(X) <- c("data", colnames(sers))
    nplot <- ncomp + 1
    if (range.bars)
        mx <- min(apply(rx <- apply(X, 2, range), 2, diff))
    if (length(set.pars)) {
        oldpar <- do.call("par", as.list(names(set.pars)))
        on.exit(par(oldpar))
        do.call("par", set.pars)
    }
    for (i in 1:nplot) {
        plot(X[, i], col='red'
        #,ylim = range(X[,1]) 
        , ylim = if (i==1 | i==3) range(X[,1]) 
        else range(X[,i],sim$time.series[,i-1],na.rm=TRUE)
        , type = if (i < nplot)
            "l"
        else "h", xlab = "", ylab = "", axes = FALSE, ...)
        if (range.bars) {
            dx <- 1/64 * diff(ux <- par("usr")[1:2])
            y <- mean(rx[, i])
            rect(ux[2] - dx, y + mx/2, ux[2] - 0.4 * dx, y -
                mx/2, col = col.range, xpd = TRUE)
        }
        if (i == 1 && !is.null(main)) {
            title(main, line = 2, outer = par("oma")[3] > 0)
            lines(X[,i],col='black',type='l')  # Simulated data input = Estimate data More logical if this is painted black
            if (!is.null(sim)) {
              lines(X[,i+1]+X[,i+2],col='red',type='l')  # Seasonal + Trend signal
              legend("bottom",c("input","estimated seasonal + trend "),col=c('black','red'),lty=1)
            }
        }
        if (i == 2) {
            lines(sim$time.series[,'seasonal'],col='black')
            lines(out$bp.Wt)
            lines(out$ci.Wt)
        }
        if (i == 3) {
            lines(sim$time.series[,'abrupt'] ,col='black')
            lines(out$bp.Vt)
            lines(out$ci.Vt)
        }
        if (i == nplot) {
            abline(h = 0)
            lines(sim$time.series[,'remainder'],col='black')
        }
        box()
        right <- i%%2 == 0
        axis(2, labels = !right)
        axis(4, labels = right)
        axis(1, labels = i == nplot)
        mtext(labels[i], side = 2, 3)
    }
    mtext("Time", side = 1, line = 3)
    invisible()
}
