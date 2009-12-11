bfast <- function(Yt, h = 0.15, max.iter = NULL, breaks = NULL)
{
    ti <- time(Yt)
    f <- frequency(Yt)      # on cycle every f time points (seasonal cycle)
    if(f==1)
        stop("Not a seasonal time series")

    ## return value
    output <- list()

    # Start the iterative procedure and for first iteration St=decompose result
    St <- stl(Yt, "periodic")$time.series[, "seasonal"]
    Tt <- 0

    D <- seasonaldummy(Yt)
    D[rowSums(D)==0,] <- -1

    # number/timing of structural breaks in the trend/seasonal component
    Vt.bp <- 0
    Wt.bp <- 0 
    CheckTimeTt <- 1
    CheckTimeSt <- 1
    
    i <- 0
    while ( (!identical(CheckTimeTt,Vt.bp) | !identical(CheckTimeSt,Wt.bp)) & i <= max.iter)
    {
        CheckTimeTt <- Vt.bp
        CheckTimeSt <- Wt.bp

        # TREND
        Vt <- Yt-St
        p.Vt <- sctest(efp(Vt ~ ti, h=h, type= "OLS-MOSUM"))
        if (p.Vt$p.value <=0.05) 
        {
          bp.Vt <- breakpoints(Vt ~ ti, h=h,breaks=breaks)
          nobp.Vt <- is.na(breakpoints (bp.Vt)[1])
        } 
        else 
        {
          nobp.Vt <- TRUE
          bp.Vt <- NA       
        }
        if (nobp.Vt)
        {
            fm0 <- rlm(Vt ~  ti)
            Vt.bp <- 0      # no breaks times
            Tt <- ts(fitted(fm0))     # Data minus trend
            tsp(Tt) <- tsp(Yt)
            ci.Vt <- NA
        } 
        else
        {
            fm1 <- rlm(Vt ~ breakfactor(bp.Vt)/ti)
            ci.Vt <- confint(bp.Vt, het.err = FALSE)
            Vt.bp <- ci.Vt$confint[,2]
            Tt <- ts(fitted(fm1))     # Data minus trend
            tsp(Tt) <- tsp(Yt)
        }
        
        # SEASONAL COMPONENT
        Wt <- Yt-Tt
        
        p.Wt <- sctest(efp(Wt ~ -1+D, h=h, type= "OLS-MOSUM"))      # preliminary test 
#        if (p.Wt$p.value <=0.05) # OR statement 
#        {
            bp.Wt <- breakpoints(Wt ~ -1+D, h=h,breaks=breaks) # Breakpoints in the seasonal component
            nobp.Wt <- is.na(breakpoints (bp.Wt)[1])
#        } 
#        else 
#        {
#            nobp.Wt <- TRUE
#            bp.Wt <- NA       
#        }
        if (nobp.Wt)
        {
            sm0 <- rlm(Wt ~ -1+D)
            St <- ts(fitted(sm0))  #  The fitted seasonal component
            tsp(St) <- tsp(Yt)
            Wt.bp <- 0             # no seasonal breaks
            ci.Wt <- NA
        } 
        else
        {
            sm1 <-rlm(Wt ~ -1+D %in% breakfactor(bp.Wt))
            St <- ts(fitted(sm1))  #  The fitted seasonal component
            tsp(St) <- tsp(Yt)
            ci.Wt <- confint(bp.Wt, het.err = FALSE)
            Wt.bp <- ci.Wt$confint[,2] 
        }
        i <- i+1
        output[[i]] <- list(Tt=Tt,St=St,Nt=Yt-Tt-St,
            Vt=Vt, bp.Vt=bp.Vt, Vt.bp=Vt.bp, ci.Vt=ci.Vt,
            Wt=Wt, bp.Wt=bp.Wt, Wt.bp=Wt.bp, ci.Wt=ci.Wt)
    }
    if (!nobp.Vt) 
    {
      Vt.nrbp <- length(bp.Vt$breakpoints)
      co <- coef(fm1) # final fitted trend model
      Mag <- matrix(NA,Vt.nrbp,3)
      for (r in 1:Vt.nrbp) 
      {
        if (r==1) 
            y1 <- co[1]+co[r+Vt.nrbp+1]*ti[Vt.bp[r]]
        else 
        y1 <- co[1]+co[r]+co[r+Vt.nrbp+1]*ti[Vt.bp[r]]
        y2 <- (co[1]+co[r+1])+co[r+Vt.nrbp+2]*ti[Vt.bp[r]+1]
        Mag[r,1] <- y1
        Mag[r,2] <- y2
        Mag[r,3] <- y2-y1   
      }
      index <- which.max(abs(Mag[,3]))
      m.x <- rep(Vt.bp[index],2)
      m.y <- c(Mag[index,1],Mag[index,2]) #Magnitude position
      Magnitude <- Mag[index,3] # Magnitude of biggest change
      Time <- Vt.bp[index]
    } 
    else 
    {
      m.x <- NA; m.y <- NA
      Magnitude <- 0  # if we do not detect a break then the magnitude is zero
      Time <- NA # if we do not detect a break then we have no timing of the break
      Mag <- 0
    }
    return(structure(list(Yt=Yt,output=output,nobp=list(Vt=nobp.Vt,Wt=nobp.Wt),Magnitude=Magnitude,Mags=Mag,
            Time=Time,jump=list(x=ti[m.x],y=m.y)),class="bfast"))  
}
