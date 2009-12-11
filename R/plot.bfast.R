plot.bfast <- function(x,type=c("components","all","data","seasonal","trend","noise"), sim=NULL, largest=FALSE, main, ...)
{                                                                                                      
    type <- match.arg(type)
    realdata <- is.null(sim)
    Trend.bp <- !x$nobp$Vt
    if(type=="largest" & !Trend.bp)
        stop("No trend breakpoints")
    title <- !missing(main)
    niter <- length(x$output)
    out <- x$output[[niter]]
    Tt <- out$Tt
    St <- out$St
    noise <- out$Nt

    if(type=="data")
    # Plot original data
    {
        if(!title)
            main <- "Yt"
        plot(x$Yt,main=main,...)
    }
    else if(type=="components")
    # Plot components from final iteration
    {
        ft  <- cbind(seasonal= out$St, trend=out$Tt,remainder=out$Nt)
        tsp(ft) <- tsp(x$Yt)
        ft <- list(time.series = ft)
        if(!title) 
            main <- paste('no. iterations to estimate stable breakpoints:',niter) 
        seasonal(ft,out,sim=sim,main=main) # plotting function based on STL structure. How can we use e.g. plot.seasonal ?
    }
    else if(type=="noise")
    # Plot noise component
    {
        require(forecast)
        if(!title)
            main <- "Noise component"
        tsdisplay(noise, main=main, ...)
    }
    else
    {
        if(type=="all")
        {
            idx <- 1:niter
            opar <- par(mfrow=c(2,niter))
        }
        else
            idx <- niter
        for(i in idx)
        {
            out <- x$output[[i]]
            
            if(type != "seasonal")
            {
                # PLOT TREND COMPONENT
                if(type=="trend" & !title)
                    main <- "Trend component"
                else if(!title)
                    main <- paste("Iteration ",i,": Trend",sep='')
                plot(out$Vt,main=main,ylab="Vt",...)
                lines(out$Tt, col = 4)
                if (Trend.bp)
                {
                    lines(out$bp.Vt)
                    lines(out$ci.Vt)
                    legend("topright",paste("Time of BP(s)",paste(out$Vt.bp,collapse=",")),col=2)
                }
                if (!realdata)
                {
                    lines(sim$time.series[,'abrupt'], col=1, lty=2) # original data
                    legend("bottomleft",c("estimated","simulated"), lty=c(1,2),col=1)
                }
                if(largest)
                {
                    # Mark largest jump
                    legend("bottomright",c("Magnitude of most sign change"), lty=c(1),col=6)
                    lines(x$jump,col=6)
                    points(x$jump, pch=14, cex=1,col=6)
                }
            }
            
            if(type != "trend")
            {
                # PLOT SEASONAL COMPONENT
                if(type=="seasonal" & !title)
                    main <- "Seasonal component"
                else if(!title)
                    main <- paste("Iteration ",i,": Seasonal",sep='')
                plot(out$Wt,main=main,ylab="Wt",...)
                lines(out$St,col=2)
                Seas.bp <- !x$nobp$Wt
                if(Seas.bp)
                {
                    lines(out$bp.Wt)
                    lines(out$ci.Wt)
                    legend("topright",paste("Time of BP(s)",paste(out$Wt.bp,collapse=",")),col=2)
                }
                if (!realdata)
                {
                    lines(sim$time.series[,'seasonal'], col=1, lty=2) # orginal data
                    legend("bottomleft",c("first run seasonality","first run estimated","simulated"), lty=c(1,1,2),col=c(1,2,1))
                }  
            }
        }
        if(type=="all")
            par(opar)
    }
                   
}
