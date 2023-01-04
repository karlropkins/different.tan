############################################
#' @title Pareto Enrichment Plots
############################################

#' @name plot_ParetoEnrichment
#' @description [to be added]
#' @param x [to be added]
#' @param y [to be added]
#' @param cut [to be added]
#' @param data [to be added]
#' @param ... [to be added]
#' @param min.bin [to be added]
#' @param drop.nas [to be added]
#' @param order [to be added]
#' @param level.nmax [to be added]

#splatted function
#########################
#' @export

############################
# jobs
############################

# this needs tidying and documenting....

plot_ParetoEnrichment <- 
function (x, y=NULL, cut=0.8, data = NULL, ..., 
                                  min.bin=1, drop.nas = TRUE, order=FALSE, level.nmax=NULL) 
{
    
    #to do
    #check which formals are used 
    #think about group and cond
    #think about cut currently only expecting one...
    #think about key
    #tidy code so we can flip axes
    #tidy code so we can update grid and line elements
    #think about other options to display results
    #overlapping histograms?
    
    
    #setup
    argnames <- names(as.list(match.call(expand.dots = TRUE)[-1]))
    arguments <- as.list(match.call()[-1])
    extra.args <- list(...)
    
    #names
    #currently no groups or cond
    x.name <- as.character(arguments)[argnames == "x"]
    y.name <- as.character(arguments)[argnames == "y"]
    
    #name updates
    y.name <- paste("cutPareto(", y.name, ", ", cut, ") Enrichment", sep="")
    
    #currently no key
    #    #key
    #    extra.args <- do.call(listLoad, listUpdate(extra.args, list(load = "key")))
    #    key <- extra.args$key
    #    if(is.null(key)) key=TRUE
    #    extra.args <- extra.args[!names(extra.args) %in% c("key")]
    
    #data source
    if (!is.null(data)) 
        data <- as.data.frame(data)
    env <- parent.frame()
    #currently groups and cond not checked for when making df
    df <- list(x = eval(substitute(x), data, env), y = eval(substitute(y), 
                                                            data, env))
    ref <- lapply(df, length)
    df <- as.data.frame(df[ref > 0])
    
    calc.df <- calcPareto(df$y, output="all") 
    df <- cbind(df[calc.df$ind,], calc.df)
    
    #return(df)
    
    #note currently flipping axes
    #might want to think about this
    
    #set default col
    
    col <- colHandler(z=10, zlim=c(1, 10), col.regions="Spectral")
    
    plot.args <- listUpdate(list(x=x~y, data=df, xlab=y.name, ylab=x.name, origin=0, col=col), extra.args)
    plt <- do.call(xyplot, plot.args)
    
    #rework outputs
    #this assumes 
    #above x and y structure - y factor x numeric
    #cut is currently in panel but assuming only one panel...
    #   so we can look at multiple panel
    #   so could simplify code if we decide definitely not to do this
    
    #doing this is way so cut is in-panel not all data  
    
    min.max <- c() 
    rt <- rep(0, length(levels(plt$panel.args[[1]]$y)))
    for(i in 1:length(plt$panel.args)){
        all.data <- data.frame(x=plt$panel.args[[i]]$x, y=plt$panel.args[[i]]$y) 
        cum.sum <- cumsum(all.data$x)
        cum.sum <- cum.sum/max(cum.sum, na.rm=TRUE)
        hi.cut <- subset(all.data, cum.sum <= cut)
        
        y <- summary(all.data$y, maxsum=length(levels(all.data$y)))
        y[y < min.bin] <- NA
        
        y <- y/sum(y, na.rm=TRUE)
        #levels should be same
        y2 <- summary(hi.cut$y, maxsum=length(levels(all.data$y)))
        y2 <- y2/sum(y2, na.rm=TRUE)
        y <- y2/y
        
        plt$panel.args[[i]]$x <- as.vector(y)
        min.max <- c(min(c(min.max, as.vector(y)), na.rm=TRUE), max(c(min.max, as.vector(y)), na.rm=TRUE))
        plt$panel.args[[i]]$y <- factor(names(y), levels=levels(plt$panel.args[[i]]$y))
        y[is.na(y)] <- 0
        rt <- rt+y
    }
    
    border <- (min.max[2]-min.max[1])*0.05
    plt$x.limits <- min.max + c(-border, border)
    if("origin" %in% names(plt$panel.args.common))
        plt$x.limits[1] <- 0-border
    plt$panel.args.common$horizontal <- TRUE
    
    if(order){
        plt$y.limits <- plt$y.limits[order(rt)]
        for(i in 1:length(plt$panel.args)){
            plt$panel.args[[i]]$x <- plt$panel.args[[i]]$x[order(rt)]
            plt$panel.args[[i]]$y <- as.character(plt$panel.args[[i]]$y[order(rt)])
            plt$panel.args[[i]]$y <- factor(plt$panel.args[[i]]$y, levels=plt$y.limits)
        }
        rt <- rt[order(rt)]
    }
    if(drop.nas & any(rt==0)){
        
        #this might not completely work
        #if enrichment is zero not NA...
        #if rt contains NAs...
        
        plt$y.limits <- plt$y.limits[rt!=0]
        for(i in 1:length(plt$panel.args)){
            plt$panel.args[[i]]$x <- plt$panel.args[[i]]$x[rt!=0]
            plt$panel.args[[i]]$y <- as.character(plt$panel.args[[i]]$y[rt!=0])
            plt$panel.args[[i]]$y <- factor(plt$panel.args[[i]]$y, levels=plt$y.limits)
        }
        rt<-rt[rt!=0]
    }
    
    if(is.numeric(level.nmax)){
        temp <- if(order) rev(rev(1:length(plt$y.limits))[1:level.nmax]) else 1:level.nmax
        if(length(plt$y.limits > level.nmax)){
            plt$y.limits <- plt$y.limits[temp]
            for(i in 1:length(plt$panel.args)){
                plt$panel.args[[i]]$x <- plt$panel.args[[i]]$x[temp]
                plt$panel.args[[i]]$y <- as.character(plt$panel.args[[i]]$y[temp])
                plt$panel.args[[i]]$y <- factor(plt$panel.args[[i]]$y, levels=plt$y.limits)
            }
            #rt<-rt[rt!=0]
            if(!order)
                warning(paste("In ParetoEnrichment(): Note, just showing first ", 
                   level.nmax, " levels\n\tReplot with order=TRUE if you want highest", sep=""), 
                   call.=FALSE)
            
        }
    }
    
    plt$panel <- function(...){
        panel.grid(-1,-1)
        panel.barchart(...)
        panel.abline(v=1,col="red",lty=2)                       
    }
    if(length(plt$panel.args[[1]]$x) > 40)
        warning(paste("In ParetoEnrichment(): Large '", x.name, 
            "' level number? \n\tMaybe refactorise, or apply min.bin, order and/or level.nmax?", 
            sep=""), call.=FALSE)
    
    plt
    
}



