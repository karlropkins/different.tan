############################################
#' @title plot_Pareto
############################################

#' @name plot_Pareto 
#' @description  In-development plots functions intended for use with VERSS data. 
#' @param y (numeric vector), measurements to plotted as PartoPlot Y-axis terms. In VERSS 
#' analysis these are typically emission measurements from passing vehicles, but can be 
#' any data-series where the frequency of occurrence of \code{y} is of interest. 
#' @param groups (optional) sub-sampling term for grouping measurements within a plot.  
#' @param cond (optional) sub-sampling term for plotting data as separate panels. 
#' @param data (optional, data.frame or similar), source of \code{y}, \code{groups},
#' \code{cond}, etc.
#' @param x (optional, number vector), by default, the index of \code{y} values, lowest 
#' to highest.
#' @param ... Passed to lattice and loa plot functions (and used if possible).
#' @param panel lattice plot argument
#' @param drop.nas Remove NAs before plotting.
#' @param Pareto (Character) Pareto plot type, options: 'Pareto' standard Pareto; 
#' 'cum.Pareto' cumulative Pareto; 'normal.cum.Pareto' cumulative Pareto expressed 
#' as a proportion; and, 'percent.cum.Pareto' cumulative Pareto expressed 
#' as a percentage.
#' @returns \code{lattice} object plot. 


##########################
#to do 
##########################

# add references??? 

###########################
#to check
###########################

##not sure this is removing nas!
##needs references adding 



#splatted function
#version v0.1.2 kr 
#' @export
plot_Pareto <- 
function (y, groups = NULL, cond = NULL, data = NULL, x = NULL,  
          ..., panel=panel.ParetoPlot, drop.nas = TRUE, 
          Pareto = "normal.cum.Pareto") 
{
    
    #to do
    #check args are aligned 
    #e.g. does drop.nas link into drop.unused.levels
    #     and how should it do this???
    
    
    #setup
    argnames <- names(as.list(match.call(expand.dots = TRUE)[-1]))
    arguments <- as.list(match.call()[-1])
    extra.args <- list(...)
    
    #names
    x.name <- as.character(arguments)[argnames == "x"]
    y.name <- as.character(arguments)[argnames == "y"]
    groups.name <- as.character(arguments)[argnames == "groups"]
    cond.name <- as.character(arguments)[argnames == "cond"]
    
    #key
    extra.args <- do.call(listLoad, listUpdate(extra.args, list(load = "key")))
    key <- extra.args$key
    if(is.null(key)) key=TRUE
    extra.args <- extra.args[!names(extra.args) %in% c("key")]
    
    #data source
    if (!is.null(data)) 
        data <- as.data.frame(data)
    env <- parent.frame()
    df <- list(x = eval(substitute(x), data, env), 
               y = eval(substitute(y), data, env), 
               groups = eval(substitute(groups), data, env), 
               cond = eval(substitute(cond), data, env))
    ref <- lapply(df, length)
    df <- as.data.frame(df[ref > 0])
    
    calc.df <- calcPareto(df$y, output="all") 
    df <- cbind(df[calc.df$ind,], calc.df)
    df$y <- df[,Pareto]
    y.name <- paste(Pareto, "(", y.name, ")", sep="")
    
    #add x if not supplied
    if (is.null(df$x)){
        df$x <- c(1:length(df$y))
        if(length(x.name)<1){
            x.name <- "count"
        }
    }
    
    form <- "y~x"
    form <- as.formula(form)
    grps <- if (is.null(df$groups)) 1 else 
        if (is.factor(df$groups)) levels(df$groups) else 
            sort(unique((df$groups)))
    temp <- colHandler(1:20, col.regions = "Spectral")
    temp <- listUpdate(listUpdate(list(col.regions = temp), extra.args), 
                       list(z = 1:length(grps)), ignore = c("zlim"))
    col <- if (length(temp$z) < 2) {
        temp$z <- c(1, 2)
        do.call(colHandler, temp)[2]
    }
    else do.call(colHandler, temp)
    
    plot.list <- list(x=form, data=df, xlab=x.name, ylab=y.name, col=col, 
                      drop.unused.levels=drop.nas, method=Pareto,
                      panel = panel, grid=TRUE, parent=TRUE, line=TRUE, 
                      shadow = TRUE)
    if("groups" %in% names(df))
        plot.list$groups <- df$groups
    plot.list <- listUpdate(plot.list, extra.args)
    
    for (i in c("grid", "line", "shadow", "parent")) 
      plot.list <- do.call(listLoad, listUpdate(plot.list, list(load = i)))
    
    plt <- do.call(xyplot, plot.list)
    
    if("cond" %in% names(df)){
        plt$panel.args.common$cond <- df$cond
        cnds <- if (is.factor(df$cond)) levels(df$cond) else 
            sort(unique((df$cond)))
        if(plot.list$drop.unused.levels){
            ans <- sapply(cnds, function(x) length(df$cond[df$cond==x]))
            cnds <- cnds[c(ans>0)]
        } 
        for(i in 1:length(cnds)){
            plt$panel.args[[i]] <- plt$panel.args[[1]]
            plt$panel.args[[i]]$local.y <- rep(0, length(plt$panel.args[[i]]$y))
            temp <- plt$panel.args[[i]]$y
            if(grepl("cum", tolower(plt$panel.args.common$method)))
                temp <- c(temp[1], diff(temp))
            plt$panel.args[[i]]$local.y[df$cond==cnds[i]] <- temp[df$cond==cnds[i]]
        }
        plt$condlevels <- list(cnds)
        plt$index.cond <- list(1:length(cnds))
        #strip could be set up user
        if(is.logical(plt$strip) && !plt$strip)
            plt$strip <- "strip.default"
    } else {
        plt$panel.args[[1]]$local.y <- plt$panel.args[[1]]$y
        if(grepl("cum", tolower(plt$panel.args.common$method)))
            plt$panel.args[[1]]$local.y <- c(plt$panel.args[[1]]$local.y[1], 
                                             diff(plt$panel.args[[1]]$local.y))
    }
    
    #log method?
    plt$loa <- list(results = df)
    
    #make key if requested
    if("groups" %in% names(df) & isGood4LOA(key)){
        
        grps <- rev(levels(as.factor(df$groups)))
        col <- rev(col)
        #col already declared - line col
        
        if(isGood4LOA(plt$panel.args.common$shadow)){
            temp <- listUpdate(listUpdate(list(border=FALSE), extra.args), 
                               plt$panel.args.common$shadow, ignore="groups")
            if(!"alpha" %in% names(temp))
                temp$alpha <- 0.5
            shadow.col <- do.call(colHandler, listUpdate(temp, list(z=1:length(col), 
                                                                    col.regions=col)))
            
        }
        
        my.key <- list(space="right", title=if ("main" %in% names(key)) key$main else groups.name,
                       cex.title=1, adj=0, between=1)
        
        if(isGood4LOA(plt$panel.args.common$line) & !isGood4LOA(plt$panel.args.common$shadow)){
            my.key$lines <- list(col=col, size=2) 
            if(!is.null(plt$panel.args.common$lty)) my.key$lines$lty <- plt$panel.args.common$lty
            if(!is.null(plt$panel.args.common$lwd)) my.key$lines$lwd <- plt$panel.args.common$lwd
        }
        if(isGood4LOA(plt$panel.args.common$shadow) & isGood4LOA(plt$panel.args.common$line))
            my.key$rect <- list(col=shadow.col, border=col, size=2) 
        if(isGood4LOA(plt$panel.args.common$shadow) & !isGood4LOA(plt$panel.args.common$line))
            my.key$rect <- list(col=shadow.col, border=FALSE, size=2) 
        my.key$text <- list(grps)
        my.key$rep <- TRUE
        
        key <- listUpdate(my.key, key)
        plt$legend <- list(right=list(fun="draw.key", args=list(key=key, draw=FALSE)))
        
        #######################
        #key needs more work
        #######################
        
        if(key$space != "right"){
            names(plt$legend)[1] <- key$space 
            if(key$space == "inside"){
                if("x" %in% names(key))
                    plt$legend$inside$args$x <- key$x
                if("y" %in% names(key))
                    plt$legend$inside$args$y <- key$y
            }
        }
        
    }
    plot(plt)
    invisible(plt)
}


#local functions for now

panel.ParetoPlot <- function(...){
  
  #set up
  extra.args <- list(...)
  
  #track grid, line, shadow, parent settings
  
  #could we do this in the main plot?
  
  for (i in c("grid", "line", "shadow", "parent")) extra.args <- do.call(listLoad, 
                                                                         listUpdate(extra.args, list(load = i)))
  grid <- extra.args$grid
  line <- extra.args$line
  shadow <- extra.args$shadow
  parent <- extra.args$parent
  extra.args <- extra.args[!names(extra.args) %in% c("grid", 
                                                     "line", "shadow", "parent")]
  
  #then it would be sorted in all cases where user looks at this...?
  
  #show grid if requested
  if(isGood4LOA(grid))
    do.call(panel.grid, listUpdate(list(h=-1, v=-1), grid))
  
  #check for method
  cumsum <- if(!"method" %in% names(extra.args)){
    warning("method unknown: this may not work if data is cumsum...")
    FALSE
  } else { 
    grepl("cum", tolower(extra.args$method))
  }      
  
  #check for group and cond
  cond <- if("cond" %in% names(extra.args)) TRUE else FALSE
  groups <- if("groups" %in% names(extra.args)) TRUE else FALSE
  
  if(any(c(cond, groups)))
    if(isGood4LOA(parent)){
      temp <- listUpdate(listUpdate(list(type="l", lty=2), extra.args), parent, ignore="groups")
      if(!is.null(temp$col))
        temp$col <- temp$col[length(temp$col)]
      do.call(panel.xyplot, temp)
    }
  
  if(!groups) extra.args$groups <- rep(1, length(extra.args$y))
  grps <- levels(as.factor(extra.args$groups))
  prev.y <- rep(0, length(extra.args$y))
  
  ###################################################
  #should probably make new panel or plot for cumsum = FALSE
  #too much overplotting
  ###################################################
  
  for(i in 1:length(grps)){
    
    #think about this 
    #it does not like not dropping the NAs
    y <- rep(0, length(extra.args$local.y))
    y[extra.args$groups == grps[i] & !is.na(extra.args$groups)] <- extra.args$local.y[extra.args$groups == grps[i] & !is.na(extra.args$groups)]
    if(cumsum) y <- cumsum(y)
    #think about this
    temp <- listUpdate(listUpdate(list(border=FALSE), extra.args), shadow, ignore="groups")
    temp$x <- c(temp$x, rev(temp$x))
    temp$y <- c(y+prev.y, rev(prev.y))
    prev.y <- y + prev.y
    if(!"alpha" %in% names(temp))
      temp$alpha <- 0.3
    temp$col <- extra.args$col[i]
    temp$border <- if(isGood4LOA(line)) extra.args$col[i] else FALSE
    if(isGood4LOA(shadow) & !all(y==0))
      do.call(panel.polygon, temp)
    temp <- listUpdate(temp, line)
    temp$x <- extra.args$x
    temp$y <- prev.y
    temp$type <- "l"
    if(isGood4LOA(line) & !all(y==0))
      do.call(panel.xyplot, temp)
  }   
  
  transpose <- do.call(listLoad, listUpdate(list(load="transpose", transpose.col="red", transpose.label.format=2,
                                                 transpose.cex=0.85), extra.args))$transpose   
  if(any(c("y", "y.normal", "y.percent") %in% names(transpose))){
    borders <- current.panel.limits(unit="native")
    diffs <- c((borders$xlim[2]-borders$xlim[1])*0.02, (borders$ylim[2]-borders$ylim[1])*0.02)
    lims <- c(min(extra.args$y, na.rm=TRUE), max(extra.args$y, na.rm=TRUE))
    
    if(!"y" %in% names(transpose)){
      if("y.normal" %in% names(transpose)) transpose$y <- max(lims, na.rm=TRUE) * transpose$y.normal 
      if("y.percent" %in% names(transpose)) transpose$y <- max(lims, na.rm=TRUE)* (transpose$y.percent/100) 
    }
    for(i in transpose$y){
      temp <- extra.args$x[extra.args$y >= i][1]
      do.call(panel.lines, listUpdate(transpose, list(y=rep(i, 2), x=c(borders$xlim[1], temp))))
      do.call(panel.arrows, listUpdate(transpose, list(x0=temp, x1=temp, y0=i, y1=borders$ylim[1],
                                                       length=grid::unit(0.025, units="npc"))))
      label <- as.character(signif(i, 4))
      if(transpose$label.format==2) label <- paste(round((i/lims[2])*100), "%", sep="")
      if(transpose$label.format==3) label <- paste(label, " (", round((i/lims[2])*100), "%)", sep="")
      do.call(panel.text, listUpdate(transpose, list(x=borders$xlim[1]+diffs[1], y=i+diffs[2], label=label, pos=4,
                                                     cex=grid::unit(1, units="npc"))))
      label <- as.character(round(temp))
      if(transpose$label.format==2) label <- paste(round((temp/max(extra.args$x, na.rm=TRUE))*100), "%", sep="")
      if(transpose$label.format==3) label <- paste(label, " (", round((temp/max(extra.args$x, na.rm=TRUE))*100), "%)", sep="")
      do.call(panel.text, listUpdate(transpose, list(x=temp+(0.5*diffs[1]), y=borders$ylim[1]+diffs[2], label=label, pos=4,
                                                     cex=grid::unit(1, units="npc"))))
    }
  }
  
}



#local function for now...
calcPareto <- function(x, na.rm=TRUE, neg2zero=TRUE, method="normal.cum.Pareto", ..., output="ans"){
  #function calculates various Pareto terms 
  
  x <- as.numeric(x)
  
  ##########################
  #want to hold onto the 
  #original indices
  #for output != "ans"
  ##########################
  
  d <- data.frame(ind=1:length(x), Pareto=x)
  if(na.rm) d <- na.omit(d)
  #order highest first
  d <- d[rev(order(d$Pareto)),]
  
  ##############################
  #doing this after ordering 
  #assumes negs affect order of
  #the zero cases
  ############################## 
  if(neg2zero) d$Pareto[d$Pareto<0] <- 0
  temp <- d$Pareto
  temp[is.na(temp)] <- 0
  if(method=="Pareto" & output=="ans") return(d$Pareto)
  
  d$cum.Pareto <- cumsum(temp)
  if(method=="cum.Pareto" & output=="ans") return(d$cum.Pareto)
  
  d$normal.cum.Pareto <- d$cum.Pareto/max(d$cum.Pareto, na.rm=TRUE)
  if(method=="normal.cum.Pareto" & output=="ans") return(d$normal.cum.Pareto)
  
  d$percent.cum.Pareto <- d$normal.cum.Pareto * 100
  if(method=="percent.cum.Pareto" & output=="ans") return(d$percent.cum.Pareto)
  
  row.names(d) <- 1:nrow(d) 
  
  #output
  d
}


#local function for now

cutPareto <- function(x, cut=0.8, cond=NULL, ...){
  
  #this is function to chop Pareto series
  
  pareto <- calcPareto(x, output="all")
  if(!is.null(cond)) pareto$cond <- as.factor(cond[pareto$ind])
  
  ans <- lapply(cut, function(x){
    total <- pareto$cum.Pareto[length(pareto$cum.Pareto)]
    subtotal <- total * x
    temp <- subset(pareto, pareto$cum.Pareto <= subtotal)
    out <- c(total=total, cut=x, subtotal=subtotal, n=nrow(pareto), subn=nrow(temp), prop=nrow(temp)/nrow(pareto))
    if(is.null(cond)) out else c(out, summary(temp$cond))
  })
  ans <- do.call(rbind, ans)
  ans
}



