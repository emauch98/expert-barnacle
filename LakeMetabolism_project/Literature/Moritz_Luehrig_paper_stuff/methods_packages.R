
rm(list= ls())
# load packages -----------------------------------------------------------

pkgs<-c(
  "bit64",
  "cowplot",
  "data.table",
  "lubridate",
  "tidyverse",
  "viridis",
  "zoo"
)

## install if needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(char=pkgs,install=T)
rm(pkgs)


# outlier removal function ------------------------------------------------

outlier_removal <- function(timeseries, window, threshold, return="removed", plot=F, save=F, name, path){
  
  upper <- rollapply(timeseries, window, FUN=function(x){
    m = median(x); median(x) + threshold * median(abs(x - m))
  }, fill=NA)
  
  lower <- rollapply(timeseries, window, FUN=function(x){
    m = median(x); median(x) - threshold * median(abs(x - m))
  }, fill=NA)
  
  outliers <- timeseries > upper | timeseries < lower
  n_out <- length(timeseries[outliers])
  n <- length(outliers)

  
  if(plot==T){
    x=seq(1,length(timeseries),1)
    plot(x,timeseries, type="l", lwd=1.5, col="red", 
            ylim=c(min(lower, na.rm = T)-(min(lower, na.rm = T)/50), c(max(upper, na.rm = T)+(max(upper, na.rm = T)/50))))
    lines(x, upper, col="blue", lwd=1)
    lines(x, lower, col="blue", lwd=1)
    points(x[outliers], timeseries[outliers], pch=19, cex=1)
    title(name, sub = paste("window = ",window, ", threshold = ", threshold, ", number of outliers = ", n_out, ", number of observations = ", n))
    dev.off()
  }
  if(save==T){
    save_path = file.path(getwd(), path)
    dir.create(save_path, recursive=T)
    png(paste0(save_path, "/",name, ".png"), width = 18, height = 9, units = 'in', res = 300)
    x=seq(1,length(timeseries),1)
    plot(x,timeseries, type="l", lwd=1.5, col="red", 
         ylim=c(min(lower, na.rm = T)-(min(lower, na.rm = T)/50), c(max(upper, na.rm = T)+(max(upper, na.rm = T)/50))))
    lines(x, upper, col="blue", lwd=1)
    lines(x, lower, col="blue", lwd=1)
    points(x[outliers], timeseries[outliers], pch=19, cex=1)
    title(name, sub = paste("window = ",window, ", threshold = ", threshold, ", number of outliers = ", n_out, ", number of observations = ", n))
    dev.off()
  }
  if(return=="removed"){
    outliers[is.na(outliers)]<-FALSE
    return(ifelse(outliers, NA, timeseries))}
  else if(return=="boolean"){
    return(outliers)
    }
}


# convenience functions ---------------------------------------------------

summ <- function(x, value.cols=NULL, by=NULL, mean_only=F, na.rm=T){
  x <- setDT(x)
  if(mean_only==T){
    summ = x[, sapply(.SD, m_se), by=by, .SDcols=value.cols]
    setnames(summ, (length(by)+1):length(summ), CJ(value.cols, c("mean","se"), sorted = FALSE)[, paste(V1, V2, sep ="_")])
  }
  else {
  summ <- x[, psych::describe(.SD, na.rm = na.rm),  by=by, .SDcols=value.cols]
  summ$vars = factor(summ$vars)
  setattr(summ[["vars"]],"levels",value.cols)
  setnames(summ, (length(by)+2):length(summ), 
           c("N", "Mean", "SD", "Median", "Trimmed", 
             "Mad", "Min", "Max", "Range", "Skewness", "Kurtosis", "SE"))
  }
  return(summ)
}

grid_arrange_shared_legend <- function(..., nrow = nrow, ncol = ncol) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  pl  <- lapply(plots, function(x) x + theme(legend.position="none", plot.margin=margin(20,20,20,20)))
  tmp <- do.call(arrangeGrob, c(pl,list(ncol=ncol, nrow=nrow) ))
  grid.arrange(tmp, legend, ncol=1, heights = unit.c(unit(1, "npc") - lheight, lheight))
}


AC <- function(x){((acf(x,plot=F, na.action=na.pass)[1][[1]][1]))}

CV <- function(x){ 
  x = x[complete.cases(x)]
  y = if(min(x, na.rm=T)<=0) (x+abs(min(x, na.rm=T))) else(x)
  (sd(y)/mean(y))}

m_r = function(x, digits=10){MEAN = round(mean(x, na.rm=T), digits)}

m_se = function(x, digits=10){list(MEAN = round(mean(x, na.rm=T),digits), SE = round((sd(x,na.rm=T) / sqrt(length(x))), digits))}

gev_fun = function(x){
  tryCatch({fgev(x,std.err = FALSE)$estimate[[3]]}, 
           error=function(err){as.numeric(NA)})}

