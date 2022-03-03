Sys.setenv(TZ = "UTC")

# read sata data
read_sat <- function() {
 
  s2_gee<-read.csv('data-raw/s2_nepal_centmass.csv')
  sapply(ls(),function(x)get(x),simplify=F,USE.NAMES=T)
}	

# read field data
read_field <- function() {
  
  ## all nz with TSS 1976-2010
  #all_nz_field <- read.csv("data/raw/Raw_lake_data.csv")
  sapply(ls(),function(x)get(x),simplify=F,USE.NAMES=T)
}	

#########################
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    ylab("In situ")+ xlab("Machine learn")+
    theme_bw()+
    stat_smooth(method = "lm", col = "red") +
    scale_y_log10(breaks=c(0.01,0.1,1,4,10,100),labels=c(0.01,0.1,1,4,10,100)) + # Log-scale
    scale_x_log10(breaks=c(0.01,0.1,1,4,10,100),labels=c(0.01,0.1,1,4,10,100)) + # Log-scale
    
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5)))
                      # " P =",signif(summary(fit)$coef[2,4], 5)))
}
####################
