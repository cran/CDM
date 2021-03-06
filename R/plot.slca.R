## File Name: plot.slca.R
## File Version: 0.09

###################################################
# plot slca
plot.slca <- function( x, group=1,  ... )
{
    pi.k <- x$pi.k
    TP <- nrow(pi.k)
    xlabels <- seq(1, TP)
    graphics::barplot( pi.k[,group], xlab="Class", ylab="Probability",
                    names.arg=xlabels, main=paste0("Class Distribution | Group ", group ), ... )
}
