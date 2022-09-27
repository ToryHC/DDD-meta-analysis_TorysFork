## Author: Nathalie Jreidini
## Purpose: for analysis of density-dependent dispersal data for LDP project

## download csv from Dryad repository
data <- rdryad::dryad_dataset(doi = "10.5061/dryad.cz8w9gj6c")

## NOTE: not using groundhog so as not to interfere with alredy existing R code

# explore data, fix/cleanup
summary(data)
data$Sex <- factor(data$Sex,levels = c("M", "F", "M+F"))

## obtain significance value p from effect size r and sample size n:
# create a function for calc:
rn_to_p <- function(rval, n){
  # calculate r to t
  tval <- rval/(sqrt((1-rval**2)/(n-2)))
  # look up p value for t
  pval <- 2*pt(abs(tval), n-2, lower=FALSE)
  return(pval)
}
# apply function:
data$p <- rn_to_p(data$r, data$n)


## Multilevel mixed-effect analysis
metafor::rma.mv(TE, seTE, data=data, method="REML", mod= ~ Sex + Age 
                + manipulated + density.parameter + dispersal.parameter 
                + spatial.scale + temporal.scale + migratory.,
       random= ~ 1|Taxon)

## Funnel plot for publication bias and spread of effect sizes
# set margins for plot
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
# calculate fixed and random effects
m.cor <- meta::metacor(r, n, data = data, studlab = data$Citation, sm="ZCOR", method.tau = "ML")
m.cor_trimfill <- trimfill(m.cor) # to take into account imputed studies, i.e., account for publication bias
# plot
funnel(m.cor_trimfill, comb.fixed = FALSE, col="brown",bg="brown",
       comb.random = m.cor_trimfill$comb.random, xlab = "Effect size (Zr)",
       # studlab = data$Taxon,
       cex=1,contour = c(.95,.99),
       col.contour=c("black","light grey"))
legend("topright",inset=c(-0.15, 0.1), c("P < 0.05", "P < 0.01", "NS"),bty = "n",
       fill=c("black","light grey","white", NA))
##
########################################################## END ########################################################## 