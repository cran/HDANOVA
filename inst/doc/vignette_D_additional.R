## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6, 
  fig.height=4
)
# Legge denne i YAML på toppen for å skrive ut til tex
#output: 
#  pdf_document: 
#    keep_tex: true
# Original:
#  rmarkdown::html_vignette:
#    toc: true

## ----setup--------------------------------------------------------------------
# Start the HDANOVA R package
library(HDANOVA)

## -----------------------------------------------------------------------------
# Load Caldana data
data(caldana)

prc.cal <- prc(compounds ~ light * time, caldana)
summary(prc.cal)

## -----------------------------------------------------------------------------
plot(prc.cal, species = FALSE, axis = 2, lwd = 4, legpos = "bottomright")

## -----------------------------------------------------------------------------
permanova.cal <- permanova(compounds ~ light * time, caldana)
permanova.cal

