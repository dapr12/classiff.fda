rm(list=ls(all=TRUE))
library("devtools")
library("roxygen2")
library("testthat")

getwd()

create("Clasificafda")
setwd("./Clasificafda")


## Add Code

#document()
#test()

setwd("..")
list.files()
roxygenize("Clasificafda")

build('Clasificafda')
install('Clasificafda')
library('Clasificafda')

#Updload to github

devtools::install_github("dapr12/Clasificafda")
library('Clasificafda')
