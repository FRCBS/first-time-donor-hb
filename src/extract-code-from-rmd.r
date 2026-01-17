# Make sure the working directory is set to the root folder


getwd()
f <- '/src/main-with-export.Rmd'
fp=paste0(getwd(),f)
knitr::purl(fp,output=sub('Rmd$','r',fp))
# file.copy(sub('Rmd$','R',paste0(getwd(),f)),paste0(getwd(),'/src'),overwrite=TRUE)
Stangle(f)

?file.copy


