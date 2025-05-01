library(openxlsx)

exportCountry = function(country,file) {
  file=file.path(param$wd,'data','exported-statistics_Hb_NL.xlsx')
  sheet.names = getSheetNames(file)
  country='NL'
  for (sn in sheet.names) {
    data = read.xlsx(file,colNames=TRUE,rowNames=FALSE,sheet = sn)
    data$country = country
    data=data[,c(ncol(data),1:(ncol(data)-1))]
    if (sn == 'annual.hb'){
      data = data[abs(data$Hb) < 1000 & !is.na(data$Hb),]
      hb.freq = data
    } else if (sn == 'annual.age') {
      age.freq = data
    } else if (sn == 'monthly.statistics') {
      montly = data
    }
  }
  return(list(hb.freq=hb.freq,age.freq=age.freq,monthly=monthly))
}

rbindByElement = function(x,y) {
  for (n in names(x)) {
    x[[n]] = rbind(x[[n]],y[[n]])
  }
  x
}

# data=exportCountry('NL','exported-statistics_Hb_NL.xlsx')

data = exportCountry('FI',exported-statistics.xlsx)
data = rbindByElement(data,exportCountry('NL','exported-statistics_Hb_NL.xlsx'))
hb.freq=data$hb.freq
age.freq=data$age.freq
monthly=data$monthly


