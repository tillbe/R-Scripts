#read google docs

getGDrive=function(key){
  url=paste("https://docs.google.com/spreadsheets/d/",key, "/export?format=csv", sep="")
  print(url)
  url=getURL(url, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  x=read.csv(textConnection(url), header = T, sep = ",")
  return(x)
}

