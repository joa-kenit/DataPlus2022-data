#First time you publish the website

install.packages("rsconnect")


rsconnect::setAccountInfo(name='dukerivercenter',
                          token='9CA757C8DF1F78F7C8C0DC1D4FFE1DEB',
                          secret='<SECRET>')

#change "path/to/your/app"
rsconnect::deployApp('path/to/your/app')
 


#if you want to access to the library
library(rsconnect)