options(RCurlOptions = list(
    proxy           = "proxy", 
    proxyusername   = "username", 
    proxypassword   = "password",
    proxyport       = 8080,
    capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), 
    ssl.verifypeer = FALSE
))

opts <- list(
    proxy         = "proxy", 
    proxyusername = "username", 
    proxypassword = "password",
    proxyport     = 8080)