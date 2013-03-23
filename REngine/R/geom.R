#Build a geometry object
geom = function(...)
{
    D = list(...)
    N = names(D)
    out = mapply(FUN = function(l, n)
       {
           temp = data.frame(name = rep(n,length(l["x"])), x = l["x"], y = l["y"])

           #temp[order(temp$y, temp$x), ] #THIS IS NOT CORRECT. NEED DIFFERENT ORDERING SCHEME
       }, D, N, SIMPLIFY = FALSE)

    do.call(rbind, out)
    #coordinates(object = out2) <- ~x+y
    #out2
}
#geom(S1 = list(x = c(0,150,150,110,50,0), y = c(0,0,50,50,20,20)))
#geom(clay1 = list(x=c(0,10,0,10), y=c(0,0,2,2)), clay2 = list(x=c(0,10,0,10), y=c(2,2,4,4)))

