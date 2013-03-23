#Function to find the surface of a geometry

topLayer = function(geom)
{
    temp = lapply(unique(geom$x), function(val)
       {
           y = max(geom[geom$x==val,"y"])
           c(val,y)
       })
    out = as.data.frame(do.call(rbind, temp))
    names(out) = c("x","y")
    out[order(out$x), ]

}
