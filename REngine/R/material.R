#A constructor of a material

#setClass(Class = "material",
#         representation = list(MEAN = "numeric", SD = "numeric", SKEWNESS = "numeric", KURTOSIS = "numeric", N = "#integer"))

material = function(...)
{
    require(moments)
    D = list(...)
    data.frame(MEAN = sapply(D,mean),
               SD = sapply(D, sd),
               SKEWNESS = sapply(D, skewness),
               KURTOSIS = sapply(D, kurtosis),
               N = sapply(D, length))
}





if(FALSE)
{
setMethod(f = "print", signature = c(x = "material"), definition = function(x)
      {
          print(data.frame(MEAN = slot(x, "MEAN"),
                           SD = slot(x, "SD"),
                           SKEWNESS = slot(x, "SKEWNESS"),
                           KURTOSIS = slot(x, "KURTOSIS"),
                           N = slot(x, "N")))
      })

setMethod(f = "show", signature = c(object = "material"), definition = function(object)
      {
          show(data.frame(MEAN = slot(object, "MEAN"),
                           SD = slot(object, "SD"),
                           SKEWNESS = slot(object, "SKEWNESS"),
                           KURTOSIS = slot(object, "KURTOSIS"),
                           N = slot(object, "N")))
      })
}
