F = function(val, TL, FAEr, cE, u, gam, met, n)
{
    Xc = val[1]
    Yc = val[2]
    R = val[3]
    method = met
                                        #Find where the circle intercepts
    int = cirIntercept(Xc = Xc, Yc = Yc, R = R , TopLayer = TL)

                                        #Develop the slice coordinates
    b = diff(range(int$int.x, na.rm = TRUE))/n #Width of slices

    slice = data.frame(x = rep(NA, n+1), y_low = rep(NA, n+1), y_high = rep(NA, n+1)) #Empty data frame for slice coordinates
    slice$x = seq(from = min(int$int.x, na.rm = TRUE), to = max(int$int.x, na.rm = TRUE), by = b) #input x values
    slice$y_low = -sqrt(R^2 - (slice$x - Xc)^2) + Yc #y values on circle
    slice$y_high = sapply(slice$x, function(x)
{
    for(i in 1:dim(int)[1])
    {
        if(x>=int[i, "xrange_low"]&x<=int[i, "xrange_high"])
        {
            y = int[i, "m"] * x + int[i, "b"]
            break
        }
    }
    y
}) #y values on surface

                                        #Run spencers method
if(method == "spencers")
{

}else if(method == "bishops")
      {
          out = bishops(sliceCoords = slice, FA_eff_radians = rep(FAEr, n), c_eff = rep(cE, n), gamma = rep(gam, n), u = rep(u, n))
      }else if(method == "oms")
            {
                out = oms(sliceCoords = slice, FA_eff_radians = rep(FAEr, n), c_eff = rep(cE, n), gamma = rep(gam, n), u = rep(u, n))
            }else
            {
                error("Invalid Method")
            }

if(FALSE)
{
    browser()
}
out
}
