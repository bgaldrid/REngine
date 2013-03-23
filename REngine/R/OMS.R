#Factor of safety from ordinary method of slices

oms = function(sliceCoords, FA_eff_radians, c_eff, gamma, u)
{
    b = diff(sliceCoords$x) #width of slices
    alpha = diff(sliceCoords$y_low)/b #angle at bottom of slice

    alphac = (cos(alpha)>0)*alpha + (cos(alpha)<0)*(alpha+pi)
    alphas = (sin(alpha)>0)*alpha + (sin(alpha)<0)*(alpha+pi)

    l = b/cos(alphac)

    h = mapply(FUN = function(a,b)
    {
        mean(c(a,b))
        }, (sliceCoords$y_high - sliceCoords$y_low)[1:(length(sliceCoords$y_low) - 1)], (sliceCoords$y_high - sliceCoords$y_low)[2:(length(sliceCoords$y_low))]) #mean height of the slice. This could be improved to calc W

    W = gamma*h*b

    out = sum(c_eff*l + (W*cos(alphac) - u*l)*tan(FA_eff_radians))/sum(W*sin(alphas))
    if(out<0)
    {
        browser()
    }
    out
}
