#Calculates factor of safety for Bishops Method of Slices

bishops = function(sliceCoords, FA_eff_radians, c_eff, gamma, u)
{
    b = diff(sliceCoords$x) #width of slices
    alpha = diff(sliceCoords$y_low)/b #angle at bottom of slice

    h = mapply(FUN = function(a,b)
    {
        mean(c(a,b))
        }, (sliceCoords$y_high - sliceCoords$y_low)[1:(length(sliceCoords$y_low) - 1)], (sliceCoords$y_high - sliceCoords$y_low)[2:(length(sliceCoords$y_low))]) #mean height of the slice. This could be improved


    f = function(F, b, alpha, cohesion_eff, gamma, h, u, FrictionAngle_eff)
    {
        phi_eff = FrictionAngle_eff
        c_eff = cohesion_eff

        m_alpha = cos(alpha) * (1 + (tan(phi_eff) * tan(alpha)) / F)

        sum(b*(c_eff + (gamma*h - u)*tan(phi_eff)) * (1/m_alpha))/sum(b*gamma*h*sin(alpha)) - F
    }

    Froot = uniroot(f, interval = c(0.1, 10), b = b, alpha = alpha, cohesion_eff = c_eff, gamma = gamma, h = h, u = u, FrictionAngle_eff = FA_eff_radians, tol = 0.001)

    F = Froot$root
    F
}

