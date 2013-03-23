#Function to find the slope stability of a given slope

SlopeStability = function(geom, materials, xrange, yrange, Rrange,
n = 10, N = 10,
method = "oms",
startX = mean(xrange),
startY = mean(yrange),
startR = mean(Rrange))
{

    #SOME NOTE
    #NEED TO LIMIT R TO THE SIZE OF THE AREA (SO CIRCLE IS CONTAINED)
    up = c(max(xrange), max(yrange), max(Rrange))
    low = c(min(xrange), min(yrange), min(Rrange))

    #Find the points that are at the surface
    tl = topLayer(geom)

    #Optimize over F======
    #Create value sets

    if(N==1)
    {
        fae = materials["FrictionAngle_eff","MEAN"]*pi/180
        ce = materials["cohesion_eff","MEAN"]
        g = materials["UnitWeight","MEAN"]
        pp = 0
    }else
    {
        fae = rnorm(n = N, mean = (materials["FrictionAngle_eff","MEAN"]*pi)/180, sd = (materials["FrictionAngle_eff","SD"]*pi)/180)
        ce = rnorm(n = N, mean = materials["cohesion_eff","MEAN"], sd = materials["cohesion_eff","SD"])
        g = rnorm(n = N, mean = materials["UnitWeight","MEAN"], sd = materials["UnitWeight","SD"])
        pp = 0
    }

    vals = list(fae, ce, g, pp)
    values = do.call(expand.grid, vals)
    names(values) = c("fae","ce","g","pp")

    #Run in parallel

    num = detectCores()
    if(num<2)
    {
        num = 2
    }
    cluster = makeCluster(spec = num)

    clusterExport(cl = cluster, varlist = c("values","method","n","up","low","Rrange","tl", "startX","startY","startR","F","oms","cirIntercept"), envir = environment())

    fs = clusterApplyLB(cl = cluster, 1:dim(values)[1], function(i)
               {
                   fae = values[i,"fae"]
                   ce = values[i,"ce"]
                   g = values[i,"g"]
                   pp = values[i,"pp"]

                   temp = optim(par = c(startX, startY, startR), TL = tl, fn = F, FAEr = fae, cE = ce, u = pp, gam = g, met = method, n = n, method = "L-BFGS-B", upper = up, lower = low)
                   temp$value
               })

    stopCluster(cluster)

    FS = unlist(fs)

    #Generate probability of failure (if applicable)
    if(N>1)
    {
    beta = (mean(FS) - 1)/sd(FS)
    Pf = 1 - pnorm(beta)

}else
{
    warning("Only one run done, returning Factor of Safety")
    FS
}
}
