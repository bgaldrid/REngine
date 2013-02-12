#Function to create a statistical material property
setClass("SMP",representation(meanlog = "numeric", sdlog = "numeric"))

StatMatProp=function(mean, sd = NA, logvalues = FALSE)
{
    if (logvalues)
    {
        out = new("SMP", meanlog = mean, sdlog = sd)
    }else
    {
        out=new("SMP", meanlog = log10(mean), sdlog = log10(sd))
    }
}
