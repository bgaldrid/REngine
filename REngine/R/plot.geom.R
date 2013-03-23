#Plotting function for geom
plot.geom = function(geom)
{
    ggplot(data = temp) + geom_polygon(mapping = aes(x = x, y = y, group = name, fill=name))
}
