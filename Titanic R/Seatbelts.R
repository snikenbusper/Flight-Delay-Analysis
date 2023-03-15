library(ggplot2)

data <- data.frame(as.matrix(Seatbelts), date=time(Seatbelts))

colnames(data)
summary(data)

#check for na values
for(i in 1:ncol(data))
{
  if(any(is.na(data[,i])))
  {
    print(colnames(data)[i])
  }
}
#check for null values
for(j in 1:ncol(data))
{
  for(i in 1:nrow(data))
  {
    if(is.null(data[i, j]))
    {
      print(i, j)
    }
  }
}

#plot time to amount of drivers injured/killed
plot.ts(y=data$drivers, x=data$date)
abline(lm(data$drivers ~ data$date))


#comparing the distribution of driver injury/death in a year when the seatbelt law is applied
noLaw <- data.frame(count=data[data$law==0, "drivers"])
withLaw <- data.frame(count=data[data$law==1, "drivers"])
noLaw$name <- "no law"
withLaw$name <- "with law"
drivers <- rbind(noLaw, withLaw)
ggplot(drivers, aes(count, fill=name))+geom_density(alpha=0.5)


#comparing distance travelled with distribution of driver injury/death
ggplot(data, aes(x=kms, y=drivers))+geom_point()+geom_smooth(method="lm")




