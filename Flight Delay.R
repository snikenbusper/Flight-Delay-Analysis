#install.packages("ggplot2")
library(ggplot2)
#install.packages("hash")
library(hash)

#import database dan melihat nama kolom dan summary
data <- read.csv("pro//ds//Flight_delay.csv")
colnames(data)
summary(data)
str(data)



sort(unique(data$Org_Airport)) #ada data dengan value "#N/A" dalam bentuk string
sort(unique(data$Dest_Airport))

#karena "Origin" dan "Dest" memilki peran yang hampir sama dengan kedua kolom tersebut, maka kedua kolom tersebut di drop


#cari kolom yang semua valuenya sama
for(i in 1:ncol(data))
{
  if(length(unique(data[,i]))==1)
  {
    print(colnames(data)[i])
  }
}

# drop kolom yang tidak dipakai
data = subset(data, select=-c(Org_Airport, Dest_Airport, Cancelled, CancellationCode, Diverted))

#menunjukan bahwa dataset hanya mengandung pesawat yang terdelay
for(i in 1:nrow(data))
{
  if((data$LateAircraftDelay[i] + data$SecurityDelay[i] + data$WeatherDelay[i] + data$NASDelay[i] + data$CarrierDelay[i])==0)
  {
    print("No Delay")
  }
}

#menambahkan kolom TotalDelay
TotalDelay = data$LateAircraftDelay + data$SecurityDelay + data$WeatherDelay + data$NASDelay + data$CarrierDelay
data$TotalDelay <- TotalDelay
colnames(data)


data$Month <- format(as.Date(data$Date, format="%d-%m-%Y"), "%m")
unique(data$Month)
# dataset hanya mengandung delay penerbangan dari bulan 1 hingga bulan 6

data$Year <- format(as.Date(data$Date, format="%d-%m-%Y"), "%Y")
unique(data$Year)
#dataset hanya mengandung delay penerbangan pada tahun 2019
#Sehingga kolom year tidak diperlukan
data = subset(data, select=-c(Year))


#membuat dictionary untuk jumlah delay yang disebabkan oleh alasan tertentu
delay_contribution_count <- hash()
delay_contribution_count[["weather"]] <- length(data[data$WeatherDelay!=0, ]$WeatherDelay)
delay_contribution_count[["lateAircraft"]] <- length(data[data$LateAircraftDelay!=0, ]$LateAircraftDelay)
delay_contribution_count[["nas"]] <- length(data[data$NASDelay!=0, ]$NASDelay)
delay_contribution_count[["security"]] <- length(data[data$SecurityDelay!=0, ]$SecurityDelay)
delay_contribution_count[["carrier"]] <- length(data[data$CarrierDelay!=0, ]$CarrierDelay)


#membuat pie chart untuk melihat penyebab delay paling besar
percentages = 100*round(values(delay_contribution_count)/sum(values(delay_contribution_count)),3)
pie_labels <- paste0(keys(delay_contribution_count), " : ", percentages, "%")
pie(values(delay_contribution_count), pie_labels, main="Delay Contribution Count", radius=1, cex=0.55)


#membuat bar chart untuk melihat hari yang paling banyak delay
dayOfWeek_count = as.data.frame(table(data$DayOfWeek))
barplot(height=dayOfWeek_count$Freq, xlab="Day Of Week", ylab="Delay Count" ,names.arg=dayOfWeek_count$Var1, cex.axis=0.5, las=1, col="lightblue")


#membuat bar chart untuk melihat airline yang paling banyak delay
airlineDelay_count = as.data.frame(table(data$Airline))
barplot(height=airlineDelay_count$Freq, ylab="Delay Count", names.arg=airlineDelay_count$Var1, cex.axis=0.5, cex.names=0.35,las=2, col="pink")


#membuat bar chart untuk airline dengan jarak tempuh paling banyak
airline_Dist <- aggregate(data$Distance, by=list(data$Airline), FUN=sum)
barplot(height=airline_Dist$x, ylab="Distance", names.arg=airline_Dist$Group.1, cex.axis=0.5, cex.names=0.35, las=2, col="orange")
