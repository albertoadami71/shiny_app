dados<-read.csv("Data_Extract_From_World_Development_Indicators_Data.csv", na.strings=c(".."))
names(dados)[1]="Name"
library(forecast)
shinyServer(
function(input, output) {

output$country1 <- renderText({
pais = input$id2
if (pais=="") {print("-")} else {print(pais)}
})
output$country2 <- renderText({input$id3})
output$newPlot <-renderPlot({
#titulo = "Historical Data from World Development Indicators"
titulo = ""
x = seq(from=1960, to=2015, by=1)
pais = input$id2
pais2 = input$id3
if ((pais!="")&((pais2=="")|(pais2=="-"))){
extrai = dados[ (dados$Name==pais)&(dados$Series.Name== dados$Series.Name[2]), 5:dim(dados)[2]]
extrai = as.numeric(extrai)
ts1<-ts(extrai)
tstrain = window(ts1,start=1, end=55)
ets1 =ets(tstrain,model="AAN")
fcast = forecast(ets1,h=6)
prev = c(ts1[length(ts1)-1],as.vector(fcast$mean))
oldyear = ts1[length(ts1)-1]
newyear = fcast$mean[1]
growth = 100*(newyear-oldyear)/oldyear
growth = format(round(growth, 2), nsmall = 2) 
newyear = format(round(newyear, 2), nsmall = 2) 

anos=c(2014,2015,2016,2017,2018,2019,2020)
xaux=c(x,anos)
xmin = min(xaux)
xmax = max(xaux)
ymin = min(min(ts1[!is.na(ts1)]),min(prev))
ymax = max(max(ts1[!is.na(ts1)]),max(prev))
plot(x,ts1,type="l", col = "blue",xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="year",ylab="GDP per capita (current US$)", main = titulo)
texto = paste(pais," - GDP per capita (projection in 2015) = US$", as.character(newyear),", ","growth (%) = ", as.character(growth))
mtext(texto, col="blue")
par(lwd=4)
lines(anos,prev,col="grey")

}

if (( (pais2!="")&(pais2!="-") )&(pais=="")){
extrai = dados[ (dados$Name==pais2)&(dados$Series.Name== dados$Series.Name[2]), 5:dim(dados)[2]]
extrai = as.numeric(extrai)
ts2<-ts(extrai)
tstrain = window(ts2,start=1, end=55)
ets2 =ets(tstrain,model="AAN")
fcast = forecast(ets2,h=6)
prev = c(ts2[length(ts2)-1],as.vector(fcast$mean))
oldyear2 = ts2[length(ts2)-1]
newyear2 = fcast$mean[1] 
growth2 = 100*(newyear2-oldyear2)/oldyear2
growth2 = format(round(growth2, 2), nsmall = 2) 
newyear2 = format(round(newyear2, 2), nsmall = 2)
anos=c(2014,2015,2016,2017,2018,2019,2020)
xaux=c(x,anos)
xmin = min(xaux)
xmax = max(xaux)
ymin = min(min(ts2[!is.na(ts2)]),min(prev))
ymax = max(max(ts2[!is.na(ts2)]),max(prev))
plot(x,ts2,type="l", col = "red",xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="year",ylab="GDP per capita (current US$)", main = titulo)
texto = paste(pais2," - GDP per capita (projection in 2015) = US$", as.character(newyear2),", ","growth (%) = ", as.character(growth2))
mtext(texto, col="red")

par(lwd=4)
lines(anos,prev,col="grey")
}


if (((pais2!="")&(pais2!="-")  )&(pais!="")){
extrai = dados[ (dados$Name==pais)&(dados$Series.Name== dados$Series.Name[2]), 5:dim(dados)[2]]
extrai = as.numeric(extrai)
ts1<-ts(extrai)
tstrain = window(ts1,start=1, end=55)
ets1 =ets(tstrain,model="AAN")
fcast = forecast(ets1,h=6)
prev1 = c(ts1[length(ts1)-1],as.vector(fcast$mean))
anos=c(2014,2015,2016,2017,2018,2019,2020)
xaux=c(x,anos)
xmin1 = min(xaux)
xmax1 = max(xaux)
ymin1 = min(min(ts1[!is.na(ts1)]),min(prev1))
ymax1 = max(max(ts1[!is.na(ts1)]),max(prev1))
oldyear = ts1[length(ts1)-1]
newyear = fcast$mean[1] 
growth = 100*(newyear-oldyear)/oldyear
growth = format(round(growth, 2), nsmall = 2) 
newyear = format(round(newyear, 2), nsmall = 2) 
texto1 = paste(pais," - GDP per capita (projection in 2015) = US$", as.character(newyear),", ","growth (%) = ", as.character(growth))


extrai = dados[ (dados$Name==pais2)&(dados$Series.Name== dados$Series.Name[2]), 5:dim(dados)[2]]
extrai = as.numeric(extrai)
ts2<-ts(extrai)
tstrain = window(ts2,start=1, end=55)
ets2 =ets(tstrain,model="AAN")
fcast = forecast(ets2,h=6)
prev2 = c(ts2[length(ts2)-1],as.vector(fcast$mean))
anos=c(2014,2015,2016,2017,2018,2019,2020)
xaux=c(x,anos)
xmin2 = min(xaux)
xmax2 = max(xaux)
ymin2 = min(min(ts2[!is.na(ts2)]),min(prev2))
ymax2 = max(max(ts2[!is.na(ts2)]),max(prev2))
oldyear2 = ts2[length(ts2)-1]
newyear2 = fcast$mean[1] 
growth2 = 100*(newyear2-oldyear2)/oldyear2
growth2 = format(round(growth2, 2), nsmall = 2) 
newyear2 = format(round(newyear2, 2), nsmall = 2)
texto2 = paste(pais2," - GDP per capita (projection in 2015) = US$", as.character(newyear2),", ","growth (%) = ", as.character(growth2))



xmin = min(xmin1, xmin2)
xmax = max(xmax1, xmax2)
ymin = min(ymin1, ymin2)
ymax = max(ymax1, ymax2)


plot(x,ts1,type="l", col = "blue",xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="year",ylab="GDP per capita (current US$)", main = titulo)
par(lwd=4)
lines(anos,prev1,col="grey")

par(lwd=1)
points(x,ts2,type="l", col = "red", ylim=c(ymin,ymax))
texto=paste(texto1,"; ",texto2)
mtext(texto1, col="blue", line=1)
mtext(texto2, col="red",line=0)

par(lwd=4)
lines(anos,prev2,col="grey")

}


})



}
)