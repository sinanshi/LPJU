############################################
#read FAO data
############################################


read.fao<-function(path,startyear,endyear) {

 for(i in startyear:endyear){
  temp<-read.csv(paste(path,"fao",i,"_yields_in_hgha.csv",sep=""),sep=";",header=T)
  if(i==startyear){
     year<-temp$year
     countries<-temp$countries
     wheat<-temp$wheat
     pulses<-temp$pulses 
     soybeans<-temp$soybeans       
     rice<-temp$rice
     sugarbeet<-temp$sugarbeet
     groundnuts<-temp$groundnuts
     maize<-temp$maize
     cassava<-temp$cassava
     rape<-temp$rape
     element<-temp$element 
     millet<-temp$millet         
     sunflower<-temp$sunflower      
     sugarcane<-temp$sugarcane   
  }
  else{
     year<-c(year,temp$year)
     countries<-c(as.vector(countries),as.vector(temp$countries))
     wheat<-c(wheat,temp$wheat)
     pulses<-c(pulses,temp$pulses)         
     soybeans<-c(soybeans,temp$soybeans) 
     rice<-c(rice,temp$rice)           
     sugarbeet<-c(sugarbeet,temp$sugarbeet)
     groundnuts<-c(groundnuts,temp$groundnuts)
     maize<-c(maize,temp$maize)
     cassava<-c(cassava,temp$cassava)
     rape<-c(rape,temp$rape)                                     
     element<-c(element,temp$element) 
     millet<-c(millet,temp$millet)         
     sunflower<-c(sunflower,temp$sunflower)      
     sugarcane<-c(sugarcane,temp$sugarcane)

  } 

 }

  countries<-array(countries)
  fao.data<-data.frame(year,countries,wheat,pulses,soybeans,rice,sugarbeet,groundnuts,maize,cassava,rape,element,millet,sunflower,sugarcane)

return(fao.data)

}


read.fao.area<-function(path){

  fao.area<-read.csv(paste(path,"fao_area.csv",sep=""),sep=";",header=T)
  return(fao.area)
}




