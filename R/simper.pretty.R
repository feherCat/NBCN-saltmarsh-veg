simper.pretty = function(x, metrics, simper_df, interesting, perc_cutoff, low_cutoff = NULL, low_val = NULL, output_name) {
  # adapted from https://github.com/asteinberger9/seq_scripts
  
  library(vegan)

  #running simper
  for(variables in interesting){
    #test_1=with(metrics, simper(x, metrics[[interesting]]))
    test_1 = simper_df
    #parsing through simper output, saving desired info to table
    for(name in names(test_1)){
      testmx=matrix(ncol=length(interesting))
      testmx=cbind(test_1[[name]]$ord,test_1[[name]]$cusum)
      sorted=testmx[order(testmx[,1]),]
      sorted=cbind(sorted,test_1[[name]]$species)
      sorted=sorted[order(sorted[,2]),]
      t=matrix(sorted[sorted[,2]<=perc_cutoff,],ncol=3)
      i=nrow(t)
      #converting percents to percent of whole
      while(i>1){
        t[i,2]=as.character(as.numeric(t[i,2])-as.numeric(t[i-1,2]))
        i=i-1
      }
      t[,1]=name
      write.table(t,file=paste(output_name,'_simper.csv',sep=""), append=TRUE, sep=",", col.names = FALSE)
    }}
  y=read.table(paste(output_name,'_simper.csv',sep=""), header=FALSE,sep=",",fill = TRUE,row.names = NULL)
  file.remove(paste(output_name,'_simper.csv',sep = ""))
  y=y[-c(1)]
  colnames(y) = c("Comparison", "SIMPER", "OTU")
  #removing results lower than low cutoff
  # if(low_cutoff=='y'){
  #   y=y[!(as.numeric(as.character(y$SIMPER))<low_val),]
  # }
  # #prevents changing of colnames if OTU table
  # if(58 %in% save){
  #   y$OTU=orig_names[match(y$OTU, rownames(orig_names)),1]
  # }
  write.csv(y,file=paste("./data/derived/simper/",output_name,'_clean_simper.csv', sep=''))
}
