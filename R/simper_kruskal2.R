simper_kruskal <- function(park){
source("simper.pretty.R")
source("kruskal.pretty.R")

species_matrix <- plot_veg_cover_usda %>%
  ungroup() %>%
  filter(UnitCode == park) %>%
  select(EventID, UniqueID, UnitCode, Year_chr, SciName_cor, rel_percent_cover) %>%
  group_by(EventID, UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  pivot_wider(., names_from = "SciName_cor", values_from = "rel_percent_cover", values_fill = 0) %>%
  ungroup() %>%
  filter(complete.cases(.))

kruskal_species <- species_matrix %>%
  arrange(EventID, UniqueID, UnitCode, Year_chr) %>%
  filter(UnitCode == park) %>%
  select(-c(EventID, UniqueID, UnitCode, Year_chr))

kruskal_env <- species_matrix %>%
  filter(UnitCode == park) %>%
  select(EventID, UniqueID, Year_chr) %>%
  distinct()

simper_mod <- with(kruskal_env, simper(kruskal_species, Year_chr, permutations = 999))

df <-plot_veg_cover_usda %>%
  filter(UnitCode == park) %>%
  ungroup() %>%
  select(EventID, UniqueID, UnitCode, Year_chr, SciName_cor, rel_percent_cover) %>%
  mutate(SciName_cor = str_remove(SciName_cor, pattern = " "),
         UniqueID = str_remove(UniqueID, pattern = "_"),
         id = paste0(EventID, UniqueID, UnitCode, Year_chr)) %>%
  pivot_wider(., names_from = "SciName_cor", values_from = "rel_percent_cover", values_fill = 0) %>%
  column_to_rownames(var = "id")
  
species_df <- df %>%
  select(-c(EventID, UniqueID, UnitCode, Year_chr)) 
  
env_df <- df %>%
  select(c(EventID, UniqueID, UnitCode, Year_chr))
  
simper.pretty(x = species_df, metrics = env_df, simper_df = simper_mod, interesting = c('Year_chr'), perc_cutoff = 1, output_name = paste0(tolower(park)))

otu <- kruskal_species
metrics <- kruskal_env
csv_name <- paste0("./data/derived/simper/", tolower(park), "_clean_simper.csv")
csv <- read.csv(csv_name)
interesting = c('Year_chr')
output_name <- tolower(park)

csv$X=as.integer(rownames(csv))
L=list()
R=list()
mean_L=c()
sd_L=c()
mean_R=c()
sd_R=c()
L_mean=c()
R_mean=c()
L_sd=c()
R_sd=c()
krusk=c()
tax=c()
L_abund=c()
R_abund=c()
L_abund_sd=c()
R_abund_sd=c()
abund=as.matrix(otu)
#abund=abund/rowSums(abund)

for(b in unique(csv$Comparison)){
  otu_list=dplyr::filter(csv, Comparison==b) #saves otu list for current comparison
  for(i in csv$X){
    if(as.character(csv$Comparison[i])==b){  ##splitting comparisons so can call individually for table generation
      splt=as.data.frame(matrix(unlist(strsplit(as.character(csv$Comparison[i]),'_')), nrow=1, byrow=T))
      cola=as.character(splt[1,1])
      colb=as.character(splt[1,2])
      break
    }
  }
  #saving topic containing var of interest (cola/colb) (less memory intensive)
  for(topic in interesting){
    #preventing crash if there is only one topic in interesting
    if(is.null(unique(metrics[[topic]]))==TRUE){
      topic1=topic
      break
    }
    for(sbtpic in unique(metrics[[topic]])){
      if(sbtpic==cola){
        topic1=topic
        break
      }
    } 
  }
  #iterate thru rows in tpics of intrst til matches cola and colb, generates otu and metrics tbl  ##!Processing can be reduced!##
  for(rowe1 in metrics[[topic1]]){
    for(rowe2 in metrics[[topic1]]){ 
      if(rowe1==cola & rowe2==colb){ 
        listbact=otu[c(metrics[[topic1]]==cola|metrics[[topic1]]==colb),]
        listmet=metrics[c(metrics[[topic1]]==cola|metrics[[topic1]]==colb),]
        break
      }
    }
  }
  #collecting differential abundances
  sample_L=row.names(subset(metrics, metrics[[topic1]] == c(cola)))
  sample_R=row.names(subset(metrics, metrics[[topic1]] == c(colb)))
  #collecting abund values, perform/save mean and stdev calculations
  for(otus in otu_list$OTU){
    otus = as.character(otus)
    for(sample in sample_L){
      #L=append(L,abund[sample,otus])
      L=append(L,abund[as.numeric(sample),otus])
      mean_L[[otus]]=mean(as.numeric(L))
      sd_L[[otus]]=sd(as.numeric(L))
    }
    for(sample in sample_R){
      #R=append(R,abund[sample,otus])
      R=append(R,abund[as.numeric(sample),otus])
      mean_R[[otus]]=mean(as.numeric(R))
      sd_R[[otus]]=sd(as.numeric(R))
    }
    L=list()
    R=list()
  }
  #runs kruskal.test for each otu in simper csv, stores as list, also stores abundances
  for(otus in otu_list$OTU){
    otus = as.character(otus)
    result=kruskal.test(listbact[[otus]]~listmet[[topic1]])
    krusk=append(krusk, result$p.value)
    tax=append(tax, c("NA"))
    L_mean=append(L_mean, as.character(mean_L[[otus]]))
    R_mean=append(R_mean, as.character(mean_R[[otus]]))
    L_sd=append(L_sd, as.character(sd_L[[otus]]))
    R_sd=append(R_sd, as.character(sd_R[[otus]]))
    
  }
}

fdr=p.adjust(krusk, method='fdr')
#order csv to match 'krusk'/'fdr' list, add p.val, add taxonomy, re-ord to match orig csv, write to csv
#o_csv=dplyr::arrange(csv, Comparison)
o_csv=csv
o_csv[,5]=krusk
o_csv[,6]=fdr
o_csv[,7]=tax
o_csv[,8]=as.numeric(L_mean)
o_csv[,9]=as.numeric(L_sd)
o_csv[,10]=as.numeric(R_mean)
o_csv[,11]=as.numeric(R_sd)
#o_csv=dplyr::arrange(o_csv, X)
colnames(o_csv)[which(names(o_csv) == "V5")] <- "krusk_p.val" #changes column header
colnames(o_csv)[which(names(o_csv) == "V6")] <- "fdr_krusk_p.val"
colnames(o_csv)[which(names(o_csv) == "V7")] <- "taxonomy"
colnames(o_csv)[which(names(o_csv) == "V8")] <- "left_mean_abund"
colnames(o_csv)[which(names(o_csv) == "V9")] <- "left_stdev"
colnames(o_csv)[which(names(o_csv) == "V10")] <- "right_mean_abund"
colnames(o_csv)[which(names(o_csv) == "V11")] <- "right_stdev"
o_csv[,1]=NULL

o_csv <- o_csv %>%
  mutate(dif = abs(left_mean_abund - right_mean_abund))

write.csv(o_csv, file=paste("./data/derived/simper/",output_name,"_krusk_simper.csv", sep=""))
}