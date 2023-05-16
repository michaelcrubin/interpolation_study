
#*****************************************************************************************
#*****************************************************************************************
####   HERE ARE ALL THE FUNCTIONS FOR THE STATISTICAL ALGORITHM AND THE PLOTTING     #### 
#*****************************************************************************************
#*****************************************************************************************


#*****************************************************************************************
#******FUNCTIONS FOR DATA PREPARATION ****************************************************
#*****************************************************************************************


#IMPORT AND TRANSPOSE DATA FRAME
import_fun<-function(filename){

  #getting current working directory
  my_dir<- getwd()
  
  #Importing the File
  pathname<-paste(my_dir, 'Data',filename, sep = "/")
  df1 <- read.table(pathname, 
                    header = TRUE,
                    sep = ";", dec="," )
  #returning
  df1
}

#IMPORT AND TRANSPOSE DATA FRAME FROM CSV
get_transposed_df<-function(filename){  
  
  #importing the file by function
  #data_set$FileName
  filename<-paste(filename, '.csv', sep = '')
  df1<-import_fun(filename)
  
  #Transpose the df1 from a row to a colum matrix, then add the correct column headers and delete them in the data rows 
  dft = as.data.frame(x = t(sapply(df1, as.numeric)), stringsAsFactors = FALSE)
  timestamps<-df1[,1]
  colnames(dft) <- timestamps
  dft <- dft[-1, ]
  #adjusting the column names
  stations<-colnames(df1)
  stations <- stations[(2:length(stations))]
  dft<-cbind(index=stations, dft)
  dft #returning the Df
}

#DEFINE NAMES FOR SAVING RESULTS AND CREATING FOLDER
file_names<-function(data_set){
  #FUNCTION TAKES: dataset_name
  #FUNCTION RETURNS: vector with all data set name, creates directory
  
  #Creating directory for saving
  my_dir<- paste(getwd(), 'Cases', sep = "/")
  dir.create(file.path(my_dir), showWarnings = F)
  resdir <- paste(my_dir,data_set$FileName, sep = "/")
  dir.create(file.path(resdir), showWarnings = F)
  resdir2<- paste(resdir, 'Results', sep = "/")
  dir.create(file.path(resdir2), showWarnings = F)
  
  #Creating some names for saving of files afterwards
  metaname<-paste(resdir2, paste('META_DATA.json',sep = '_'), sep = '/')
  descriptname<-paste(resdir2, paste('DESCRIPTIVE_STATS.csv',sep = '_'), sep = '/')
  mapname<-paste(resdir, paste('MAP.png',sep = '_'), sep = '/')
  
  #putting all names in a list and returning
  return<-list(metaname=metaname, descriptname=descriptname,mapname=mapname, parentpath=resdir, resultpath=resdir2, setname=as.character(data_set$SetName), variable=as.character(data_set$Variable), filename=as.character(data_set$FileName))
}

#CALCULATING THE DISTANCES BETWEEN STATIONS AND POINT_0
distance_calc<-function(dft, Point_0x, Point_0y){
  #FUNCTION TAKES: Data frame dft, Point_0x, Point_0y
  #FUNCTION RETURNS: df_data
  
  #GIVING BIRTH TO THE DATA DF FROM MY MOTHER DATA FRAME
  df_data <-dft[!(row.names(dft) %like% 'erification'),]
  
  #adding the columns for the Distances to Point 0
  df_data<- add_column(df_data, distanceEuclidian = 0, .after = 3) #adding column for Euclidian Distance
  df_data<- add_column(df_data, distanceGeo = 0, .after = 3) #add column for Geographical distance
  
  #calculating the true geographical Distance using the Harversine function(note: there are some differences to Google Earth, but might be unimportant)
  Point_0x_vect<-c(rep(Point_0x, length(df_data$x))) #creating a vector with same lengh of n but all coordinates Point 0
  Point_0y_vect<-c(rep(Point_0y, length(df_data$y)))
  df_data$distanceGeo <- distGeo(cbind(Point_0x_vect, Point_0y_vect),cbind(df_data$x, df_data$y),a=6378137)
  
  #calculating Euclidian, which is not accurate, just in case you dont have the library above
  df_data$distanceEuclidian<-sqrt((Point_0x-df_data$x)^2+(Point_0y-df_data$y)^2)*100000
  
  df_data
}

#GETTING INDEXES FOR DF DATA SLICING
get_ind_data<-function(df_data){
  #Creating some indexes for more convenient slicing & Managing #####TAKE CARE MAYBE THIS MUST BE FOR STATS NOT FOR DATA
  len_df_data<-length(colnames(df_data)) #is total length of data frame
  
  metadata<-c('index', 'x', 'y', 'distanceGeo', 'distanceEuclidian', 'MAE', 'RMSE', 'Averages')
  z<-(!(colnames(df_data)) %in% metadata)==TRUE#just helper
  n<-length(z[z==TRUE]) # is the lenght of timestamps for df
  
  ind_df_data=len_df_data-n+1 #is the starting index of the data in the df
  
  k<-length(df_data$index) #is total number of stations
  
  return<-list(n=n, k=k,len_df_data=len_df_data,ind_df_data=ind_df_data )
}

#CALCULATING THE TOTAL INTERPOLATED AREA AND OTHER VARIBALES FOR DESCRIBTION
get_area<-function(df_data, len_df_data, ind_df_data){
  #preparing coordinate map
  B = (matrix(c(as.numeric(df_data$x),as.numeric(df_data$y)), nrow=4,ncol=2))
  B=B*-100000
  #calculating area in HA
  area=round(shoelace(B)/10000, digits = 0)
  
  #calculate and store n (number of data observations)
  n<-len_df_data-ind_df_data
  
  #calculate and store k (number of neighbours)
  k<-length(df_data$index)
  
  #calculate distance
  av_dist = round(mean(df_data$distanceGeo), digits = 0)
  #calculate min and max date
  a<-colnames(df_data)
  min_date = a[len_df_data]
  max_date = a[ind_df_data]
  
  return<-list(area=area, av_dist=av_dist,min_date=min_date,max_date=max_date, n=n, k=k)
}
  
#PLOTTING AND SAVING THE MAP OF THE SITUATION
save_map<-function(df_data,Point_0x, Point_0y, av_dist, area, mapname){
  #CREATING MAP PLOT WITH ALL STATIONS AND POINT 0
  mapplot<-ggplot(df_data, aes(x=x, y=y)) +
    geom_point(size=2, shape=4, colour='blue')+
    geom_point(aes(x=Point_0x, y=Point_0y), size=4, shape=4, colour='red')+
    theme_light()+
    labs(title=paste('Average Distance=', av_dist,'m', ' / Interpolated Area=', area,'HA', sep=""),
         x ="LAT", y = "LONG")
  #Saving this plot
  ggsave(mapname, plot = mapplot, width = 5.5, height = 5)
}


#*****************************************************************************************
#******DESCRIBTIVE STATS ARE CALCULATED ****************************************************
#*****************************************************************************************

#CREATES STATS DATA FRAME
stat_df<-function(dft){
  #FUNCTION TAKES: 
  #FUNCTION RETURNS:
  #GENERATING THE STATISTICS DF FROM MY MOTHER DATA FRAME
  df_stats <-dft[(row.names(dft) %like% 'erification'),]
  
  #adding a row for Mean, SD, Min, Max in STATS DF
  df_stats<- add_row(df_stats,index="Mean", .after = 1) #
  df_stats<- add_row(df_stats,index="SD", .after = 1) #
  df_stats<- add_row(df_stats,index="MIN", .after = 1) #
  df_stats<- add_row(df_stats,index="MAX", .after = 1) #
  df_stats<- add_row(df_stats,index="Range", .after = 1) #
  
  #adding the columns for the Average of all stats
  df_stats<- add_column(df_stats, Averages = 0, .after = 3)
  #return the df_stats
  df_stats
}

#CREATE INDICIES FOR STATS DATA FRAME
get_ind_stat<-function(df_stats){

  #Creating some indexes for more convenient slicing & Managing
  z<-df_stats$index=='Mean' ##index of Mean row
  Meanindex<-min(which(z == TRUE)) 
  z<-df_stats$index=='SD' ##index of SD row
  SDindex<-min(which(z == TRUE))
  z<-df_stats$index=='MIN' ##index of RMSE row
  Minindex<-min(which(z == TRUE))
  z<-df_stats$index=='MAX' ##index of RMSE row
  Maxindex<-min(which(z == TRUE)) 
  z<-df_stats$index=='Range' ##index of RMSE row
  Rangeindex<-min(which(z == TRUE)) 
  z<-df_stats$index %like% 'erification' ##index of Point_0 row
  Verifindex<-min(which(z == TRUE)) 
  
  #Creating some indexes for more convenient slicing & Managing
  metadata<-c('index', 'x', 'y', 'distanceGeo', 'distanceEuclidian', 'MAE', 'RMSE', 'Averages')
  len_df_stat<-length(colnames(df_stats)) #is total length of data STAT
  z<-(!(colnames(df_stats)) %in% metadata)==TRUE#just helper
  o<-length(z[z==TRUE]) # is the lenght of timestamps for df STAT
  ind_df_stat=len_df_stat-o+1 #is the starting index of the STAT
  
  return<-list(Meanindex=Meanindex, SDindex=SDindex,Minindex=Minindex,Maxindex=Maxindex,Rangeindex=Rangeindex,Verifindex=Verifindex,len_df_stat=len_df_stat,ind_df_stat=ind_df_stat)
}

#CALCULATING THE DESCRIPTIVE STATISTICS
create_stats<-function(df_stats, val_matrix, stats_index, filenames){

  #Calculating Min, Max, Range
  df_stats[stats_index$Minindex, stats_index$ind_df_stat:stats_index$len_df_stat] <- sapply(val_matrix, min)
  df_stats[stats_index$Maxindex, stats_index$ind_df_stat:stats_index$len_df_stat] <- sapply(val_matrix, max)
  df_stats[stats_index$Rangeindex, stats_index$ind_df_stat:stats_index$len_df_stat] <- df_stats[stats_index$Maxindex, stats_index$ind_df_stat:stats_index$len_df_stat]-df_stats[stats_index$Minindex, stats_index$ind_df_stat:stats_index$len_df_stat]
  
  #Calculating Mean
  k=length(val_matrix[,1])
  df_stats[stats_index$Meanindex, stats_index$ind_df_stat:stats_index$len_df_stat] <- colSums(val_matrix)/k
  #calculating Standard Deviation
  df_stats[stats_index$SDindex, stats_index$ind_df_stat:stats_index$len_df_stat] <- sqrt(sapply(val_matrix, var))
  
  #Calculating the Averages
  df_stats$Averages=rowMeans(round(df_stats[, stats_index$ind_df_stat:stats_index$len_df_stat],digits=3))
  
  #SAVING DESCRIPTIVE DATA DF TO CSV
  write.csv(df_stats[,1:4], filenames$descriptname )
  df_stats
}


#***************************************************************************************
#******* CALCULATE THE INTERPOLATION ESTIMATES FOR DIFFERENT METHODS INCL ERRORS *******
#***************************************************************************************

#CREATING A SCORE DATA FRAME
score_df<-function(dft){
  
  #GIVING LIVE TO MY SCORE DF, WHICH CONTAINS THE RESULTS FOR ALL DIFFERENT ESTIMATORS
  df_score <- dft[0, ]#copying structure of DF
  
  #adding MAE + RMSE column in score df
  df_score<- add_column(df_score, RMSE_SD = 0, .after = 3) #adding column 
  df_score<- add_column(df_score, MAE_MEAN = 0, .after = 3) #add column 
  df_score
}

#GETTING THE INCIDES OF THE SCORE DF
get_ind_score<-function(df_score){

  #Creating some indexes for more convenient slicing & Managing
  metadata<-c('index', 'x', 'y', 'distanceGeo', 'distanceEuclidian', 'RMSE_SD', 'MAE_MEAN', 'Averages')
  len_df_score<-length(colnames(df_score)) #is total length of data score
  z<-(!(colnames(df_score)) %in% metadata)==TRUE#just helper
  m<-length(z[z==TRUE]) # is the lenght of timestamps for df score
  ind_df_score=len_df_score-m+1 #is the starting index of the SCORE

  return<-list(len_df_score=len_df_score, ind_df_score=ind_df_score)
}

#CREATING THE IDW ESTIMATORS FOR DIFFERENT POWERS
get_idw_estim<-function(df_score, distvect, val_matrix, estimator_names, score_index, Point_0x, Point_0y){
  counter<-0
  n=length(val_matrix[1,])
  # IDW LOOP - GOING THROUGH EACH POWER, INSERTING ROWS AND DOING CALCS
  for (i in estimator_names){
    #setting counter for powers and position for rows
    counter<-counter+1
    insert_pos<-0
    
    #CALCULATE THE IDW ESTIMATOR
    #insert rows
    df_score<- add_row(df_score, index=paste(i, "Point_0", sep = "_"), x = Point_0x, y=Point_0y, .after = insert_pos) #add a row in pos 1 with same coordinates
    #set power
    d=idw_powers[counter]
    #calculating the denominator terms
    demoninator<-sum(1/distvect^d)
    #calculating the Numerator Matrix operation for IWD
    numerator_matrix<-val_matrix/(distvect^d) #creating a matrix of all data divided by its distances
    numeratorvect <- colSums(numerator_matrix) #sum it up to create numerator
    #Calculating IWD RESULT and write into score df
    df_score[(insert_pos+1), score_index$ind_df_score:score_index$len_df_score]<-numeratorvect/demoninator
    #Calculate Mean and SD
    df_score[(insert_pos+1), 4] <- sum(df_score[(insert_pos+1), score_index$ind_df_score:score_index$len_df_score])/n
    df_score[(insert_pos+1), 5] <- sd(df_score[(insert_pos+1), score_index$ind_df_score:score_index$len_df_score])
  }
  df_score
}
 
#CREATING SAMPLE MEAN ESTIMATOR 
get_mean_estim<-function(df_score, val_matrix, score_index, Point_0x, Point_0y){
  #Instead of IDW, we use also just the sample mean as one candidate estimator.
  insert_pos<-0
  n=length(val_matrix[1,])
  df_score<- add_row(df_score, index='Sample_Mean_Point_0', x = Point_0x, y=Point_0y, .after = insert_pos) #add a row in pos 1 with same coordinates
  #Calculating RESULT and inserting
  df_score[(insert_pos+1), score_index$ind_df_score:score_index$len_df_score]<-colMeans(val_matrix)
  #Calculate Mean and SD
  df_score[(insert_pos+1), 4] <- sum(df_score[(insert_pos+1), score_index$ind_df_score:score_index$len_df_score])/n
  df_score[(insert_pos+1), 5] <- sd(df_score[(insert_pos+1), score_index$ind_df_score:score_index$len_df_score])
  
  df_score
}

#CALCULATING THE ERRORS OF EACH ESTIMATOR 
get_errors<-function(df_score,score_index,df_stats,stats_index, Point_0x, Point_0y){
  
  #getting incides of all estimators on board
  
  indicies<-df_score[,1]
  i=1
  #loop thruopugh
  for (i in 1:length(indicies)){
    #calculating the vectors and then the errors
    y<-as.numeric(df_stats[stats_index$Verifindex, stats_index$ind_df_stat:stats_index$len_df_stat])
    x<-as.numeric(df_score[i, score_index$ind_df_score:score_index$len_df_score])
    z<- (x-y)
    n=length(z)
    #Calculate MAE, RMSE
    mae<- sum(abs(z))/n
    rmse<-sqrt(sum(z)^2/n)
    #adding new data row to DF score (take care not to convert all data to string format)
    newrow<-df_score[0, 1:score_index$len_df_score]
    newrow[1,1]<-gsub('Point_0', 'Error', indicies[i])
    newrow[1,2:score_index$len_df_score]<-c((Point_0x), (Point_0y), (mae), (rmse),(z))
    df_score = rbind(df_score,newrow)
  }

  #adding verification line
  #Calculate Mean and SD
  m <- mean(y)
  sd <- sd(y)
  #adding new data row to DF score (take care not to convert all data to string format)
  newrow<-df_score[0, 1:score_index$len_df_score]
  newrow[1,1]<-df_stats[stats_index$Verifindex, 1]
  newrow[1,2:score_index$len_df_score]<-c(Point_0x, Point_0y, m, sd, y)
  df_score = rbind(df_score,newrow)
  
  df_score

}


#******************************************************************************
#*******ANALYZE THE ERROR DISTRIBUTION (MAXIMUM LIKELIHOOD ESTIMAITON) ********
#******************************************************************************

#THIS FUNCTION MANAGES THE ENTIRE MLE AND KS TEST PROCEDURE AND STRUCTURES RESULTS
analyze_errors<-function(df_score,score_index, filenames){

  #creating clean list with all Estimator names
  estimator_names<- gsub("_Error","", filter(df_score, index %like% 'Err')[,1])
  #df_meta <- tibble(estimators = estimator_names,data = list(tibble(x = 1, y = 2),tibble(x = 4:5, y = 6:7),tibble(x = 10)))

  #creating the vector with all row indices of error terms to loop through
  err_ind<-which(df_score$index %like% 'Err')
  
  #initialize list for intermediary storage
  res<- list()
  
  #TESTING DIFFERENT CANDIDATE DISTRIBUTIONS AGAINST EACH OTHER FOR THE BEST FIT
  for (h in err_ind){

    #creating the error vector, main subject of this analysis
    error_vect <- as.numeric(df_score[h, score_index$ind_df_score:score_index$len_df_score])
    
    # creating Transpose of error DF for ggplot use
    error_df <-  as.data.frame(t(df_score[h, score_index$ind_df_score:score_index$len_df_score]))
    error_df<-cbind(Timestamps = rownames(error_df), error_df)
    colnames(error_df) <- c('Timestamps','errors')
    
    #creating the estimator name of this particular estimator
    estim_name<-gsub("_Error","", df_score$index[h])
    
    #Starting the Distributions Data Frame
    distr_names <- c("Normal", "Laplace", "Cauchy")
    params <- c("Mean", "SD")
    distributions <- data.frame(matrix(ncol = 3, nrow = 2))
    colnames(distributions) <-distr_names
    rownames(distributions) <-params
    
    #getting MLE for Normal
    distributions$Normal<-MLE_normal(error_vect)
    #getting MLE for Laplace
    distributions$Laplace<-MLE_laplace(error_vect)
    #getting MLE for Cauchy
    distributions$Cauchy<-MLE_cauchy(error_vect)
    #calling the KS Test Algorithm USING THE MLE ESTIMATORS
    distributions<-ks_test(error_vect, distributions)
    res<-c(res, list(distributions))
    
    #call the Plotting function (no return because saves plots)
    create_plot(error_df, distributions, estim_name, filenames)
    
  }
  #Nesting the resulting tables to a Nested DF and returning
  df_meta <- tibble(Estimators = estimator_names,Results = res)
  #Saving the Meta DF as JSON
  exportJSON <- toJSON(df_meta)
  write(exportJSON,  filenames$metaname)
  #returning file
  df_meta
}

#KOLMOGOROV-SMIRNOV TEST
ks_test<-function(error_vect, distributions){
  
  #Starting result Data rame
  distr_names <- c("Normal", "Laplace", "Cauchy")
  params <- c("KS_Stat", "KS_pval")
  result <- data.frame(matrix(ncol = 3, nrow = 2))
  colnames(result) <-distr_names
  rownames(result) <-params

  #executing the KS Test for different candidates
  ks_norm<-ks.test(error_vect, "pnorm", mean=as.numeric(distributions$Normal['Mean']), sd=as.numeric(distributions$Normal['SD']), alternative = "two.sided")
  ks_laplace<-ks.test(error_vect, "plaplace", m = as.numeric(distributions$Laplace['Mean']), s = as.numeric(distributions$Laplace['SD']), alternative = "two.sided")
  ks_cauch<-ks.test(error_vect, "pcauchy", location=as.numeric(distributions$Cauchy['Mean']), scale=as.numeric(distributions$Cauchy['SD']), alternative = "two.sided")

  #Writting the Results
  result['KS_Stat','Normal'] <-ks_norm$statistic
  result['KS_pval','Normal'] <-ks_norm$p.value
  #Writting the Results
  result['KS_Stat','Laplace'] <-ks_laplace$statistic
  result['KS_pval','Laplace'] <-ks_laplace$p.value
  #Writting the Results
  result['KS_Stat','Cauchy'] <-ks_cauch$statistic
  result['KS_pval','Cauchy'] <-ks_cauch$p.value

  #joining result DF and returning
  distributions<-rbind(distributions, result)
  distributions
}

#MAXIMUM LIKELIHOOD ESTIMATOR FOR NORMAL
MLE_normal<-function(error_vect){
  #For mu it is the sample mean
  MLE_Nor_mean<-mean(error_vect)
  #for Variance it is the Biased Sample variance
  MLE_Nor_SD<-sd(error_vect)
  #create vector and feed back
  MLE_Nor<-list(Mean=MLE_Nor_mean, SD=MLE_Nor_SD)
  MLE_Nor
}

#MAXIMUM LIKELIHOOD ESTIMATOR FOR LAPLACE
MLE_laplace<-function(error_vect){
  #For mu LAPLACE is the sample MEDIAN
  MLE_Lap_mean<-median(error_vect)
  #for Variance it is the Biased Sample variance
  n<-length(error_vect)
  MLE_Lap_SD<-sum(abs(error_vect-MLE_Lap_mean))/n
  #create vector and feed back
  MLE_Lap<-list(Mean=MLE_Lap_mean, SD=MLE_Lap_SD)
  MLE_Lap
}

#MAXIMUM LIKELIHOOD ESTIMATOR FOR CAUCHY
MLE_cauchy<-function(error_vect){
  
  #For the Cauchy, we cannot solve it conveniently in analytical form. therefore, we make a numerical optimization
  #Defining starting parameters
  MLE_Cau_mean<-0
  MLE_Cau_SD<-0
  z<-0
  
  #defining the trial range over which I want to optimize mu and gamma
  #play with the resoluton (by) you will need more iterations but get more exact results
  range_mu<-seq(min(error_vect)/2,max(error_vect)/2, by=1)#0.04
  range_gamma<-seq(0.1,max(error_vect)/3, by=1)#0.02
  
  #setting the initial value of z = the first terms in the optimization to make sure it is not general greater
  loglike_vect <-log(1/(pi*min(range_gamma)*(1+((error_vect-min(range_mu))/min(range_gamma))^2)))
  logsum<-sum(loglike_vect, na.rm = TRUE)
  
  #my self-made optimization algorithm just tries out over all mu-gamma combinations and
  #stores the parameters which maximizes the log sum
  for (j in range_gamma){
    for (i in range_mu){
      loglike_vect <-log(1/(pi*j*(1+((error_vect-i)/j)^2)))
      z<-sum(loglike_vect, na.rm = TRUE)
      if (z > logsum){
        MLE_Cau_mean<-i
        MLE_Cau_SD<-j
        logsum<-z
      }
    }
  }
  MLE_Cau<-list(Mean=MLE_Cau_mean, SD=MLE_Cau_SD)
  MLE_Cau
}

#CREATE PLOT AND GRAPHS
create_plot<-function(error_df, distributions, estim_name, filenames){
  
  #defining the color Vector for the use in the graphs
  colors <- c('Data'='#D6DCE4', 'MLE Normal' = "#4CB258", 'MLE Cauchy' = "#B80000", "MLE LaPlace" = "#4E7CC9")
  
  error_vect <- as.numeric(error_df$errors)
  MinErr<-min(error_vect)
  MaxErr<-max(error_vect)
  bw=(MaxErr-MinErr)/100
  n=length(error_vect)
  
  #filename for all files
  plot1_name<-paste(filenames$resultpath, paste(estim_name,'Distributions.png',sep = '_'), sep = '/')
  plot2_name<-paste(filenames$resultpath, paste(estim_name,'CDF.png',sep = '_'), sep = '/')
  plot3_name<-paste(filenames$resultpath, paste(estim_name,'Combined.png',sep = '_'), sep = '/')
  
  #creating GGplot Histogram + MLE Normal + MLE Cauchy + MLE Laplace and saving in variable
  graph1<-ggplot(data=error_df, aes(x=errors))  +
    geom_histogram(fill = '#D6DCE4', binwidth = bw) + 
    stat_function(fun = function(x) dnorm(x, mean = as.numeric(distributions['Mean', 'Normal']), sd = as.numeric(distributions['SD', 'Normal'])) * bw * n, colour = colors['MLE Normal'], size=1)+
    stat_function(fun = function(x) dcauchy(x, location = as.numeric(distributions['Mean', 'Cauchy']), scale = as.numeric(distributions['SD', 'Cauchy']))* bw * n,  colour = colors['MLE Cauchy'], size=1)+
    stat_function(fun = function(x) dlaplace(x, m = as.numeric(distributions['Mean', 'Laplace']), s = as.numeric(distributions['SD', 'Laplace'])) * bw * n, colour = colors['MLE LaPlace'], size=1)+
    theme_light()+
    #design funcitons
    labs(x = "Interpolation Error [ºK]",
         y = "count",
         title = paste("     ", "Error Distribution:", estim_name,"     "))

  #creating GGplot CDF/KS GRAPH + MLE Normal + MLE Cauchy + MLE Laplace and saving in variable
  graph2<-ggplot(data=error_df, aes(errors)) +
    stat_ecdf(geom = "step" , colour = colors['Data'], size=0.7)+
    stat_function(fun = function(x) pnorm(x, mean = as.numeric(distributions['Mean', 'Normal']), sd = as.numeric(distributions['SD', 'Normal'])),colour = colors['MLE Normal'], size=0.7)+
    stat_function(fun = function(x) pcauchy(x, location = as.numeric(distributions['Mean', 'Cauchy']), scale = as.numeric(distributions['SD', 'Cauchy'])), colour = colors['MLE Cauchy'], size=0.7)+
    stat_function(fun = function(x) plaplace(x, m = as.numeric(distributions['Mean', 'Laplace']), s = as.numeric(distributions['SD', 'Laplace'])), colour = colors['MLE LaPlace'], size=0.7)+
    theme_light()+
    #design funcitons
    labs(x = "Interpolation Error [ºK]",
         y = "F(x)",
         title = paste("       ", "CDF Errors:", estim_name),"       ")
  
  #Call Function to combine graphs in 1 plot
  combined_graph<-arrange_plots(graph1, graph2, distributions, estim_name, colors, filenames)
  
  #SAVING THE PICTURES AND DF IN THE TARGET DIR
  ggsave(plot3_name, plot = combined_graph, width = 7, height = 5)
  ggsave(plot1_name, plot = graph1, width = 4, height = 3)
  ggsave(plot2_name, plot = graph2, width = 4, height = 3)
}

#GETTING THE BEST FITTING DISTRIBUTION FROM A TABLE OF KS RESULTS AND RETURNING MEAN/SD, TYPE
get_best<-function(distributions) {
  i<-apply( distributions['KS_Stat',], 1, which.min)
  name<-colnames(distributions[i])
  Mean<-as.numeric(distributions['Mean',i])
  SD<-as.numeric(distributions['SD',i])
  return<-c(name,round(Mean,digits = 3), round(SD, digits = 3), i)
}
  
#THIS FUNCTION TAKES GRAPHS AND ARRANGES THEM IN A COMBINED PLOT INCLUDING TEXT AND TABLE
arrange_plots<-function(graph1, graph2, distributions, estim_name, colors, filenames){
  
  #getting best fit distribution
  best<-get_best(distributions)
  bestind<-as.numeric(best[4])+1
  
  #creating the main Chart Title
  est<-gsub('_',' ',estim_name)
  l1<-paste('   ', 'Estimator', est, sep = ' ')
  title <- ggparagraph(text = l1, face = "bold", size = 15, color = "#2C5890", lineheight=1)
  
  #creating Sub Title
  l2<-paste('     ',as.character(filenames$setname),' - ',as.character(filenames$variable), sep = '')
  case <- ggparagraph(text = l2, size = 12, color = "#4CB258", lineheight=1)
  
  #converting distribution nexted list into a ggtext table and transpose it
  df<-rbind(as.numeric(distributions$Normal[1:3]),as.numeric(distributions$Laplace[1:3]),as.numeric(distributions$Cauchy[1:3]))
  df<-round(df,3)
  df<-cbind(as.data.frame(colnames(distributions)), df)
  header<-c('Distribution', rownames(distributions)[1:3])
  colnames(df)<-header

  #Creating the Main Result Body
  main.title <- "Results of Error Distribution"
  subtitle <- paste0(
    "How are the errors of the interpolation  method ", est," distributed?",
    " The best fitting error distribution is the ", best[1],
    " with Mean = ", best[2],", and Standard Deviation = " ,best[3],"."
  ) %>%
    strwrap(width = 40) %>%
    paste(collapse = "\n")
  #joining the Table with Text Section and Title
  stable <- ggtexttable(df, rows = NULL, theme = ttheme("light", base_size = 10))
  stable<- table_cell_bg(stable, row = bestind, column = 1:4, linewidth = 0,fill="#FFBC00")
  tabl<-stable %>%
    tab_add_title(text = subtitle, face = "italic", size = 9) %>%
    tab_add_title(text = main.title, face = "bold", size = 12, padding = unit(0.5, "line"))

  #creating the Legend
  legtext<-as.data.frame(c('Data',as.character(df$Distribution)))
  colnames(legtext)<-'Legend'
  ltab <- ggtexttable(legtext, rows = NULL, theme = ttheme("light"))%>%
    tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)
  ltab <- table_cell_font(ltab, row = 2, column = 1,size = 9.5, color = colors['Data'])
  ltab <- table_cell_font(ltab, row = 3, column = 1, size = 9.5,color = colors['MLE Normal'])
  ltab <- table_cell_font(ltab, row = 4, column = 1, size = 9.5,color = colors['MLE LaPlace'])
  ltab <- table_cell_font(ltab, row = 5, column = 1, size = 9.5,color = colors['MLE Cauchy'])

  #import image for marketing
  imagesource<-paste(getwd(),'Material', 'Picture1.jpg', sep = '/')
  img <- readJPEG(imagesource)
  im_A <- ggplot() + background_image(img) # tried to insert the image as background.. there must be a better way
  #create sub plot for lower left
  p3 <- ggarrange(im_A, ltab,
                  ncol = 2, nrow = 1,
                  widths = c(0.52, 0.38))
  # create sub plot for the right side
  p1 <- ggarrange(graph1, graph2,
                  ncol = 1, nrow = 2)
  #create sub plot for the left side
  p2 <- ggarrange('',title,case, tabl,p3,'',
                  ncol = 1, nrow = 6,
                  heights = c(0.03,0.07,0.06, 0.65, 0.35, 0.02))
  
  #combine the plot
  combined_graph<-ggarrange(p2, p1, ncol = 2, nrow = 1,
            widths = c(0.9,1.1))
  #return plot
  combined_graph
}




#******************************************************************************
#***************************THIS IS JUST RUBBISH ****************************
#******************************************************************************

# # # #####Plotting with Legent integrated
# # # #creating GGplot Histogram + MLE Normal + MLE Cauchy + MLE Laplace and saving in variable
# # # graph1<- ggplot(data=error_df, aes(x=errors))  +
# # #   geom_histogram(fill = '#D6DCE4', binwidth = bw) + 
# # #   stat_function(fun = function(x) dnorm(x, mean = as.numeric(distributions['Mean', 'Normal']), sd = as.numeric(distributions['SD', 'Normal'])) * bw * n, aes(colour = 'MLE Normal'), size=1)+
# # #   stat_function(fun = function(x) dcauchy(x, location = as.numeric(distributions['Mean', 'Cauchy']), scale = as.numeric(distributions['SD', 'Cauchy']))* bw * n,  aes(colour = 'MLE Cauchy'), size=1)+
# # #   stat_function(fun = function(x) dlaplace(x, m = as.numeric(distributions['Mean', 'Laplace']), s = as.numeric(distributions['SD', 'Laplace'])) * bw * n,aes(colour = 'MLE LaPlace'), size=1)+
# # #   theme_light()+
# # #   #design funcitons
# # # 
# # # # labs(x = "Interpolation Error",
# # # #      y = "F(x)",
# # # #      color = "K-S Test/CDF" +
# # # #        title = paste("Cumulative Distribution:", estim_name)) +
# # # #   scale_color_manual(values = colors)