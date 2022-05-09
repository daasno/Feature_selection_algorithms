R2CI <- function(df, list_of_feature, class_label){
  Rel_list <- data.frame(feature=NA, REL=NA)
  Final_list <- c()
  #Itr_list <- data.frame(feature=NA, ITR=NA)
  S <- data.frame(feature=NA)
  #for(feature in colnames(df)){
  new_df <- df
  for(t in 1:ncol(new_df)){
    #if(is.na(df[class_label]) == TRUE) {break}
    Rdd <- 0
    Cpl <- 0
    Itr <- 0
    for(i in 1:ncol(df)){
    ########### Class-Relevance #########
    #if(df[i] != df[class_label]){
      Rel <- mutinformation(X=df[i], Y=new_df[class_label], method="emp")
      Rel_list[i,"feature"] <- colnames(df[i])
      Rel_list[i,"REL"] <- Rel
      #i <- i+1
    #}
    }
    Rel_list <- slice(Rel_list, 1:(n() - 1))
    Rel_list <- Rel_list %>% arrange(desc(REL))
    S <- rbind(S, Rel_list[1,1])
    S <- na.omit(S)
    if (dim(df)[2] != 0){
      df <- df %>% select(-Rel_list[1, 1])
      }
    #Rel_list <- data.frame(feature=NA, REL=NA)
    if (dim(S)[1] != 0){
      for(s in 1:nrow(S)){
        #if(is.na(s) == TRUE) break;
        f <- as.character(S[s,1])
        if(is_empty(f) == T) next
        Rdd <- Rdd + condinformation(new_df[f], new_df[class_label], S=new_df[t], method="emp") - mutinformation(X=new_df[f], Y=new_df[class_label])
        Cpl <- Cpl + condinformation(new_df[t], new_df[class_label], S=new_df[f], method="emp")
        #print(f)
        }
        if(is_empty(df) == TRUE) break;
      n <- ncol(df) - 1
    for(j in 1:n){
      if(dim(df)[2] == 1) break;
      Itr <- Itr + condinformation(X=new_df[t], Y=new_df[class_label], S=df[j], method="emp")- mutinformation(new_df[t],new_df[class_label])
    }
      Score <- Rel_list[1, 2] + (1/nrow(S))*(Rdd+Cpl) + (1/(nrow(df)-1))*Itr
      if(Score > 0){
        Final_list <- append(Final_list, colnames(new_df[t])) 
      }
      Score <- 0
      Rel_list <- data.frame(feature=NA, REL=NA)
      }
    #   if (dim(df)[2] == 1) break;
    #   Rel_list <- data.frame(feature=NA, REL=NA)
  }
  return(Final_list)
}

R2CI(discretize(Parkinsson_disease), colnames(Parkinsson_disease), 'status')

