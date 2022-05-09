FS_RRC <- function(df, list_of_feature, class_label){
  s_list <- data.frame(feature=NA, SU=NA)
  #rep(NA, length(df)
  i <- 1
  s_comp <- data.frame(feature=NA)
  score <- c()
  s_best <- data.frame()
  ### Working ###
  for(feature in list_of_feature){
    su <- 2*mutinformation(df[class_label],df[feature])/(entropy(df[feature])+entropy(df[class_label]))
    if(is.na(su)==TRUE) {break}
    if(su > 0){
      s_list[i,"feature"] <- feature
      s_list[i,"SU"] <- su
      i <- i+1
    }
  }
  s_list <- s_list %>% arrange(desc(SU))
  s_list <- subset(s_list, feature!=class_label)
  s_list <- s_list %>% select(feature)
  ################
  s <- s_list # s=s*_list #s is a data frame
  k <- 1
  f <- s_list[k,1]
  j <- 2
  #while(is.null(f)==FALSE){
  repeat{
    f_new <- s_list[j, 1]
    #while(is.null(f_new)==FALSE){
    # Working #
    repeat{
      su_f_fnew <- 2*mutinformation(df[f],df[f_new])/(entropy(df[f])+entropy(df[f_new]))
      su_fnew_c <- 2*mutinformation(df[class_label],df[f_new])/(entropy(df[class_label])+entropy(df[f_new]))
      if(su_f_fnew > su_fnew_c){
        s_list <- subset(s_list, feature=f_new)
      }
      j <- j+1
      f_new <- s_list[j, 1]
      if(is.na(f_new) == TRUE) {
        j <- 2
        break
      }
    }
    ####################
    #j <- 2
    # k <- 2
    t <- 1
    for(fnew in s[1]){
        if(fnew != f){
          score <- append(score,condinformation(X=df[f], Y=df[class_label], S=df[fnew], method="emp")-mutinformation(df[f],df[class_label]))
          # if(score > max_score){
          #max <- max(score)
          # }
        }
    }
      # s_comp[t,"feature"] <- which.max(score)
    #s_comp[t,"SU"] <- NA
        # if(is.na(f) == TRUE) break;
        # t <- t+1
        # f<-s[t, 1]
        # if(is.na(f) == TRUE) break
    #s_comp <- s_comp %>% select(feature)
    if(is.na(f) == TRUE) break;
    k <- k + 1
    f <- s[k, 1]
    if(is.na(f) == TRUE) break;
  }
  s_comp[t,"feature"] <- which.max(score)
  s_best <- rbind(s_list, s_comp)
  # s_comp
  s_best
}
FS_RRC(discretize(Parkinsson_disease), colnames(Parkinsson_disease), "status")
FS_RRC(discretize(tdata), colnames(tdata), "result")


S <- FS_RRC(discretize(toy), colnames(toy), "Y")




FS_RRC(divorce, colnames(divorce), "Class")
FS_RRC(divorce, c("Atr1","Atr2","Atr3","Atr4","Atr5"), "Atr44")
debug(FS_RRC(divorce, c("Atr1","Atr2","Atr3","Atr4","Atr5"), "Atr44"))

for(fnew in S[, 1]){
  print(fnew)}


# repeat{
#   fnew <- s[t,1]
#   if(fnew != f){
#       score <- append(score,condinformation(X=df[f], Y=df[class_label], S=df[fnew], method="emp")-mutinformation(df[f],df[class_label]))
#       # if(score > max_score){
#       #max <- max(score)
#       s_comp[t,"feature"] <- which.max(score)
#       s_comp[t,"SU"] <- max(score)
#       # }
#   }
#   else{
#     break;
#   }
#   if(is.na(f) == TRUE) break;
#     t <- t+1
# }
