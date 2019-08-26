# Algorithm
up_phanbo <- function(rf,var_port,e_port,opt_weights_i){
  A<-t(e_port)%*%solve(var_port)%*%e_port
  B<-t(e_port)%*%solve(var_port)%*%matrix(rep(1,nrow(e_port)),byrow = T)
  C<-t(matrix(rep(1,nrow(e_port)),byrow = T))%*%solve(var_port)%*%matrix(rep(1,nrow(e_port)),byrow = T)
  D<-A*C-(B)^2
  
  min_expectation<-as.numeric(B/C) #Expectation of Portfolio
  min_sd<-as.numeric(sqrt(1/C)) #SD of Portfolio
  min_var<-as.numeric((1/C))#Var of portfolio
  
  
  #(iii)********************************error
  #**********
  #a)
  exp_pstar<-as.numeric(((rf*B)-A)/((rf*C)-B))
  sd_pstar<-as.numeric(sqrt(((C*((exp_pstar)^2)-(2*B*exp_pstar)+A)/D)))
  slope<-as.numeric((D*sd_pstar)/(C*exp_pstar-B))
  
  #b)  compute the market price of risk
  (as.numeric(D)*sd_pstar)/((as.numeric(C)*exp_pstar)-as.numeric(B))
  
  #C)
  ###calculating optimal weights
  opt_weights<-solve(var_port)%*%matrix(c(e_port,rep(1,nrow(e_port))),byrow = F,ncol = 2)%*%matrix(c(C,(-B),(-B),A),byrow = T,ncol = 2)%*%matrix(c(exp_pstar,1),ncol=1,byrow=T)%*%(1/D)
  opt_weights = t(ifelse(opt_weights <= 0,0,opt_weights/sum(opt_weights[opt_weights>0])))
  opt_weights_i <- opt_weights
  return(opt_weights_i)
}

# i day will have in the begining
n = as.numeric(KS[2])
i= date
if (i %in% c(1:90,182:212,305:365)){
  book_1 = sample(4:6,1)
  book_2 = sample(6:8,1)
  book_3 = sample(8:11,1)
  ai_1 = sample(1:3,1)
  ai_2 = sample(1:4,1)
  ai_3 = sample(1:5,1)
  ag_1 = sample(3:6,1)
  ag_2 = sample(5:8,1)
  ag_3 = sample(7:11,1)
} else{
  book_1 = sample(3:6,1)
  book_2 = sample(3:7,1)
  book_3 = sample(3:9,1)
  ai_1 = sample(1:2,1)
  ai_2 = sample(0:3,1)
  ai_3 = sample(0:4,1)
  ag_1 = sample(2:6,1)
  ag_2 = sample(2:8,1)
  ag_3 = sample(2:9,1)
}
k = 1
rf <- as.numeric(rf_table[i,2])
var_port <- as.matrix(covmatrix_data[which(covmatrix_data$code_date == i),2:4]) #Variance covariance matrix
rownames(var_port) = colnames(var_port)
e_port <- t(as.matrix(mean_data[which(mean_data$code_date == i),2:4])) #expectation of portfolio
opt_weights_i <- Total_weights[i,]
Total_room <- round(opt_weights_i*n)
Total_room[3] <- n - sum(Total_room[1:2])
opt_room_i <- Total_room
m <- c(0,0,0)
SPUT_t <- c(0,0,0)
# Generate Cong suat thuc tai
#install.packages('truncnorm')
#library(rtruncnorm)

#for (t in c(0:(24*k))) {
  if (!0 %in% opt_room_i) {
    if(t %in% c(((7*k)+1):(10*k))){
      SPUT_t = 6.67*opt_weights_i + SPUT_t
      BE_1 = c(round(runif(1, min = 0, max = book_1),0),round(runif(1, min = 0, max = ai_1),0),round(runif(1, min = 0, max = ag_1),0))
      
    } else if(t %in% c(((10*k)+1):(14*k))){
      SPUT_t = 8.75*opt_weights_i + SPUT_t
      BE_1 = c(round(runif(1, min = 0, max = book_2),0),round(runif(1, min = 0, max = ai_2),0),round(runif(1, min = 0, max = ag_2),0))
      
    } else if(t %in% c(((14*k)+1):(18*k))){
      SPUT_t = 11.25*opt_weights_i + SPUT_t
      BE_1 = c(round(runif(1, min = 0, max = book_3),0),round(runif(1, min = 0, max = ai_3),0),round(runif(1, min = 0, max = ag_3),0))
    }   else {
      SPUT_t = 0*opt_weights_i + SPUT_t
      BE_1 = c(0,0,0)
    }
    BE_1 = ifelse(opt_room_i-BE_1>=0,BE_1,0)
    m = m + BE_1
    BE_2 = m/ifelse(SPUT_t!=0,SPUT_t,1)
    n = n - sum(BE_1)
    e_port[1] <- max(e_port[1],BE_2[1])
    e_port[2] <- max(e_port[2],BE_2[2])
    e_port[3] <- max(e_port[3],BE_2[3])
    CSexp_Book = max(DTM[i,3],BE_2[1])
    CSexp_Ai = max(DTM[i,4],BE_2[2])
    CSexp_Ag = max(DTM[i,5],BE_2[3])
    BE_4 = c()
    BE_4[1] = CSexp_Book*opt_room_i[1]*DTM[i,6]
    BE_4[2] = CSexp_Book*opt_room_i[2]*DTM[i,7]
    BE_4[3] = CSexp_Book*opt_room_i[3]*DTM[i,8]
    opt_weights_i <- up_phanbo(rf,var_port,e_port,opt_weights_i)
    opt_room_i <- round(opt_weights_i*n)
    opt_room_i[3] <- n - sum(opt_room_i[1:2])
    RV = sum(m * DTM[i,c(6:8)])
    BE_5 = ifelse((DTM[i,9] - RV)<= 0 ,0,(DTM[i,9] - RV))
  } else if (0 %in% opt_room_i & sum(opt_room_i)!= 0){
    BE_1 = c(round(runif(1, min = 0, max = book_3),0),round(runif(1, min = 0, max = ai_3),0),round(runif(1, min = 0, max = ag_3),0))
    #BE_1[2] = sample(1:(n-BE_1[1]),1)
    #BE_1[3] = n - sum(BE_1[c(1,2)])
    SPUT_t[1] = sample(1:n,1)
    SPUT_t[2] = sample(1:(n-SPUT_t[1]),1)
    SPUT_t[3] = n-sum(SPUT_t[c(1,2)])
    BE_1 = ifelse(opt_room_i-BE_1>=0,BE_1,0)
    m = m + BE_1
    m
    BE_2 = m/(SPUT_t)
    BE_2
    n = n - sum(BE_1)
    e_port[1] <- max(e_port[1],BE_2[1])
    e_port[2] <- max(e_port[2],BE_2[2])
    e_port[3] <- max(e_port[3],BE_2[3])
    CSexp_Book = max(DTM[i,3],BE_2[1])
    CSexp_Ai = max(DTM[i,4],BE_2[2])
    CSexp_Ag = max(DTM[i,5],BE_2[3])
    BE_4 = c()
    BE_4[1] = CSexp_Book*opt_room_i[1]*DTM[i,6]
    BE_4[2] = CSexp_Book*opt_room_i[2]*DTM[i,7]
    BE_4[3] = CSexp_Book*opt_room_i[3]*DTM[i,8]
    opt_weights_i <- up_phanbo(rf,var_port,e_port,opt_weights_i)
    opt_room_i <- round(opt_weights_i*n)
    opt_room_i[3] <- n - sum(opt_room_i[1:2])
    BE_1
    m
    opt_room_i
    RV = sum(m * DTM[i,c(6:8)])
    BE_5 = ifelse((DTM[i,9] - RV)<= 0 ,0,(DTM[i,9] - RV))
  } else{
    break
  }
#}