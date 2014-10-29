RandomNum<-function(row,col,method='Sampling',replace='T',max_bound=100){
 w<-matrix(0,row,col,replace='T')
 if(method=='Sampling'){
  ss<-sample(1:max_bound,col*row)
  for(a in 1:row){
   for(b in 1:col){
    w[a,b]<-ss[b*row+a-row]
   }
  }
 }
 else{
  rr<-rnorm(1:100,col*row)
  for(a in 1:row){
   for(b in 1:col){
    w[a,b]<-rr[b*row+a-row]
   }
  }
 }
 w
}
