library(AMORE, lib.loc="/home/ucfilho/Documents/R_Lang")
library(foreach, lib.loc="/home/ucfilho/Documents/R_Lang")
library(iterators, lib.loc="/home/ucfilho/Documents/R_Lang")
library(GA, lib.loc="/home/ucfilho/Documents/R_Lang")
# source('PROG_006_Membrana_otimiza_mar_21_2017.R')
# setwd('/home/ucfilho/Documents/Doutorados_Mestrados/Doutorado_Larrysse/Paper_Manganes/Paper_Calcula')

cat("\n \n \n hello world!!!! \n \n \n");
time=format(Sys.time(), "%a_%b_%d_%H_%M_%S.txt");
cat("nome do arquivo que vai salvar o calculo:",time)

DAT=read.table('Dados_001_Larrysse_ABRIL_05_2017.txt',header=T);
DAT=DAT[,-1];
print(DAT)

TREINA=DAT[,1:5]/1.0; TARGET=DAT[,6]/max(DAT[,6]);TESTE=TREINA; 

RMSE=function(y.calc,TARGET)
{
  Num=length(y.calc);
  soma=0;
  for (i in 1:Num){soma=soma+(y.calc[i]-TARGET[i])^2; }
  soma=(soma/Num)^(0.5);
  return(soma);
}


num.kk=9;num.jj=20;MAT.EFEITO=matrix(data=NA,nrow=num.jj,ncol=5,byrow =T);

Lista=runif((num.kk*num.jj), min=0, max=300);cont.lista=0;
Neuro.Entra=5;rede=0;


  ref=1e99;
  Neurons=9 ;
  for(jj in 3:num.jj){


  cont.lista= cont.lista+1;set.seed(Lista[cont.lista]);# cat('aleatorio=',Lista[cont.lista],'\n');
  NET= newff(n.neurons=c(Neuro.Entra,Neurons,1),
           learning.rate.global=1e-2,
           momentum.global=0.5,
           error.criterium="LMS",Stao=NA, hidden.layer="tansig",
           output.layer="purelin",
           method="ADAPTgdwm");

  result = train(NET, TREINA, TARGET,
               error.criterium="LMS",
               report=FALSE,
               show.step=100,
               n.shows=100,
               Stao=NA,
               prob=NULL);


  y.calc= sim(result$net,TESTE);
  rmse=0;rmse=RMSE(TARGET,y.calc);# cat("rmse=",rmse,"\n");
  if( rmse< ref) { rede=result;ref=rmse;}
 }  # fim do   for(jj in 1:num.jj){

 
y.calc= sim(rede$net,TESTE);cat("rmse=",rmse,"\n");

MAX=max(DAT[,6]);

fun = function(x) 
{
  x=as.vector(x)
  ENTRA.OTIMIZA=matrix(c(x[1],x[2],x[3],x[4],x[5]),ncol=5);
  y.calc = sim(rede$net,ENTRA.OTIMIZA)*(-1)*MAX;
  return(y.calc);
} # fim ----> fun <- function(x) {

MIN.GA=c(-2,-2,-2,-2,-2);
MAX.GA=c(2,2,2,2,2);

x=rep(0,5)
GA = ga(type = "real-valued", fitness =function(x) -fun(x),
         min = MIN.GA, max = MAX.GA,
         popSize =600, maxiter = 400)
summary(GA)
dev.new();plot(GA)
Mat=0;
Mat=GA@solution
print(Mat)

y.calc = sim(rede$net,Mat)*MAX;
cat("Maximo=",y.calc,"\n")

X.menos=c(0.08,4,0.4,8,4)
X.mais=c(0.16,8,0.8,24,12);
Nomes=c("D2EHPA","Tempo","Acido","NaCl","EDTA")
a=(X.mais+X.menos)/2;
b=(X.mais-X.menos)/2;
Y=a+b*Mat;
colnames(Y)=Nomes;print(Y);



