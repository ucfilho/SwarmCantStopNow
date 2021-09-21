library(writexl)
setwd('C:/Users/ucfil/OneDrive/Desktop/paper')
cat("\n \n \n hello world!!!! \n \n \n");
time=format(Sys.time(), "%a_%b_%d_%H_%M_%S.txt");
cat("nome do arquivo que vai salvar o calculo:",time)

DAT=read.table('Dados_001_Larrysse_ABRIL_05_2017.txt',header=T);
DAT=DAT[,-1];
df = as.data.frame(DAT)
write.csv2(df, file = "membrana.csv")
write_xlsx(df,   "Membrana_Congress.xlsx")
