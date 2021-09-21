# EXAMPLE 3: sphere, global minimum at about (0,0,...,0) -----------------
library(ABCoptim)


Sphere= function(x)
  {
  Num=length(x)
	Sum = 0.0
	for (i in 1:Num)
	  {
		Sum =Sum+ x[i]**2.0
		}
	fun=Sum
  return(fun)
  }

D = 20
POP= 500
NPAR = 50
Num = 3
fobj = rep(0,Num)
for (i in 1:Num)
{
ans = abc_optim(rep(5,D), Sphere, lb=-5.12, ub=5.12,FoodNumber = NPAR, criter=POP);
fobj[i] = ans$value
cat("repeticao=",i,"\n")

}

print(fobj);
cat("media",mean(fobj),"\n");
cat("desvio",sd(fobj),"\n");
cat("melhor",min(fobj),"\n");

################################
# media 3.869472e-16 
# desvio 4.815003e-17 
# melhor 3.314893e-16 
###############################

