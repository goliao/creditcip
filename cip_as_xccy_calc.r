
#constant*interest rate
r=.03
n=10
PV=(1/(1+r))



n=10
z=.05
# pv exact
-((z+1)^-n-1)/z
#pv approx
n-1/2*(n*(n+1))*z

# disc of X: exact
-(((z+1)^-n-1)/(z*n))*(1+z)
# disc of X: 2nd order taylor approximation
1+1/2*(1-n)*z
# 3rd order taylor approximation
1+1/2*(1-n)*z+1/6*(n^2-1)*z^2

x=1
T=n

((z+1)^T)^(1/T)-z-1
(((z+1)^T)^(1/T-1)*((z+1)^T-1))/(T*z)
