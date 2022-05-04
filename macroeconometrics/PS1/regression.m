function [b,res,cov_b] = regression(y,x)


%b = inv(x'*x)*x'*y

b = x\y; % (x'*x)*b=x'*y -> x*b = y -> b = inv(x)*y (this cannot be done, as x is a vector!!)
%pause
res = y-x*b;

cov_b = inv(x'*x)*cov(res);
end