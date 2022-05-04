function [x,epsilon]=ARMApq(n,sigma_sq,AR,MA)
p=size(AR,2); 
q=size(MA,2);
x = zeros(n,1); %initialize
epsilon = sqrt(sigma_sq)*randn(n,1);
for i=1:max(p,q)
    x(i)=epsilon(i);
end
for t=max(p,q)+1:n
    if p~=0
        for i=1:p
            x(t) = x(t)+AR(i)*x(t-i);
        end
    end
    if q~=0
        for i=1:q
            x(t)=x(t)+MA(i)*epsilon(t-i);
        end
    end
    x(t) = x(t)+epsilon(t);
end