

%%% Q1
n=500;
phi=0.5;
sigma_sq=0.6;
x = zeros(n,1); %initialize
epsilon = sqrt(sigma_sq)*randn(n,1); %generate a vector epsilon size n with variance 0.6
x(1)=epsilon(1); % the starting condition is the first term of the error term

% a) Generating with a for loop
for t=2:n
	x(t)= phi*x(t-1) + epsilon(t);
end

%plot(x)
disp('mean of the process is');
disp(mean(x));
disp('variance of the process is');
disp(std(x)^2);

% b) Generating using the filter function
% The MA part is 1 and the coefficient of the AR part are in the second input of the function filter.

b=[1 -phi];
y=filter(1,b,epsilon);

disp('mean of the process is')
disp(mean(y));
disp('actual variance is');
disp(std(y)^2);
%figure;autocorr(y)

%theoretical variance of our AR(1) process
disp('theoretical variance is');
disp(sigma_sq/(1-phi^2));
plot(x)

%% Q2 
clear

n=500;
phi=0.7;
sigma_sq=0.5;
omega=2; % desired mean
mu=(1-phi)*omega; %define a constant term
x = zeros(n,1); %initialize
epsilon = sqrt(sigma_sq)*randn(n,1); %generate a vector epsilon size n with variance 0.6
x(1)=-100; % starting condition

% Generating with a for loop the AR(1) process
for t=2:n
	x(t)= mu + phi*x(t-1) + epsilon(t);
end

plot(x(1:100))
disp("new")
disp(mean(x))
disp(std(x)^2)
%figure;autocorr(x)

%unconditional variance of our AR(1) process
disp(sigma_sq/(1-phi^2))

%% Q3 

clear

n=500;
theta=0.5;
sigma_sq=0.4;
x = zeros(n,1); %initialize
epsilon = sqrt(sigma_sq)*randn(n,1); %generate a vector epsilon size n with variance 0.4
x(1)=epsilon(1);

% a) Generating with a for loop
for t=2:n
	x(t)= theta*epsilon(t-1) + epsilon(t);
end

%plot(x)
disp(mean(x))
disp(std(x)^2)

% b) Generating using the filter function
b=[1 theta];
y=filter(b,1,epsilon);

disp(mean(y))
disp(std(y)^2)
%figure;autocorr(y)

%theoretical variance of our MA(1) process
disp(sigma_sq*(1+theta^2))


%% Q5
clear
T = 300;
phi = 0.4;
sigma_sq = 1;

simu = 1000;
coeff = 0;
t_stat = 0;
for t = 1:simu
    y = 0;
    for i = 1:T
        y(i+1) = y(i)*phi + sqrt(sigma_sq)*randn(1);
    end
    y=y.';
    [beta,res,cov_b] = regression(y(2:end), y(1:end-1));
    
    coeff(t) = beta(1);
    t_stat(t) = beta(1)/(cov_b(1,1)^.5);
end
sum(abs(t_stat)>1.96)/simu;

%% Q6

T = 250;
theta = 0.4;

simu = 1000;
coeff = 0;
t_stat = 0;
for t = 1:simu
    y = 0;
    eps = 0;
    for i = 1:T
        eps(i+1) = randn(1);
        y(i+1) = 0.4*eps(i) + eps(i+1);
    end
    y=y.';
    [beta,res,cov_b] = regression(y(2:end),y(1:end-1));
    
    coeff(t) = beta;
end
disp('theoretical mean is')
disp(theta/(1+theta^2))
disp('mean of the AR(1) coefficient is');
disp(mean(coeff));
disp('std of the AR(1) coefficient is');
disp(std(coeff));
histogram(coeff)



T = 2500;
theta = 0.4;

simu = 1000;
coeff = 0;
t_stat = 0;
for t = 1:simu
    y = 0;
    eps = 0;
    for i = 1:T
        eps(i+1) = randn(1);
        y(i+1) = 0.4*eps(i) + eps(i+1);
    end
    y=y.';
    [beta,res,cov_b] = regression(y(2:end),y(1:end-1));
    
    coeff(t) = beta;
end
disp('theoretical mean is')
disp(theta/(1+theta^2))
disp('mean of the AR(1) coefficient is');
disp(mean(coeff));
disp('std of the AR(1) coefficient is');
disp(std(coeff));
histogram(coeff)

%% Q7

T = 250;
phi = 1;

simu = 2500;
coeff = 0;
coeff2 = 0;
t_stat = 0;
t_stat2 = 0;
for t = 1:simu
    y = 0;
    for i = 1:T
        y(i+1) = y(i)*phi + randn(1);
    end
    y=y.';
    y1 = y(2:end);
    y2 = y(1:end-1);
    [beta,res,cov_b] = regression(y1,[ones(size(y1,1),1) y2]);
    [beta2, res2, cov_b2] = regression(diff(y),[ones(size(y2,1),1) y2]);
    coeff(t) = beta(2);
    t_stat(t) = (beta(2) - phi)/(cov_b(2,2)^.5);
    coeff2(t) = beta2(2);
    t_stat2(t) = beta2(2)/(cov_b2(2,2)^.5);
    
end


h1 =histogram(t_stat2);title('Histogram for a');
h1.Normalization = 'probability';
h1.BinWidth = 0.1;

%-----------------------------------
sum(abs(t_stat2)>1.96)/simu;
skewness(y)
%-----------------------------------
disp('theorical DF at 1% and 5% without a trend')
disp([-3.43	-2.86])
disp('Empirical percentiles at 1% and 5%')
disp([prctile(t_stat2,1) prctile(t_stat2,5)])

%% Q7

T = 250;
phi = 1;
ssr2 = 0;
ssr3 = 0;
simu = 10000;
coeff = 0;
coeff2 = 0;
t_stat = 0;
t_stat2 = 0;
f = 0;
resq =0;
for t = 1:simu
    y = 0;
    for i = 1:T
        y(i+1) = 0.05 + y(i)*phi + randn(1);
    end
    y=y.';
    y1 = y(2:end);
    y2 = y(1:end-1);
    [beta,res,cov_b] = regression(y1,[ones(size(y1,1),1) y2]);
    [beta2, res2, cov_b2] = regression(diff(y),[ones(size(y2,1),1) y2]);
    coeff(t) = beta(2);
    t_stat(t) = (beta(2) - phi)/(cov_b(2,2)^.5);
    coeff2(t) = beta2(2);
    t_stat2(t) = beta2(2)/(cov_b2(2,2)^.5);
    ssr2(t) = sum(res2.^2);
    ssr3(t) = sum(diff(y).^2);
    f(t) = ((ssr3(t) - ssr2(t))/2)/(ssr3(t)/(250-2));
   
end


histogram(t_stat2)
prctile(t_stat2,1)
prctile(t_stat2,5)
%-----
sum(f>4.71)/simu;
%----

h1 =histogram(f);title('Histogram for f');
h1.Normalization = 'probability';
h1.BinWidth = 0.01;


%----------------------
% POINT C
T=250;
simu=1000;
a=0.5;
g=.04;
time = (1:T)';
f = 0;
ssr2=0;
ssr3=0;
%z=0
for t=1:simu
    y=0;
    epsilon=sqrt(2)*randn(T);
    y(1)=epsilon(1);
    for j=2:T
        y(j)=a+g*j+epsilon(j-1);
    end
    y = y.';
    y1 = y(2:end);
    y2 = y(1:end-1);
    [beta,res,cov_b] = regression(y1,[ones(size(y1,1),1) y2]);
    [beta2, res2, cov_b2] = regression(diff(y),[ones(size(y2,1),1) y2 time(2:end)]);
    ssr2(t) = sum(res2.^2);
    ssr3(t) = sum(diff(y).^2);
    f(t) = ((ssr3(t) - ssr2(t))/3)/(ssr3(t)/(250-3));
   
end
sum(f>4.88)/simu;
    