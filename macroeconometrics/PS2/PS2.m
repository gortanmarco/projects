%% Q1
clear

n = 500;
alpha = 0.5;
rho = 1;
sigma = 1;
y = zeros(n,1); %initialize
epsilon = sqrt(sigma)*randn(n,1); %generate a vector epsilon size n with variance sigma
y(1) = epsilon(1);

% a) Generating with a for loop
for t=2:n
	y(t) = alpha + rho*y(t-1) + epsilon(t);
end

plot(y)
disp(mean(y))
disp(std(y)^2)
%figure;autocorr(y)


%%
% Now we compute the empirical distribution of the test statistic
clear t_ratio s_beta sig_hat b_hat iXX X Y

it = 10000; %number of simulations
for t = 1:it
    eps = sigma*randn(n,1);
    y = zeros(n,1);
    y(1) = eps(1);
    for i=2:n
	    y(i) = alpha + rho*y(i-1) + eps(i);
    end                         
    X = [ones(n-1,1) y(1:end-1,1)];        
    Y = y(2:end,1);                        
    iXX = inv(X'*X);
    rho_hat(:,t) = X\Y;                                   
    sig_hat(t) = cov(Y - X*rho_hat(:,t));                 
    s_rho = iXX(2,2)*sig_hat(t);                       
    t_ratio(t) = (rho_hat(2,t)-1)/sqrt(s_rho);   
end

[N,X] = hist(t_ratio,30);
plot(X,N/it);title('Histogram for t statistic');grid

histfit(t_ratio)
disp(mean(t_ratio))
disp(mean(rho_hat(2,:)))

%%
% We add a time dependent regressor and check the results from the previous
% questions

clear

n = 500;
alpha = 0.5;
rho = 1;
b = 1; %time dependent regressor added
sigma = 1;
it = 10000;

for t = 1:it
    eps = sigma*randn(n,1);
    y = zeros(n,1);
    y(1) = eps(1);
    for i=2:n
	    y(i) = alpha + b*(i-1) + rho*y(i-1) + eps(i);
    end                         
    X = [ones(n-1,1) (1:n-1)' y(1:end-1,1)];        
    Y = y(2:end,1);                        
    iXX = inv(X'*X);
    rho_hat(:,t) = X\Y;                                   
    sig_hat(t) = cov(Y - X*rho_hat(:,t));                 
    s_rho = iXX(3,3)*sig_hat(t);                       
    t_ratio(t) = (rho_hat(3,t)-1)/sqrt(s_rho);   
end

[N,X] = hist(t_ratio,30);
plot(X,N/it);title('Histogram for t statistic');grid

histfit(t_ratio)
disp(mean(t_ratio))
disp(mean(rho_hat(3,:)))

%% Q2

% QUESTION 2
% SPURIOUS REGRESSIONS

% CASE 1
clear 

n = 1000; % number of observations
it = 10000; % number of Monte Carlo simulations
a = 0.2;
b = 0.5;

sigma_x = 1;
sigma_y = 1;

for t=1:it
    epsilon_x = sigma_x*randn(n,1);
    epsilon_y = sigma_y*randn(n,1);

    x=filter(1,[1 -a], epsilon_x);
    y=filter(1,[1 -b], epsilon_y);
    
    % Now that we have our two independent time series, we regress y on x
    X = [ones(n-1,1) x(2:end,1)];
    Y = y(2:end,1);
    iXX = inv(X'*X);
    b_hat(:,t) = X\Y;
    res(:,t) = Y - X*b_hat(:,t);
    sig_hat(t) = cov(res(:,t));
    s_beta = iXX(2,2)*sig_hat(t);
    t_ratio(t) = (b_hat(2,t)-0)/sqrt(s_beta); %check if the coefficient is significant
    ssr(t) = sum(res(:,t).^2);
    sst(t) = var(y)*(n-1);
    R2(t) = 1-ssr(t)/sst(t);
end

[N,X] = hist(t_ratio,30);
plot(X,N/it);title('Histogram for t statistic');grid

disp(sum(abs(t_ratio)>1.96)/it)

%%
% CASE 2 : One random walk and one stationary process

clear 

n = 1000; % number of observations
it = 10000; % number of MC simulations
a = 1;
b = 0.2;

sigma_x = 1;
sigma_y = 1;

for t=1:it
    epsilon_x = sqrt(sigma_x)*randn(n,1);
    epsilon_y = sqrt(sigma_y)*randn(n,1);

    y = zeros(n,1);
    for i=2:n
	    y(i) =  a*y(i-1) + epsilon_y(i);
    end  

    x = zeros(n,1);
    for i=2:n
	    x(i) = b*x(i-1) + epsilon_x(i);
    end
    
    % Now that we have our two independent time series, we regress y on x
    
    X = [ones(n-1,1) x(2:end,1)];
    Y = y(2:end,1);
    iXX = inv(X'*X);
    b_hat(:,t) = X\Y;
    res(:,t) = Y - X*b_hat(:,t);
    sig_hat(t) = cov(res(:,t));
    s_beta = iXX(2,2)*sig_hat(t);
    t_ratio(t) = (b_hat(2,t)-0)/sqrt(s_beta); %check if the coefficient is significant
    ssr(t) = sum(res(:,t).^2);
    sst(t) = var(y)*(n-1);
    R2(t) = 1-ssr(t)/sst(t);
end

[N,X] = hist(t_ratio,30);
plot(X,N/it);title('Histogram for t statistic');grid

disp(sum(abs(t_ratio)>1.96)/it)

%%
% CASE 3 : Two random walks


clear 

n = 1000;
it = 10000;
a = 1;
b = 1;

sigma_x = 1;
sigma_y = 1;

for t=1:it
    epsilon_x = sigma_x*randn(n,1);
    epsilon_y = sigma_y*randn(n,1);

    y = zeros(n,1);
    for i=2:n
	    y(i) =  a*y(i-1) + epsilon_y(i);
    end  

    x = zeros(n,1);
    for i=2:n
	    x(i) = b*x(i-1) + epsilon_x(i);
    end
    
    % Now that we have our two independent time series, we regress y on x
    
    X = [ones(n-1,1) x(2:end,1)];
    Y = y(2:end,1);
    % With first-difference
    %X = [ones(n-1,1) x(2:end,1)-x(1:end-1,1)];
    %Y = y(2:end,1)-y(1:end-1,1);
    iXX = inv(X'*X);
    b_hat(:,t) = X\Y;
    res(:,t) = Y - X*b_hat(:,t);
    sig_hat(t) = cov(res(:,t));
    s_beta = iXX(2,2)*sig_hat(t);
    t_ratio(t) = (b_hat(2,t)-0)/sqrt(s_beta); %check if the coefficient is significant
    ssr(t) = sum(res(:,t).^2);
    sst(t) = var(y)*(n-1);
    R2(t) = 1-ssr(t)/sst(t);
end

[N,X] = hist(t_ratio,30);
plot(X,N/it);title('Histogram for t statistic');grid

disp(sum(abs(t_ratio)>1.96)/it)

%%
% CASE 4 : Two random walk but identical residuals
% Second example given by Enders

clear 

n = 1000;
it = 10000;
a = 1;
b = 1;

sigma_x = 1;
sigma_y = 1;

for t=1:it
    epsilon_x = sigma_x*randn(n,1);
    epsilon_y = sigma_x*randn(n,1);
    epsilon = randn(n,1);
    
    mu = zeros(n,1);
    for i=2:n
        mu(i)=mu(i-1) + epsilon(i);
    end
      
    y = zeros(n,1);
    for i=2:n
	    y(i) =  mu(i) + epsilon_y(i);
    end  

    x = zeros(n,1);
    for i=2:n
	    x(i) = mu(i) + epsilon_x(i);
    end
    
    % Now that we have our two independent time series, we regress y on x
    
    X = [ones(n-1,1) x(2:end,1)];
    Y = y(2:end,1);
    iXX = inv(X'*X);
    b_hat(:,t) = X\Y;
    res(:,t) = Y - X*b_hat(:,t);
    sig_hat(t) = cov(res(:,t));
    s_beta = iXX(2,2)*sig_hat(t);
    t_ratio(t) = (0 - b_hat(2,t))/sqrt(s_beta); %check if the coefficient is significant
    ssr(t) = sum(res(:,t).^2);
    sst(t) = var(y)*(n-1);
    R2(t) = 1-ssr(t)/sst(t);
end

[N,X] = hist(t_ratio,30);
plot(X,N/it);title('Histogram for t statistic');grid

disp(sum(abs(t_ratio)>1.96)/it)

%% Q3

clear

RomerRomer = xlsread("Romer_Romer.xlsx");

inflation = RomerRomer(:,1);
unemployment = RomerRomer(:,2);
ffr = RomerRomer(:,3);
RomerRomerQuarterly = RomerRomer(:,4); 

inf_0 = inflation(5:end);
inf_1 = inflation(4:end-1);
inf_2 = inflation(3:end-2);
inf_3 = inflation(2:end-3);
inf_4 = inflation(1:end-4);

unem_0 = unemployment(5:end);
unem_1 = unemployment(4:end-1);
unem_2 = unemployment(3:end-2);
unem_3 = unemployment(2:end-3);
unem_4 = unemployment(1:end-4);

ffr_0 = ffr(5:end);
ffr_1 = ffr(4:end-1);
ffr_2 = ffr(3:end-2);
ffr_3 = ffr(2:end-3);
ffr_4 = ffr(1:end-4);

rrq_0 = RomerRomerQuarterly(5:end);
rrq_1 = RomerRomerQuarterly(4:end-1);
rrq_2 = RomerRomerQuarterly(3:end-2);
rrq_3 = RomerRomerQuarterly(2:end-3);
rrq_4 = RomerRomerQuarterly(1:end-4);
%%estimation equation by equation
n = size(inf_0);

X_un = [ones(n) inf_1 inf_2 inf_3 inf_4 unem_1 unem_2 unem_3 unem_4 ffr_1 ffr_2 ffr_3 ffr_4 rrq_1 rrq_2 rrq_3 rrq_4];

iXX_un = inv(X_un'*X_un);

X_res = [ones(n) inf_1 inf_2 inf_3 inf_4 unem_1 unem_2 unem_3 unem_4 ffr_1 ffr_2 ffr_3 ffr_4];

iXX_res = inv(X_res'*X_res);

%inflation
Y = inf_0;

b_hat_un = X_un\Y;
b_hat_res = X_res\Y;

res_un = Y - X_un*b_hat_un;
res_res = Y - X_res*b_hat_res;

ssr_un = sum(res_un.^2);
ssr_res = sum(res_res.^2);
n_res = 4;
n_par_un = 17;

f_inf = ((ssr_res - ssr_un)/n_res)/(ssr_un/((size(Y,1)-n_par_un)));

%unemployment
Y = unem_0;

b_hat_un = X_un\Y;
b_hat_res = X_res\Y;

res_un = Y - X_un*b_hat_un;
res_res = Y - X_res*b_hat_res;

ssr_un = sum(res_un.^2);
ssr_res = sum(res_res.^2);
n_res = 4;
n_par_un = 17;

f_unem = ((ssr_res - ssr_un)/n_res)/(ssr_un/((size(Y,1)-n_par_un)));

%ffr
Y = ffr_0;

b_hat_un = X_un\Y;
b_hat_res = X_res\Y;

res_un = Y - X_un*b_hat_un;
res_res = Y - X_res*b_hat_res;

ssr_un = sum(res_un.^2);
ssr_res = sum(res_res.^2);
n_res = 4;
n_par_un = 17;

f_ffr = ((ssr_res - ssr_un)/n_res)/(ssr_un/((size(Y,1)-n_par_un)));

disp([f_inf, f_unem, f_ffr]);

%% Q4

clear 

n = 502;
beta = 0.6;
sigma2_eta = 1;
sigma2_eps = 0.8;
MC = 3000;

for i = 1:MC
    
    eta = sqrt(sigma2_eta)*randn(n,1);
    eps = sqrt(sigma2_eps)*randn(n,1);
    
    x = zeros(n,1);
    y = zeros(n,1);

    
    for t=3:n
        x(t) = eta(t) + eps(t-2);
        y(t) = (beta/(1-beta))*eta(t) + (beta^2/(1-beta))*eps(t) + beta*eps(t-1);
    end    
    x = x(3:end);
    y = y(3:end);

    %%VAR(4) equation by equation
    Y_x = x(5:end);
    Y_y = y(5:end);
    X = [x(4:end-1) x(3:end-2) x(2:end-3) x(1:end-4) y(4:end-1) y(3:end-2) y(2:end-3) y(1:end-4)];
    iXX = inv(X'*X);

    %x
    b_hat_x(:,i) = X\Y_x;
    b_hat_x_i = b_hat_x(:,i);
    res_x = Y_x - X*b_hat_x(:,i);
    
    %y
    b_hat_y(:,i) = X\Y_y;
    b_hat_y_i = b_hat_y(:,i);
    res_y = Y_y - X*b_hat_y(:,i);
    
    %companion form
    D_1 = [b_hat_x_i(1) b_hat_x_i(5); b_hat_y_i(1) b_hat_y_i(5)];
    D_2 = [b_hat_x_i(2) b_hat_x_i(6); b_hat_y_i(2) b_hat_y_i(7)];
    D_3 = [b_hat_x_i(3) b_hat_x_i(7); b_hat_y_i(3) b_hat_y_i(6)];
    D_4 = [b_hat_x_i(4) b_hat_x_i(8); b_hat_y_i(4) b_hat_y_i(8)];
    
    I = eye(2,2);
    O = zeros(2,2);
    
    D = [D_1 D_2 D_3 D_4;
        I O O O;
        O I O O;
        O O I O];
    
    %need to retrieve the structural errors. Assumption: the residuals
    %from the first equation are the structural
    X_res = [res_x];
    Y_res = res_y;
    a_21 = X_res\Y_res;
    
    %suppose res are reduced form shocks res = [res_x res_y]'
    %we obtain A such that A*res = [struct_x struct_y]' i.e. the vector of
    %structural shocks
    A = [1 0; a_21 1];
    
    %A for identification purposes later since the impulse responses
    %to the structural shocks from n periods are given by (D^n)*A
    for imp = 0:20
        C = D^imp;
        irf_est(:,:,i,imp+1)=C(1:2,1:2)*A;
    end
 
    
end

irf_eta_x = [1 0 0 0 0 0 0 0 0 0];
irf_eta_y = [beta/(1-beta) 0 0 0 0 0 0 0 0 0];
irf_eps_x = [0 0 1 0 0 0 0 0 0 0];
irf_eps_y = [beta^2/(1-beta) beta 0 0 0 0 0 0 0 0];


for i = 1:20
    mean_irf_etax(i) = mean(irf_est(1,1,:,i));
    mean_irf_etay(i) = mean(irf_est(2,1,:,i));
    mean_irf_epsx(i) = mean(irf_est(1,2,:,i));
    mean_irf_epsy(i) = mean(irf_est(2,2,:,i));
    
    irf_etax_05(i) = prctile(irf_est(1,1,:,i),5);
    irf_etay_05(i) = prctile(irf_est(2,1,:,i),5);
    irf_epsx_05(i) = prctile(irf_est(1,2,:,i),5);
    irf_epsy_05(i) = prctile(irf_est(2,2,:,i),5);
    
    irf_etax_95(i) = prctile(irf_est(1,1,:,i),95);
    irf_etay_95(i) = prctile(irf_est(2,1,:,i),95);
    irf_epsx_95(i) = prctile(irf_est(1,2,:,i),95);
    irf_epsy_95(i) = prctile(irf_est(2,2,:,i),95);
end

s= 0;
for m = 1:2
    for n = 1:2
        s= s+1;
        subplot(2,2,s)
        for i = 1:MC
            q = squeeze(irf_est(m,n,i,:));
            plot(q)
            hold on
        end
    end
end
hold off

irf_eta_x = [1 0 0 0 0 0 0 0 0 0];
irf_eta_y = [beta/(1-beta) 0 0 0 0 0 0 0 0 0];
irf_eps_x = [0 0 1 0 0 0 0 0 0 0];
irf_eps_y = [beta^2/(1-beta) beta 0 0 0 0 0 0 0 0];

hold on
subplot(2,2,1)
hold on
plot(irf_eta_x) 
hold on
plot(irf_etax_05)
plot(irf_etax_95)

subplot(2,2,2) 
plot(irf_eps_x)
hold on
plot(irf_epsx_05)
plot(irf_epsx_95)

subplot(2,2,3)
plot(irf_eta_y)
hold on
plot(irf_etay_05)
plot(irf_etay_95)

subplot(2,2,4) 
plot(irf_eps_y)
hold on
plot(irf_epsy_95)
plot(irf_epsy_05)
