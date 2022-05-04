clear all
%-----------------------------------------------------

% 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 

%------------------------------------------------------

b_1 = -0.6;
alpha = -0.5/b_1;
b_2 = 0.5;
alpha == 5/12*1/b_2

%-----------------------------------------------------

% 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3

%------------------------------------------------------

load_1 = [-0.6 0.5; 0.5 -5/12];
load_2 = -[0.4 0.5; 0 0];
load_3 = [1 0; 0 1];

C_1 = load_1 + load_2 + load_3;
C_2 = [0.4 0.5; 0 0];
%-----------------------------------------------------

% 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4

%------------------------------------------------------

I = eye(2,2);
O = zeros(2,2);

C = [C_1 C_2;
    I O];

omega = [1 0; 0 1];

irf_est = [];
for t=0:20
    temp=C^t;
    irf_est(:,:,1,t+1) = temp(1:2,1:2)*omega;
end


s= 0;
figure
for m = 1:2
    for n = 1:2
        s= s+1;
        subplot(2,2,s)
        q = squeeze(irf_est(m,n,1,:));
        pl = plot(q);
        set(pl,'Color','k','LineStyle','-')
        hold on
     end
end


%% --------------------------------------

% 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5

%% -----------------------------------------
% a a a a a a a a a a a a a a a a a a a a 
%-----------------------------------------------
K=1000;
T = 250;
for i=1:K
    %starting condition, first observations
    data = zeros(2,1);
    data(:,1) = zeros(2,1);
    data(:,2) = zeros(2,1);
    
    
    %run the generation
    for j=3:T
        data(:,j) = C_1*data(:,j-1) + C_2*data(:,j-2) + randn(2,1);
    end
    
    data = data';
    X = [ones(T-2,1) data(2:end-1,:) data(1:end-2,:)];
    y = data(3:end,:);
    
    b_hat = X\y;
    
    res = y - X*b_hat;
    
    C_1_sim = [b_hat(2:3,:)'];
    C_2_sim = [b_hat(4:5,:)'];
    
    I = eye(2,2);
    O = zeros(2,2);

    C_sim = [C_1_sim C_2_sim;
        I O];
    
    omega = [1 0; 0 1];
    
    for t=0:10
        temp=C_sim^t;
        irf_est(:,:,i+1,t+1) = temp(1:2,1:2)*omega;
    end
    
    
end
   

s= 0;
prc = [2.5 50 97.5];
for m = 1:2
    for n = 1:2
        for i = 1:11
            for k = [2.5 50 97.5]
                irf(m,n,i, find(prc==k)) =  prctile(irf_est(m,n,:,i),k);
            end
        end
    end
    
end

s= 0;
figure
for m = 1:2
    for n = 1:2
        s= s+1;
        for k = 1:3
            subplot(2,2,s);            
            q = squeeze(irf(m,n,:,k));
            pl = plot(q);
            if k ==2
                set(pl,'Color','k','LineStyle','-')
            else
                set(pl,'Color','k','LineStyle','--')
            end
            hold on
        end
    end
end

%% -----------------------------------------
% b b b b b b b b b b b b b b b b b b b b b b 
%-----------------------------------------------
T = 250;
K=1000;

for i=1:K
    %starting condition, first observations
    data = zeros(2,1);
    data(:,1) = zeros(2,1);
    data(:,2) = zeros(2,1);
    
    
    %run the generation
    for j=3:T
        data(:,j) = C_1*data(:,j-1) + C_2*data(:,j-2) + randn(2,1);
    end
    
    data = data';
    X = [ones(T-2,1) diff(data(1:end-1,:))];
    y = diff(data(2:end,:));
    
    b_hat = X\y;
    
    res = y - X*b_hat;
    
    C_sim = [b_hat(2:end,:)'];
    
    omega = [1 0; 0 1];
    
    temp = C_sim^0;
    irf_est(:,:,i+1,1) = temp(1:2,1:2)*omega;
    for t=1:10
        temp=C_sim^t;
        irf_est(:,:,i+1,t+1) = irf_est(:,:,i+1,t)+ temp(1:2,1:2)*omega;
    end
    
    
end
   

s= 0;
prc = [5 50 95];
for m = 1:2
    for n = 1:2
        for i = 1:11 
            for k = [5 50 95]
                irf(m,n,i, find(prc==k)) =  prctile(irf_est(m,n,:,i),k);
            end
        end
    end
    
end



s= 0;
figure
for m = 1:2
    for n = 1:2
        s= s+1;
        for k = 1:3
            subplot(2,2,s);            
            q = squeeze(irf(m,n,:,k));
            pl = plot(q);
            if k ==2
                set(pl,'Color','k','LineStyle','-')
            else
                set(pl,'Color','k','LineStyle','--')
            end            
            hold on
        end
    end
end

%%

%-----------------------------------------
% c c c c c c c c c c c c c c c c c c c  c c
%-----------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% With 1 lag
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
K=1000;

for i=1:K
    %starting condition, first observations
    data = zeros(2,2);
    %data(:,1) = zeros(2,1);
    %data(:,2) = zeros(2,1);
    
    %run the generation
    for j=3:T
        data(:,j) = C_1*data(:,j-1) + C_2*data(:,j-2) + randn(2,1);
    end
    
    data = data';
    X = [ones(T-2,1), diff(data(1:end-1,:))];
    y = diff(data(2:end,:));
    
    b_hat = X\y;

    res = y - X*b_hat;
    
    omega = [1 0; 0 1];
    C_sim=b_hat(2:3,:)';
    temp = C_sim^0;
    irf_est(:,:,i+1,1) = temp(1:2,1:2)*omega;
    for t=1:10
        temp=C_sim^t;
        irf_est(:,:,i+1,t+1) = irf_est(:,:,i+1,t)+ temp(1:2,1:2)*omega;
    end 
end
   

s= 0;
prc = [5 50 95];
for m = 1:2
    for n = 1:2
        for i = 1:11 
            for k = [5 50 95]
                irf(m,n,i, find(prc==k)) =  prctile(irf_est(m,n,:,i),k);
            end
        end
    end
end

s= 0;
figure
for m = 1:2
    for n = 1:2
        s= s+1;
        for k = 1:3
            subplot(2,2,s);            
            q = squeeze(irf(m,n,:,k));
            pl = plot(q);
            if k ==2
                set(pl,'Color','k','LineStyle','-')
            else
                set(pl,'Color','k','LineStyle','--')
            end            

            hold on
        end
    end
end
 
%%
% With 2 lags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
K=1000;

for i=1:K
    %starting condition, first observations
    data = zeros(2,2);
    %data(:,1) = zeros(2,1);
    %data(:,2) = zeros(2,1);
    
    %run the generation
    for j=3:T
        data(:,j) = C_1*data(:,j-1) + C_2*data(:,j-2) + randn(2,1);
    end
    
    data = data';
    X = [ones(T-3,1), diff(data(2:end-1,:)) diff(data(1:end-2,:))];
    y = diff(data(3:end,:));
    
    b_hat = X\y;
    
    res = y - X*b_hat;
    
    C_1_sim = [b_hat(2:3,:)'];
    C_2_sim = [b_hat(4:5,:)'];
    
    I = eye(2,2);
    O = zeros(2,2);

    C_sim = [C_1_sim C_2_sim;
        I O];
    
    omega = [1 0; 0 1];
    
    temp = C_sim^0;
    irf_est(:,:,i+1,1) = temp(1:2,1:2)*omega;
    for t=1:10
        temp=C_sim^t;
        irf_est(:,:,i+1,t+1) = irf_est(:,:,i+1,t)+ temp(1:2,1:2)*omega;
    end 
end
   
s= 0;
prc = [5 50 95];
for m = 1:2
    for n = 1:2
        for i = 1:11 
            for k = [5 50 95]
                irf(m,n,i, find(prc==k)) =  prctile(irf_est(m,n,:,i),k);
            end
        end
    end
end

s= 0;
figure
for m = 1:2
    for n = 1:2
        s= s+1;
        for k = 1:3
            subplot(2,2,s);            
            q = squeeze(irf(m,n,:,k));
            pl = plot(q);
            if k ==2
                set(pl,'Color','k','LineStyle','-')
            else
                set(pl,'Color','k','LineStyle','--')
            end            

            hold on
        end
    end
end

%%
% With 3 lags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for i=1:K
    %starting condition, first observations
    data = zeros(2,2);
    %data(:,1) = zeros(2,1);
    %data(:,2) = zeros(2,1);
    
    %run the generation
    for j=3:250
        data(:,j) = C_1*data(:,j-1) + C_2*data(:,j-2) + randn(2,1);
    end
    
    data = data';
    X = [ones(T-4,1) diff(data(3:end-1,:)) diff(data(2:end-2,:)) diff(data(1:end-3,:))];
    y = diff(data(4:end,:));
    
    b_hat = X\y;
    res = y - X*b_hat;
    
    C_1_sim = [b_hat(2:3,:)'];
    C_2_sim = [b_hat(4:5,:)'];
    C_3_sim = [b_hat(6:7,:)'];

    I = eye(2,2);
    O = zeros(2,2);

    C_sim = [C_1_sim C_2_sim C_3_sim;
        I O O;
        O I O];
    
    omega = [1 0; 0 1];

    temp = C_sim^0;
    irf_est(:,:,i+1,1) = temp(1:2,1:2)*omega;
    for t=1:10
        temp=C_sim^t;
        irf_est(:,:,i+1,t+1) = irf_est(:,:,i+1,t)+ temp(1:2,1:2)*omega;
    end 
end
   
s= 0;

prc = [5 50 95];
for m = 1:2
    for n = 1:2
        for i = 1:11 
            for k = [5 50 95]
                irf(m,n,i, find(prc==k)) =  prctile(irf_est(m,n,:,i),k);
            end
        end
    end
end

s= 0;
figure
for m = 1:2
    for n = 1:2
        s= s+1;
        for k = 1:3
            subplot(2,2,s);            
            q = squeeze(irf(m,n,:,k));
            pl = plot(q);
            if k ==2
                set(pl,'Color','k','LineStyle','-')
            else
                set(pl,'Color','k','LineStyle','--')
            end            

            hold on
        end
    end
end
 

%%
% With 4 lags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for i=1:K
    %starting condition, first observations
    data = zeros(2,2);

    for j=3:250
        data(:,j) = C_1*data(:,j-1) + C_2*data(:,j-2) + randn(2,1);
    end
    
    data = data';
    X = [ones(T-5,1) diff(data(4:end-1,:)) diff(data(3:end-2,:)) diff(data(2:end-3,:)) diff(data(1:end-4,:))];
    y = diff(data(5:end,:));
    
    b_hat = X\y;
    res = y - X*b_hat;
    
    C_1_sim = [b_hat(2:3,:)'];
    C_2_sim = [b_hat(4:5,:)'];
    C_3_sim = [b_hat(6:7,:)'];
    C_4_sim = [b_hat(8:9,:)'];

    I = eye(2,2);
    O = zeros(2,2);

    C_sim = [C_1_sim C_2_sim C_3_sim C_4_sim;
        I O O O;
        O I O O;
        O O I O];
    
    omega = [1 0; 0 1];

    temp = C_sim^0;
    irf_est(:,:,i+1,1) = temp(1:2,1:2)*omega;
    for t=1:10
        temp=C_sim^t;
        irf_est(:,:,i+1,t+1) = irf_est(:,:,i+1,t)+ temp(1:2,1:2)*omega;
    end 
end
   
s= 0;
prc = [5 50 95];
for m = 1:2
    for n = 1:2
        for i = 1:11 
            for k = [5 50 95]
                irf(m,n,i, find(prc==k)) =  prctile(irf_est(m,n,:,i),k);
            end
        end
    end
end

s= 0;
figure
for m = 1:2
    for n = 1:2
        s= s+1;
        for k = 1:3
            subplot(2,2,s);            
            q = squeeze(irf(m,n,:,k));
            pl = plot(q);
            if k ==2
                set(pl,'Color','k','LineStyle','-')
            else
                set(pl,'Color','k','LineStyle','--')
            end            
            
            hold on
        end
    end
end


%% 
% Question 5.d
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
K=1000;
irf_est = [];
for o=1:K
    %starting condition, first observations
    data = zeros(2,2);

    for j=3:250
        data(:,j) = C_1*data(:,j-1) + C_2*data(:,j-2) + randn(2,1);
    end
    
    data = data';
    data_1 = data(:,1);
    data_2 = data(:,2);
    t = size(data_1,1);
    
    % We perform the auxiliary regressions
    % First, in differences
    % With the first variable
    X = [ones(t-2,1) diff(data_1(1:end-1)) diff(data_2(1:end-1))];
    y = diff(data_1(2:end));
    
    b_hat_d1 = X\y;
    u1 = y - X*b_hat_d1;
    
    % With the second variable
    X = [ones(t-2,1) diff(data_1(1:end-1)) diff(data_2(1:end-1))];
    y = diff(data_2(2:end));
    
    b_hat_d2 = X\y;
    u2 = y - X*b_hat_d2;
    
    % Then, in levels
    % With the first variable
    X = [ones(t-2,1) diff(data_1(1:end-1)) diff(data_2(1:end-1))];
    y = data_1(2:end-1);
    
    b_hat_l1 = X\y;
    v1 = y - X*b_hat_l1;
    
    % With the second variable
    X = [ones(t-2,1) diff(data_1(1:end-1)) diff(data_2(1:end-1))];
    y = data_2(2:end-1);
    
    b_hat_l2 = X\y;
    v2 = y - X*b_hat_l2;
    
    % We stack the residuals
    u = [u1, u2];
    v = [v1, v2];
    
    % Variance-Covariance matrices of the residuals
    sigma_vv = v'*v/t;
    sigma_uu = u'*u/t;
    sigma_vu = v'*u/t;
    sigma_uv = sigma_vu';
    
    M = inv(sigma_vv)*sigma_vu*inv(sigma_uu)*sigma_uv;
    
    eigenvl = eig(M);
    [a, d] = eig(M);
    
    % We normalize the eigenvectors as suggested by Johanssen
    norm_eigenv = zeros(length(a(:,1)));
    for i=1:length(a(:,1))
        norm_eigenv(:,i) = a(:,i)./sqrt(a(:,i)'*sigma_vv*a(:,i));
    end
    
    % k or h is the dimension of the cointegrating space
    k = 1;
    
    % we take the eigenvectors associated with the k biggest eigenvalues
    [x, ind] = max(eigenvl);
    big_eig = a(:,ind);
    
    % We compute xi_0, we get something that is n by n
    xi_0 = sigma_uv*big_eig*big_eig';
    
    % We get Pi_1 and Sigma_1
    pi_1 = [b_hat_d1(2:3)' ; b_hat_d2(2:3)'];
    theta_1 = [b_hat_l1(2:3)' ; b_hat_l2(2:3)'];
    
    %b_hat_d = [b_hat_d1(2,1); b_hat_d2(2,1)];
    %b_hat_l = [b_hat_l1(2,1); b_hat_l2(2,1)];
    
    xi_1 = pi_1 - xi_0*theta_1;    

    C_1_sim = xi_0 + xi_1 + load_3;
    C_2_sim = -xi_1;
    
    I = eye(2,2);
    O = zeros(2,2);
    
    C = [C_1_sim C_2_sim;
        I O];
    
    omega = [1 0; 0 1];
    
    for t=0:10
        temp=C^t;
        irf_est(:,:,o,t+1) = temp(1:2,1:2)*omega;
    end
end


s= 0;
prc = [5 50 95];
for m = 1:2
    for n = 1:2
        for i = 1:11 
            for k = [5 50 95]
                irf(m,n,i, find(prc==k)) =  prctile(irf_est(m,n,:,i),k);
            end
        end
    end
end

s= 0;
figure
for m = 1:2
    for n = 1:2
        s= s+1;
        for k = 1:3
            subplot(2,2,s);            
            q = squeeze(irf(m,n,:,k));
            pl = plot(q);
            if k ==2
                set(pl,'Color','k','LineStyle','-')
            else
                set(pl,'Color','k','LineStyle','--')
            end            
            
            hold on
        end
    end
end

