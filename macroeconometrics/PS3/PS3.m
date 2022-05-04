    %------------------------------------------------------
    
    % 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    
    %--------------------------------------------------------
    
    clear
    USdata = xlsread("data_ps3.xlsx", "monetary_shock");
    
    % VAR (4) equation by equation
    %importing
    
    gdp = USdata(:,1);
    price = USdata(:,2);
    ffr= USdata(:,3);
    
    
    % picking our regressand
    gdp_y = gdp(5:end);
    price_y = price(5:end);
    ffr_y = ffr(5:end);
    
    % storing our regressors 
    X = [ones(size(gdp_y)) gdp(4:end-1) gdp(3:end-2) gdp(2:end-3) gdp(1:end-4) price(4:end-1) price(3:end-2) price(2:end-3) price(1:end-4) ffr(4:end-1) ffr(3:end-2) ffr(2:end-3) ffr(1:end-4)];
    
    % obtaining the coefficient of the first regression
    b_hat_gdp(:) = X\gdp_y;
    res_gdp = gdp_y - X*b_hat_gdp(:);
    
    % obtaining the coefficients of the second regression
    b_hat_price(:) = X\price_y;
    res_price = price_y - X*b_hat_price(:);
    
    %obtaining the coefficient of the third regression
    b_hat_ffr(:) = X\ffr_y;
    res_FFR = ffr_y - X*b_hat_ffr(:);
    
    res = [res_gdp res_price res_FFR];
    
    
    % companion form: check page 48 lecture notes 
    C_0 = [b_hat_gdp(1) b_hat_price(1) b_hat_ffr(1)]';
    C_1 = [b_hat_gdp(2) b_hat_gdp(6) b_hat_gdp(10); b_hat_price(2) b_hat_price(6) b_hat_price(10); b_hat_ffr(2) b_hat_ffr(6) b_hat_ffr(10)];
    C_2 = [b_hat_gdp(3) b_hat_gdp(7) b_hat_gdp(11); b_hat_price(3) b_hat_price(7) b_hat_price(11); b_hat_ffr(3) b_hat_ffr(7) b_hat_ffr(11)];
    C_3 = [b_hat_gdp(4) b_hat_gdp(8) b_hat_gdp(12); b_hat_price(4) b_hat_price(8) b_hat_price(12); b_hat_ffr(4) b_hat_ffr(8) b_hat_ffr(12)];
    C_4 = [b_hat_gdp(5) b_hat_gdp(9) b_hat_gdp(13); b_hat_price(5) b_hat_price(9) b_hat_price(13); b_hat_ffr(5) b_hat_ffr(9) b_hat_ffr(13)];
    I = eye(3,3);
    O = zeros(3,3);
    
    C = [C_1 C_2 C_3 C_4;
        I O O O;
        O I O O;
        O O I O];
    
    omega = cov(res);
    
    ciao=chol(omega)';
    
    %% IMPULSE RESPONSES
    hor = 50;
    irf_est = [];
    for t=0:hor
        temp=C^t;
        irf_est(:,:,1,t+1) = temp(1:3,1:3)*ciao;
    end
    
    s= 0;
    hold off
    for m = 1:3
        s= s+1;
        subplot(3,1,s)
        q = squeeze(irf_est(m,3,1,:));
        plot(q)
        hold on
            
    end
    
    
    %% FEVD - we follow the passages as shown at p70 lecture notes
    fevd=[];
    
    % ciao is the cholesky factor G;% initialize the numerator
    num=ciao(:,3)*ciao(:,3)';
    
    %initialize the denominator
    den=omega;
    
    % Only looking at the third column as we want to look at the monetary shock
    for j=0:hor
        temp = C^j;
        num=num+temp(1:3,1:3)*ciao(:,3)*ciao(:,3)'*temp(1:3,1:3)';
        den=den+temp(1:3,1:3)*omega*temp(1:3,1:3)';
        fevd(:,:,j+1)=diag(num)./diag(den);
    end
    
    s= 0;
    hold off
    for m = 1:3
        s= s+1;
        subplot(3,1,s)
        q = squeeze(fevd);
        pl = plot(q(m,:))
        set(pl,'Color','k')
        if m ==1
            title('Variance of gdp explained by MP shock')
        elseif m==2
            title('Variance of price explained by MP shock')
        else
            title('Variance of ffr explained by MP shock')
        end   
        hold on
            
    end
    hold off
    
    
    %% BOOTSTRAP
    K=1000;
    
    for i=1:5
        %starting condition, first observations of the dataset
        y_tilde = zeros(3,1);
        y_tilde(:,1)=USdata(1,:)';
        y_tilde(:,2)=USdata(2,:)';
        y_tilde(:,3)=USdata(3,:)';
        y_tilde(:,4)=USdata(4,:)';
        
        %extraction with replacement
        res_boot(:,:)= datasample(res,size(res,1)); 
        
        %then we run a for loop
        for j=5:size(res)+4
            y_tilde(:,j) = C_0 + C_1*y_tilde(:,j-1) + C_2*y_tilde(:,j-2) + C_3*y_tilde(:,j-3) + C_4*y_tilde(:,j-4) + res_boot(j-4,:)';
        end
    
        % Now that we have a new y_tilde, we estimate the VAR 
        
        % picking our regressand
        gdp = y_tilde(1,:)';
        price = y_tilde(2,:)';
        ffr = y_tilde(3,:)';
        
        gdp_y = y_tilde(1,5:end)';
        price_y = y_tilde(2,5:end)';
        ffr_y = y_tilde(3,5:end)';
    
        % storing our regressors 
        X = [ones(size(gdp_y)) gdp(4:end-1) gdp(3:end-2) gdp(2:end-3) gdp(1:end-4) price(4:end-1) price(3:end-2) price(2:end-3) price(1:end-4) ffr(4:end-1) ffr(3:end-2) ffr(2:end-3) ffr(1:end-4)];
        
        % obtaining the coefficient of the first regression
        b_hat_gdp(:) = X\gdp_y;
        res_gdp = gdp_y - X*b_hat_gdp(:);
    
        % obtaining the coefficients of the second regression
        b_hat_price(:) = X\price_y;
        res_price = price_y - X*b_hat_price(:);
    
        %obtaining the coefficient of the third regression
        b_hat_ffr(:) = X\ffr_y;
        res_FFR = ffr_y - X*b_hat_ffr(:);
    
        res_sim = [res_gdp res_price res_FFR];
    
    
        % companion form: check page 48 lecture notes 
        C_0_boot = [b_hat_gdp(1) b_hat_price(1) b_hat_ffr(1)]';
        C_1_boot = [b_hat_gdp(2) b_hat_gdp(6) b_hat_gdp(10); b_hat_price(2) b_hat_price(6) b_hat_price(10); b_hat_ffr(2) b_hat_ffr(6) b_hat_ffr(10)];
        C_2_boot = [b_hat_gdp(3) b_hat_gdp(7) b_hat_gdp(11); b_hat_price(3) b_hat_price(7) b_hat_price(11); b_hat_ffr(3) b_hat_ffr(7) b_hat_ffr(11)];
        C_3_boot = [b_hat_gdp(4) b_hat_gdp(8) b_hat_gdp(12); b_hat_price(4) b_hat_price(8) b_hat_price(12); b_hat_ffr(4) b_hat_ffr(8) b_hat_ffr(12)];
        C_4_boot = [b_hat_gdp(5) b_hat_gdp(9) b_hat_gdp(13); b_hat_price(5) b_hat_price(9) b_hat_price(13); b_hat_ffr(5) b_hat_ffr(9) b_hat_ffr(13)];
        I = eye(3,3);
        O = zeros(3,3);
    
        C_boot = [C_1_boot C_2_boot C_3_boot C_4_boot;
            I O O O;
            O I O O;
            O O I O];
    
        omega = cov(res_sim);
    
        ciao=chol(omega)';
    
        %% IMPULSE RESPONSES
        hor = 50;
        for t=0:hor
            temp=C_boot^t;
            irf_est(:,:,i+1,t+1) = temp(1:3,1:3)*ciao;
        end
    
      
    
    end
    
    s= 0;
    prc = [2.5 50 97.5];
    for m = 1:3
        for i = 1:50
            for k = [2.5 50 97.5]
                irf(m,3,i, find(prc==k)) =  prctile(irf_est(m,3,:,i),k);
            end
        end
        
    end
    
    hold off
    
    s= 0;
    for m = 1:3
        s= s+1;
        for t = 1:3
            subplot(3,1,s);            
            q = squeeze(irf(m,3,:,t));
            pl = plot(q)
            if m == 1
                title('MP shock on gdp')
            elseif m ==2
                title('MP shock on price')
            else
                title('MP shock on ffr')
            end
            if t == 2
                set(pl,'Color','k','LineStyle','-')
            else
                set(pl,'Color','k','LineStyle','--')
            end
            hold on
        end
    end
    
    %--------------------------------------------------------
    
    % 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
    
    %----------------------------------------------------------
    %% 
    
    clear
    Gali = xlsread("data_ps3.xlsx", "technology_shock");
    
    %% VAR (4) equation by equation
    %importing
    productivity = log(Gali(:,1));
    labor_per_capita = log(Gali(:,2));
    
    % differencing to obtain I(0)
    YL = diff(productivity) - mean(diff(productivity));
    hours = diff(labor_per_capita);
    
    % picking our regressand
    YL_y = YL(5:end);
    hours_y = hours(5:end);
    
    % storing our regressors (no need to insert the constant)
    X = [YL(4:end-1) YL(3:end-2) YL(2:end-3) YL(1:end-4) hours(4:end-1) hours(3:end-2) hours(2:end-3) hours(1:end-4)];
    
    % obtaining the coefficient of the first regression
    b_hat_YL(:) = X\YL_y;
    res_YL = YL_y - X*b_hat_YL(:);
    
    % obtaining the coefficients of the second regression
    b_hat_hours(:) = X\hours_y;
    res_hours = hours_y - X*b_hat_hours(:);
    
    % companion form: check page 48 lecture notes 
    C_1 = [b_hat_YL(1) b_hat_YL(5); b_hat_hours(1) b_hat_hours(5)];
    C_2 = [b_hat_YL(2) b_hat_YL(6); b_hat_hours(2) b_hat_hours(6)];
    C_3 = [b_hat_YL(3) b_hat_YL(7); b_hat_hours(3) b_hat_hours(7)];
    C_4 = [b_hat_YL(4) b_hat_YL(8); b_hat_hours(4) b_hat_hours(8)];
    
    I = eye(2,2);
    O = zeros(2,2);
    
    C = [C_1 C_2 C_3 C_4;
        I O O O;
        O I O O;
        O O I O];
    
    % restriction à la Blanchard and Quah (lecture notes p73) 
    I_c = eye(size(C,1));
    D = inv(I_c - C);
    D_1 = D(1:2,1:2);
    omega = cov(res_YL,res_hours);
    omega2 = D_1*omega*D_1';
    S = chol(omega2)';
    K = inv(D_1)*S;
    res_errors = [res_YL,res_hours];
    struct = inv(K)*res_errors';
    
    % obtaining the impulse responses  
       
    for imp = 0:20
        Z1 = zeros(2,2);
        for l = 0:imp
            C_l = C^l;
            Z_l = C_l(1:2,1:2);
            Z1 = Z1 + Z_l;
        end
        irf_est(:,:,1,imp+1)=Z1*K;
    end
    
    s= 0;
    hold off
    for m = 1:2
        for n = 1:2
            s= s+1;
            subplot(2,2,s)
            q = squeeze(irf_est(m,n,1,:));
            plot(q)
            hold on
            
        end
    end
    
    %% boostrap: I need all the elements according to companion form
    % we iterate the generating process of Y according to the companion form
    % setting and with matrix C obtained from the 'real' VAR
    
    Y = [YL(4); hours(4); YL(3); hours(3); YL(2); hours(2); YL(1); hours(1)];
    sim = 1000;
    sim_errors = zeros(size(res_errors));
    for i = 2:sim
        Y = [YL(4); hours(4); YL(3); hours(3); YL(2); hours(2); YL(1); hours(1)];
        sim_errors(:,:,i) = datasample(res_errors,size(res_errors,1));
        for j = 1:size(res_errors,1)
            res_iter = sim_errors(:,:,i);
            res_step = [res_iter(j,:) 0 0 0 0 0 0];
            Y(:,j+1) = C*Y(:,j) + res_step';
        end
        
        %here comes the long part of the var estimation
        YL_y = Y(1,2:end)';
        hours_y = Y(2, 2:end)';
        X_yl = [Y(1,1:end-1)' Y(3,1:end-1)' Y(5,1:end-1)' Y(7,1:end-1)'];
        X_hours = [Y(2,1:end-1)' Y(4,1:end-1)' Y(6,1:end-1)' Y(8,1:end-1)'];
        X = [X_yl X_hours];
        iXX = inv(X'*X);
    
        %YL
        b_hat_YL(:) = X\YL_y;
        res_YL = YL_y - X*b_hat_YL(:);
    
        %y
        b_hat_hours(:) = X\hours_y;
        res_hours = hours_y - X*b_hat_hours(:);
    
        %companion form
        C_1 = [b_hat_YL(1) b_hat_YL(5); b_hat_hours(1) b_hat_hours(5)];
        C_2 = [b_hat_YL(2) b_hat_YL(6); b_hat_hours(2) b_hat_hours(6)];
        C_3 = [b_hat_YL(3) b_hat_YL(7); b_hat_hours(3) b_hat_hours(7)];
        C_4 = [b_hat_YL(4) b_hat_YL(8); b_hat_hours(4) b_hat_hours(8)];
    
        I = eye(2,2);
        O = zeros(2,2);
    
        C_sim = [C_1 C_2 C_3 C_4;
            I O O O;
            O I O O;
            O O I O];
    
        %following lecture notes p73
        I_c = eye(size(C_sim,1));
        D = inv(I_c - C_sim);
        D_1 = D(1:2,1:2);
        omega = cov(res_YL,res_hours);
        omega2 = D_1*omega*D_1';
        S = chol(omega2)';
        K = inv(D_1)*S;
        errors_sim = [res_YL,res_hours];
        struct = inv(K)*errors_sim';
    
       
        for imp = 0:20
            Z1 = zeros(2,2);
            for l = 0:imp
                C_sim_l = C_sim^l;
                Z_l = C_sim_l(1:2,1:2);
                Z1 = Z1 + Z_l;
            end
        irf_est(:,:,i,imp+1)=Z1*K;
        end
    end
    
    
    s= 0;
    prc = [2.5 50 97.5];
    for m = 1:2
        for n = 1:2
            for i = 1:12
                for k = [2.5 50 97.5]
                    irf(m,n,i, find(prc==k)) =  prctile(irf_est(m,n,:,i),k);
                end
            end
        end
    end
    
    irf = irf*100;
    
    hold off
    hold on
    s= 0;
    for m = 1:2
        for n = 1:2
            s= s+1;
            
            for t = 1:3
                if s < 3
                    subplot(3,2,s);            
                    q = squeeze(irf(m,n,:,t));
                    pl = plot(q)
                    if n == 1
                        title('Tech shock on productivity')
                    else
                        title('Non-Tech on productivity')
                    end
                    if t ==2
                        set(pl,'Color','k','LineStyle','-')
                    else
                        set(pl,'Color','k','LineStyle','--')
                    end
                    hold on
                else
                    subplot(3,2,s+2);            
                    q = squeeze(irf(m,n,:,t));
                    pl = plot(q)
                    if n == 1
                        title('Tech shock on hours')
                    else
                        title('Non-Tech on hours')
                    end
                    if t ==2
                        set(pl,'Color','k','LineStyle','-')
                    else
                        set(pl,'Color','k','LineStyle','--')   
                     
                    end
                    hold on
            end
                 
        end
    end
    end
    
    s=1;
    for i = 1:2
        subplot(3,2,i+2)
        for l = 1:3
            q = sum(squeeze(irf(:,i,:,l)));
            pl = plot(q)
            if i == 1
                title('Tech shock on GDP')
            else
                title('Non-tech on GDP')
            
            end
            if l ==2
                set(pl,'Color','k','LineStyle','-')
            else
                set(pl,'Color','k','LineStyle','--')   
    
            end
            hold on
        end
    end
        