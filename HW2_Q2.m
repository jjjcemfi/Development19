%% Preliminaries

% Clearing command line and variables
clc
clear

% Setting random number seed
rng(12323)


%% Settings

% Parameter Settings
n = 1000;
maxAge = 40;
sigmaSqU = 0.2;
sigmaSqEps = 0.2;
beta = 0.99^(1/12);
nu = 1;
degreesOfSeasonality = 1:3;

% Calibration of kappa
consOutputRatio = 0.5;
theta = 0.66;
hBick = 28.5 *30/7;
kappa = theta/(consOutputRatio * hBick^(1/nu+1));

% Table 1: Deterministic seasonal component, g(m)
g = log([ ...
    0.863,	0.727,	0.932;
    0.691,	0.381,	0.845;
    1.151,	1.303,	1.076;
    1.140,	1.280,	1.070;
    1.094,	1.188,	1.047;
    1.060,	1.119,	1.030;
    1.037,	1.073,	1.018;
    1.037,	1.073,	1.018;
    1.037,	1.073,	1.018;
    1.002,	1.004,	1.001;
    0.968,	0.935,	0.984;
    0.921,	0.843,	0.961;
    ]);

 % Table 2: Stochastic seasonal component, sigmaSqM
sigmaSqM = [ ...
     0.085, 0.171, 0.043,;
     0.068, 0.137, 0.034;
     0.290, 0.580, 0.145;
     0.283, 0.567, 0.142;
     0.273, 0.546, 0.137;
     0.273, 0.546, 0.137;
     0.239, 0.478, 0.119;
     0.205, 0.410, 0.102;
     0.188, 0.376, 0.094;
     0.188, 0.376, 0.094;
     0.171, 0.341, 0.085;
     0.137, 0.273, 0.068;
     ];


%% Parts (a), (b) and (c)
% Note that shocks are perfectly correlated. I simply use the same shocks
% for both variables but change the sign. 

% Intialize results matrix
gRes = NaN(1000,3,2,2,size(degreesOfSeasonality,2));

% Draw types of individuals
z = exp(-sigmaSqU/2) * exp(sqrt(sigmaSqU)*randn(n,1));     % For consumption
zLab = exp(-sigmaSqU/2) * exp(sqrt(sigmaSqU)*randn(n,1));  % For labor (independent from consumption)

% Draw shocks for the individuals over the life cycle (for comparison
% purposes these are the same for different degrees of seasonality)
epsilon = repelem(exp(sqrt(sigmaSqEps)*randn(n,maxAge)),1,12);
epsilonTilde = exp(-sigmaSqEps/2) * epsilon; % For consumption

% Draw standard normal shocks for the individual seasonal component
logEpsMStandard = randn(n,maxAge*12);               % For consumption

% Draw the shocks for the individual over the lifecycle for labor
epsilonLab = repelem(exp(sqrt(sigmaSqEps)*randn(n,maxAge)),1,12);

for zz = 1:2 % Positively or negatively correlated consumption and labor supply
    
    % Set if labor supply and consumption are positively correlated
    corrSign = (zz==1)*1 + (zz==2)*(-1);
    
    % Compute shocks for the individual seasonal component
    logEpsMStandardLab = logEpsMStandard * corrSign; % For labor (perfectly correlated with consumption)

    for yy = 1:2 % Non-seasonal stochastic components are correlated
        
        % Compute the shocks for the individual over the lifecycle for labor
        if yy == 1 % Not correlated, i.e. redraw the shocks for labor
            epsilonTildeLab = exp(-sigmaSqEps/2) * epsilonLab; % For labor
        else % Perfectly correlated (possitively or negatively)
            epsilonTildeLab = exp(-sigmaSqEps/2) * (epsilon.^corrSign); % For labor
        end

        for xx = 1:size(degreesOfSeasonality,2) 

            % Set current degree of seasonality
            degOfSeasonality = degreesOfSeasonality(xx); % 1: Middle, 2: High, 3: Low

            % Compute the individual seasonal component
            sigm = repmat(sigmaSqM(:,degOfSeasonality)',n,maxAge);
            epsilonTildeSeason = exp(-sigm/2) .* exp(sqrt(sigm).*logEpsMStandard);
            epsilonTildeSeasonLab = exp(-sigm/2) .* exp(sqrt(sigm).*logEpsMStandardLab);

            % Get the deterministic seasonal components per month and individual
            gm = repmat(g(:,degOfSeasonality)',n,maxAge);

            % Compute the consumption for all individuals
            cons = repmat(z,1,maxAge*12) .* exp(gm) .* epsilonTildeSeason .* epsilonTilde;
            consNoSeason = repmat(z,1,maxAge*12) .* epsilonTilde;

            % Compute the labor Supply for all individuals
            laborSupply = repmat(zLab,1,maxAge*12) .* exp(corrSign*gm) .* epsilonTildeSeasonLab .* epsilonTildeLab;
            laborSupplyNoSeason = repmat(zLab,1,maxAge*12) .* epsilonTildeLab;
            
            % Rescale the labor supply
            laborSupply = laborSupply * hBick;
            laborSupplyNoSeason = laborSupplyNoSeason * hBick;

            % Compute utility per periods for all individuals
            util = log(cons) - kappa * laborSupply.^(1+1/nu)/(1+1/nu);
            utilNoSeason = log(consNoSeason) - kappa * laborSupplyNoSeason.^(1+1/nu)/(1+1/nu);
            utilNoConsSeason = log(consNoSeason) - kappa * laborSupply.^(1+1/nu)/(1+1/nu);

            % Compute discount factors for each period and all individuals
            discountFactors = repmat(beta.^(12+(0:maxAge*12-1)),n,1);

            % Compute lifetime utility
            W = sum(discountFactors.*util,2);
            WNoSeason = sum(discountFactors.*utilNoSeason,2);
            WNoConsSeason = sum(discountFactors.*utilNoConsSeason,2);

            % Compute the welfare gains
            discSum = sum(discountFactors,2);
            gc = exp((WNoConsSeason-W)./discSum)-1;
            gl = exp((WNoSeason-WNoConsSeason)./discSum)-1;
            gtot = exp((WNoSeason-W)./discSum)-1;
            
            % Store the results
            gRes(:,:,zz,yy,xx) = [gc gl gtot];

        end
    
    end
end
%%
for zz = 1:2 % Positively or negatively correlated consumption and labor supply
    
    for yy = 1:2 % Non-seasonal stochastic components are correlated
        
        posNegString = {'pos','neg'};
        nonseasString = {'','_nonseasriskcorr'};
        FID = fopen(['tab3_corr_' posNegString{zz}  nonseasString{yy} '.tex'], 'w');
            
        for qq = 1:size(gRes,2)
            
            gMedianMid = median(gRes(:,qq,zz,yy,1));
            gMedianHigh = median(gRes(:,qq,zz,yy,2));
            gMedianLow = median(gRes(:,qq,zz,yy,3));
            lbls = {'Consumption Contribution','Labor Contribution','Total'};
            fprintf(FID, '%s & %2.4f & %2.4f & %2.4f \\\\ \n', lbls{qq},gMedianLow,gMedianMid,gMedianHigh);
            
        end
        
        fclose(FID);
        
    end
    
end