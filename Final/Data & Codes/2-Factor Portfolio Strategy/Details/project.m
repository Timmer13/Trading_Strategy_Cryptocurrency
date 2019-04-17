% HW 2
% load MSCI indices returns from excel file
% you will need to change path
filename = 'C:\Users\yx2443\Desktop\5010\project_momentum.xlsx';

[data,txt] = xlsread(filename, 'equity index returns'); 
returns.header = txt(1,2:end);
returns.dates = txt(2:end,1);
returns.data = data;

clear data txt
windowLength = 36; % length of rolling window for COV estimation
% initialize COV data structure as 3-dim matrix, 3rd dimension is date

COV.header = returns.header; 
K = numel(COV.header);
COV.dates = returns.dates(windowLength:end); 
L = length(COV.dates); 
COV.data = NaN(K,K,L);

% calculate COV matrix for each month t 
for t = windowLength:length(returns.dates)

    RollingWindow = returns.data(t-windowLength+1:t,:);
    [~, ExpCovariance] = ewstats(RollingWindow,1,windowLength); 
    COV.data(:,:,t+1-windowLength) = ExpCovariance*365;

end


% Momemtum Tab
matrix1 = xlsread(filename, 'Sheet1'); 
momentum = zeros(size(matrix1,1),1);

for i = 1:size(matrix1,1)
   momentum(i) = matrix1(i,:) *  COV.data(:,:,i) * matrix1(i,:).';

end

xlswrite('momentum_risk.xls', momentum)

% Value Tab
matrix2 = xlsread(filename, 'Sheet2'); 
value = zeros(size(matrix2,1),1);

for i = 1:size(matrix2,1)
   value(i) = matrix2(i,:) *  COV.data(:,:,i) * matrix2(i,:).';

end

xlswrite('value_risk.xls', value)

% Combine Tab
matrix3 = xlsread(filename, 'Sheet3'); 
combine = zeros(size(matrix3,1),1);

for i = 1:size(matrix3,1)
   combine(i) = matrix3(i,:) *  COV.data(:,:,i) * matrix3(i,:).';

end

xlswrite('combine_risk.xls', combine)

% % Beta
% matrix4 = matrix1(146:459,:);
% cov = zeros(314,1);
% beta = zeros(314,1);
% 
% for i = 1:314
%    cov(i) = matrix2(i,:) *  COV.data(:,:,i+145) * matrix4(i,:).';
%    beta(i) = cov(i) / momentum(i+145);
% end
% 
% xlswrite('beta.xls', beta)