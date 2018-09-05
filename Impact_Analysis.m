% File to analyse GHG, Energy, and Material Impacts of US consumption
% using USEEIO and capital flow matrices
% Sep 5 2018
% Peter Berrill

% Required input files:
% USEEIO_v7_*year*_use.mat where year = 2013 or 2007

% Original functions called by this script:
% sector_reduce

%% Load data
clc; clear;
close all

question1 = inputdlg('Do you want to view impacts for 2007 or 2013??  ');
% question2 = inputdlg('Do you want to make the calculation based on ownership or use of capital goods? Type use or own');

year=str2double(question1{:});
switch year
    case 2007
        load(['USEEIO_2007.mat'])
      
    case 2013
        load(['USEEIO_2013.mat'])
end

exp=[1, 9, 13, 17]; % all expenditure categories
gov_exp=[9,13,17]; % public expenditure
priv_inv=2:5; % private investment categories
fed_gov_inv=[10:12, 14:16]; % federal government investment categories
sl_gov_inv=18:20; % state and local government investment categories
gov_inv=[fed_gov_inv sl_gov_inv]; % government investmenst categories
exp_inv=[1:5, 9:20]; % expenditure and investment, excluding trade and changes in stocks
invest=[2:5, 10:12, 14:16, 18:20]; % all investment categories of final demand

y_exp=sum(IO.Y(:,exp),2); % personal and government expenditure component of final demand 
y_exp_inv=sum(IO.Y(:,exp_inv),2); % all expenditure and investment spending, excl. trade and changes in stocks
y_gov_exp=sum(IO.Y(:,gov_exp),2); % government expenditure
y_invest=sum(IO.Y(:,invest),2); % total investment spending
y_priv_inv=sum(IO.Y(:,priv_inv),2); % private investment spending
y_gov_inv=sum(IO.Y(:,gov_inv),2); % government investment spending
y_fedg_inv=sum(IO.Y(:,fed_gov_inv),2); % federage government investment spending
y_slg_inv=sum(IO.Y(:,sl_gov_inv),2); % state and local government investment spending
y_hh=IO.Y(:,1); % household consumption
%% Total requirement matrices for original, capital, augmented and combined tables
Atot=IO.A+IO.Ak; % Total direct requirements, including capital 
I=eye(size(Atot,1)); % Identity matrix
L=inv(I-IO.A); % Leontief inverse of in-year production
Lt=inv(I-Atot); % Leontief inverse of total production
%% GHG intensities
GHG_int_pr=(IO.C(4,:)*IO.S)'; % Carbon intensity of production
GHG_int_con=(IO.C(4,:)*IO.S*L)'; % Carbon intensity of consumption, no capital
GHG_int_con_purch=(IO.C(4,:)*IO.S*L*IO.T)';
GHG_int_con_purch_392=[GHG_int_con_purch(1:274); zeros(3,1); GHG_int_con_purch(275:389)];
GHG_int_Atot=(IO.C(4,:)*IO.S*Lt)'; % Carbon inensity of consumption, incl. capital
GHG_int_Atot_purch=(IO.C(4,:)*IO.S*Lt*IO.T)';
GHG_int_Atot_purch_392=[GHG_int_Atot_purch(1:274); zeros(3,1); GHG_int_Atot_purch(275:389)];

% Matrices of GHG intensities. Column = production, row = consumption.
% Column sums equal consumption intensities.
% Each row element corresponds to the contribution of that production 
% sector to the overall intensity. 
M_GHG0=diag(GHG_int_pr)*L;
M_GHGt=diag(GHG_int_pr)*Lt;
M_GHGdiff=M_GHGt-M_GHG0; % increase in carbon intensity due to capital
% Matrices of total emissions from consumption expenditure
D_GHG0=M_GHG0*diag(y_exp); 
D_GHGt=M_GHGt*diag(y_exp);
D_GHGdiff=D_GHGt-D_GHG0;
%% load detailed and summary level labels
load('General_meta.mat')
%% private and public expenditure and investment CF in-year impacts 
d_pr_ex=(IO.C(4,:)*IO.S*L*diag(y_hh))'; % CF from private (household) expenditures
d_gov_ex=(IO.C(4,:)*IO.S*L*diag(y_gov_exp))'; % CF from government expenditures
d_pr_inv=(IO.C(4,:)*IO.S*L*diag(y_priv_inv))'; % CF from private investment
d_fedg_inv=(IO.C(4,:)*IO.S*L*diag(y_fedg_inv))'; % CF from federal government investment
d_slg_inv=(IO.C(4,:)*IO.S*L*diag(y_slg_inv))';  % CF from state and local government investment
%% concatenate total expenditure results
sum_all=[d_pr_ex d_gov_ex d_pr_inv d_fedg_inv+d_slg_inv]; % CF from all expenditure and investment activity
sum_all(:,5)=sum(sum_all,2); % sum of exp. and inv. CF by consumption sector
sum_all(:,6)=1:size(sum_all,1); % commodity sector number
sum_all_sort=flipud(sortrows(sum_all,5)); % sort CF by largest to smallest consumption sector
labsexCF=Z(sum_all_sort(:,6),3); % commodity lablels in order of largest CF
%% CF of expenditure and capital consumption including capital
d_pr_con_t=(IO.C(4,:)*IO.S*Lt*diag(y_hh))'; % total CF of private expenditures, including capital inputs
diff_pr=d_pr_con_t-d_pr_ex; % increase in private CF from consideration of capital inputs
d_gov_con_t=(IO.C(4,:)*IO.S*Lt*diag(y_gov_exp))'; % total CF of government expenditures, including capital inputs 
diff_gov=d_gov_con_t-d_gov_ex; % increase in government CF from consideration of capital inputs
%% concatenate expenditure and capital consumption results
sum_con=[d_pr_ex d_gov_ex diff_pr diff_gov]; % consumption based total CF, broken into in-year, and capital contributions
sum_con(:,5)=sum(sum_con,2); % sum of total CF by consumption sector
sum_con(:,6)=1:size(sum_con,1); % commodity number
sum_con_sort=flipud(sortrows(sum_con,5)); % sort CF by largest to smallest consumption sector
labsconCF=Z(sum_con_sort(:,6),3); % commodity lablels in order of largest total CF
%% Summary sector results expenditure/investment CF results
order=[4 3 2 1]; % needed for plotting figure legend
labels={'Pri. Consumption', 'Gov. Consumption', 'Pri. Investment','Gov. Investment'}; % legend labels
sec_sum=[string(Z(:,1)) string(td) sum_all]; % Z contains modified sector labels, td contains 2-digit sector codes
[sum_exp_inv, ZLab25] = sector_reduce(string(sector_label),sec_sum,sum_all); % aggregate detailed level exp. inv. impacts to summary level
sum_exp_inv_Mt=1e-9*sum_exp_inv; % convert impacts from kg to Mega tons (metric), Mt
f1=figure('pos',[1 50 720 540]);
q=bar(sum_exp_inv_Mt,'stack');
switch year
    case 2007
        title(['A. Carbon Footprint of consumption & capital investment, ' num2str(year)])
    case 2013
        title(['B. Carbon Footprint of consumption & capital investment, ' num2str(year)]) 
        u=legend(q(order),labels{order});
        set(u,'Fontsize',8.3)
end
cm=[.75 0.2 0; 1 0.3 0.3; 0 0 .6; 0 0.4 1];
colormap(cm);
ylabel('Mt CO2eq')
ylim([0 1600]); xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',11.5);
set(gca,'TitleFontSizeMultiplier',1.06);
%% Summary sector results consumption CF results
order=[4 3 2 1];
labels={'Pri. Consumption (PC)', 'Gov. Consumption (GC)', 'Capital inputs to PC','Capital inputs to GC'};
sec_sum_con=[string(Z(:,1)) string(td) sum_con];
[sum_con_sec, ZLab25] = sector_reduce(string(sector_label),sec_sum_con,sum_con); % aggregate capital inclusive consumption impacts to summary level 
sum_con_sec_Mt=1e-9*sum_con_sec; % convert kg to MegaTon
f2=figure('pos',[1 50 720 540]);
q3=bar(sum_con_sec_Mt,'stack');
switch year
    case 2007
        title(['C. Carbon Footprint of consumption incl. capital inputs, ' num2str(year)])
    case 2013 
        title(['D. Carbon Footprint of consumption incl. capital inputs, ' num2str(year)])
        u=legend(q3(order),labels{order});
        set(u,'Fontsize',8.3)
end
colormap(cm);
ylabel('Mt CO2eq')
ylim([0 1600]); xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',11.5);
%% Highest detail level CF sectors
order=[4 3 2 1];
labels={'Pri. Consumption (PC)', 'Gov. Consumption (GC)', 'Capital inputs to PC','Capital inputs to GC'};
f5=figure('pos',[1 50 720 540]);
sum_con_sort_Mt=1e-9*sum_con_sort; % convert kg to Mt
q=bar(sum_con_sort_Mt(1:15,1:4),'stack');
switch year
    case 2007
        title(['A. Max Carbon Footprint of detailed consumption sectors, ' num2str(year)])
        order07=sum_con_sort(:,6);
    case 2013
        title(['B. Max Carbon Footprint of detailed consumption sectors, ' num2str(year)])
        u=legend(q(order),labels{order});
        set(u,'Fontsize',8.3)
        order13=sum_con_sort(:,6);
end
colormap(cm);
ylabel('Mt CO2eq')
xticklabels(labsconCF(1:15));
xtickangle(45)
ylim([0 1600])
set(gca,'FontSize',11.5);
%% Highest detail level CF sectors with combined residential sector
s=zeros(15,4); % preallocate matrix for detail CF results with combined res. sector
res=[2 4 5 13]; % Check on labsconCF. these should correspond to Electricity, Res. nat. gas, Housing, and Res. petroleum fuels.
scs = sum_con_sort(1:17,1:4); % highest 17 CF sectors
other = sum_con_sort(18:end,1:4); % all other CF sectors
sum_other=sum(other,1); % sum of other CF sectors
s(1,:)= sum(scs(res,:)); % sum of the residential related sectors
scs(res,:)=[]; % remove the individual residential related sectors
s(2:14,:)=scs; % non-residential top CF sectors
s(15,:)=sum_other; % all other CF sectors
s_Mt=1e-9*s; % convert from kg to Mt
l=labsconCF(1:17);
l(res)=[];
reslab=[{'Housing & home energy'};l;{'All other sectors'}]; % xtick labels
f6=figure('pos',[1 50 720 540]);
q2=bar(s_Mt,'stack');
title(['Detailed sectors with highest consumption based Carbon Footprint, ' num2str(year)])
colormap(cm);
u=legend(q2(order),labels{order});
ylabel('Mt CO2eq')
xticklabels(reslab);
xtickangle(45)
ylim([0 2000])
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% private and public expenditure and investment Material Footprint in-year impacts 
tot_mat_char=sum(IO.C(16:19,:),1); % single characterisation row for total material impacts
MF_pr_ex=(tot_mat_char*IO.S*L*diag(y_hh))'; % MF from private consumption expenditures
MF_gov_ex=(tot_mat_char*IO.S*L*diag(y_gov_exp))'; % MF from government expenditures
MF_pr_inv=(tot_mat_char*IO.S*L*diag(y_priv_inv))'; % MF from private investment
MF_fedg_inv=(tot_mat_char*IO.S*L*diag(y_fedg_inv))'; % MF from federal government investment
MF_slg_inv=(tot_mat_char*IO.S*L*diag(y_slg_inv))'; % MF from state/local govt investment
%% concatenate total expenditure MF results
MF_exp=[MF_pr_ex MF_gov_ex MF_pr_inv MF_fedg_inv+MF_slg_inv]; % consumption and investment total MF, excl. capital inputs
MF_exp(:,5)=sum(MF_exp,2); % total by commodity consumption/investment sector
MF_exp(:,6)=1:size(MF_exp,1); % commodity numbers
MF_exp_sort=flipud(sortrows(MF_exp,5)); % sort by largest consumption sectors
labsexMF=Z(MF_exp_sort(:,6),3); % commodity sector labels in order of largest to smallest MF
%% MF of expenditure and capital consumption including capital
MF_pr_con_t=(tot_mat_char*IO.S*Lt*diag(y_hh))'; % total MF of private consumption, incl. capital inputs
diff_MF_pr=MF_pr_con_t-MF_pr_ex; % increase in MF from capital inputs
MF_gov_con_t=(tot_mat_char*IO.S*Lt*diag(y_gov_exp))'; % total MF of government consumption expenditures, incl. capital inputs
diff_MF_gov=MF_gov_con_t-MF_gov_ex; % increase in government MF from capital inputs
%% concatenate expenditure and capital consumption MF results
MF_con=[MF_pr_ex MF_gov_ex diff_MF_pr diff_MF_gov]; % total consumption MF, incl capital
MF_con(:,5)=sum(MF_con,2); % sum by consumption commodidty sector
MF_con(:,6)=1:size(MF_con,1); % commodity number 
MF_con_sort=flipud(sortrows(MF_con,5)); % sort by largest MF commodity
labsconMF=Z(MF_con_sort(:,6),3); %  commodity sector labels in order of largest to smallest MF
%% Summary sector results expenditure/investment MF results
order=[4 3 2 1];
labels={'Pri. Consumption', 'Gov. Consumption', 'Pri. Investment','Gov. Investment'}; % legend labels
MF_sum_exp=[string(Z(:,1)) string(td) MF_exp];
[MF_exp_inv, ZLab25] = sector_reduce(string(sector_label),MF_sum_exp,MF_exp); % aggregate results to summary sector 
MF_exp_inv_Mt=1e-9*MF_exp_inv; % convert to megaTon
f3=figure('pos',[1 50 720 540]);
q=bar(MF_exp_inv_Mt,'stack');
switch year
    case 2007
        title(['E. Material Footprint of consumption & capital investment, ' num2str(year)])
    case 2013
        title(['F. Material Footprint of consumption & capital investment, ' num2str(year)])
        u=legend(q(order),labels{order});
        set(u,'Fontsize',8.3)
end
colormap(cm);
ylabel('Mt material')
ylim([0 2200]); xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
%% Summary sector results consumption MF results
order=[4 3 2 1];
labels={'Pri. Consumption (PC)', 'Gov. Consumption (GC)', 'Capital inputs to PC','Capital inputs to GC'};
MF_sum_con=[string(Z(:,1)) string(td) MF_con];
[MF_con_sum, ZLab25] = sector_reduce(string(sector_label),MF_sum_con,MF_con); % aggregate results to summary sector 
MF_con_sum_Mt=1e-9*MF_con_sum; % convert to megaTon
f4=figure('pos',[1 50 720 540]);
q5=bar(MF_con_sum_Mt,'stack');
switch year
    case 2007
        title(['G. Material Footprint of consumption incl. capital inputs, ' num2str(year)])
    case 2013
        title(['H. Material Footprint of consumption incl. capital inputs, ' num2str(year)])
        u=legend(q5(order),labels{order});
        set(u,'Fontsize',8.3)
end
colormap(cm);
ylabel('Mt material')
ylim([0 2200]); xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
%% Highest detail level MF consumption sectors
order=[4 3 2 1];
labels={'Pri. Consumption (PC)', 'Gov. Consumption (GC)', 'Capital inputs to PC','Capital inputs to GC'};
f9=figure('pos',[1 50 720 540]);
MF_con_sort_Mt=1e-9*MF_con_sort; % convert from kg to Mt
q=bar(MF_con_sort_Mt(1:15,1:4),'stack');
switch year
    case 2007
        title(['C. Max Material Footprint of detailed consumption sectors, ' num2str(year)])
        orderm07=MF_con_sort(:,6);
    case 2013
        title(['D. Max Material Footprint of detailed consumption sectors, ' num2str(year)])
        u=legend(q(order),labels{order});
        set(u,'Fontsize',8.3)
        orderm13=MF_con_sort(:,6);
end
colormap(cm);
ylabel('Mt material')
xticklabels(labsconMF(1:15));
xtickangle(45)
ylim([0 850])
set(gca,'FontSize',11.5);
%% Highest detail level MF exp and inv sectors
order=[4 3 2 1];
labels={'Pri. Consumption', 'Gov. Consumption', 'Pri. Investment','Gov. Investment'}; % legend labels
f10=figure('pos',[1 50 720 540]);
MF_exp_sort_Mt=1e-9*MF_exp_sort; % convert from kg to Mt
q=bar(MF_exp_sort_Mt(1:15,1:4),'stack');
title(['Detailed sectors with highest Material Footprint, expenditure and investment, ' num2str(year)])
colormap(cm);
u=legend(q(order),labels{order});
ylabel('Mt material')
xticklabels(labsexMF(1:15));
xtickangle(45)
% ylim([0 1600])
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% private and public expenditure and investment EF in-year impacts 
EF_pr_ex=(IO.C(11,:)*IO.S*L*diag(y_hh))'; % energy footprint from private consumption expenditures
EF_gov_ex=(IO.C(11,:)*IO.S*L*diag(y_gov_exp))'; % EF from government consumption expenditures
EF_pr_inv=(IO.C(11,:)*IO.S*L*diag(y_priv_inv))'; % EF from private investments
EF_fedg_inv=(IO.C(11,:)*IO.S*L*diag(y_fedg_inv))'; % EF from fed government investment
EF_slg_inv=(IO.C(11,:)*IO.S*L*diag(y_slg_inv))'; % EF from state/local govt investments
%% concatenate total expenditure EF results
EF_exp=[EF_pr_ex EF_gov_ex EF_pr_inv EF_fedg_inv+EF_slg_inv]; % total consumption and investment expenditure EF 
EF_exp(:,5)=sum(EF_exp,2); % sum by commodity type
EF_exp(:,6)=1:size(EF_exp,1); % commodity number
EF_exp_sort=flipud(sortrows(EF_exp,5)); % sort by largest EF commodities
labsexEF=Z(EF_exp_sort(:,6),3); % commodity labels in order of highest EF
%% Summary sector results expenditure/investment EF results
order=[4 3 2 1];
labels={'Pri. Consumption', 'Gov. Consumption', 'Pri. Investment','Gov. Investment'}; % legend labels
EF_sum_exp=[string(Z(:,1)) string(td) EF_exp];
[EF_exp_inv, ZLab25] = sector_reduce(string(sector_label),EF_sum_exp,EF_exp); % aggregate impacts to summary level
f7=figure('pos',[1 50 720 540]);
q=bar(1e-12*EF_exp_inv,'stack'); % convert MJ to EJ
switch year
    case 2007
        title(['I. Energy Footprint, consumption & capital investment, ' num2str(year)])
    case 2013
        title(['J. Energy Footprint, consumption & capital investment, ' num2str(year)])
        u=legend(q(order),labels{order});
        set(u,'Fontsize',8.3)
end
colormap(cm);

ylabel('EJ')
xlim([0 26]);
ylim([0 25]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
%% EF of expenditure and capital consumption including capital
EF_pr_con_t=(IO.C(11,:)*IO.S*Lt*diag(y_hh))'; % EF from private consumption, including capital inputs
diff_EF_pr=EF_pr_con_t-EF_pr_ex; % increase in private EF from capital inputs
EF_gov_con_t=(IO.C(11,:)*IO.S*Lt*diag(y_gov_exp))'; % EF from government consumption, including capital
diff_EF_gov=EF_gov_con_t-EF_gov_ex; % increase in government EF from capital inputs
%% concatenate expenditure and capital consumption EF results
EF_con=[EF_pr_ex EF_gov_ex diff_EF_pr diff_EF_gov]; % total consumption EF including capital
EF_con(:,5)=sum(EF_con,2); % sum by consumption commodity
EF_con(:,6)=1:size(EF_con,1); % commodity number
EF_con_sort=flipud(sortrows(EF_con,5)); % sort by largest EF commodities
labsconEF=Z(EF_con_sort(:,6),3); % labels in order of largest EF
%% Summary sector results consumption EF results
order=[4 3 2 1];
labels={'Pri. Consumption (PC)', 'Gov. Consumption (GC)', 'Capital inputs to PC','Capital inputs to GC'};
EF_sum_con=[string(Z(:,1)) string(td) EF_con];
[EF_con_sum, ZLab25] = sector_reduce(string(sector_label),EF_sum_con,EF_con); % aggregate impacts to summary level

f8=figure('pos',[1 50 720 540]);
q5=bar(1e-12*EF_con_sum,'stack'); % convert MJ to EJ
switch year
    case 2007
        title(['K. Energy Footprint of consumption incl. capital inputs, ' num2str(year)])
    case 2013
        title(['L. Energy Footprint of consumption incl. capital inputs, ' num2str(year)])
        u=legend(q5(order),labels{order});
        set(u,'Fontsize',8.3)
end
colormap(cm);
ylabel('EJ')
xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
%% Highest detail level EF consumption sectors
order=[4 3 2 1];
labels={'Pri. Consumption (PC)', 'Gov. Consumption (GC)', 'Capital inputs to PC','Capital inputs to GC'};
f11=figure('pos',[1 50 720 540]);
q=bar(1e-12*EF_con_sort(1:15,1:4),'stack'); % convert MJ to EJ
switch year
    case 2007
        title(['E. Max Energy Footprint of detailed consumption sectors, ' num2str(year)])
        ordere07=EF_con_sort(:,6);
    case 2013
        title(['F. Max Energy Footprint of detailed consumption sectors, ' num2str(year)])
        u=legend(q(order),labels{order});
        set(u,'Fontsize',8.3)
        ordere13=EF_con_sort(:,6);
end
colormap(cm);
ylabel('EJ')
xticklabels(labsconEF(1:15));
xtickangle(45)
set(gca,'FontSize',11.5);
%% Highest detail level EF exp and inv sectors
order=[4 3 2 1];
labels={'Pri. Consumption', 'Gov. Consumption', 'Pri. Investment','Gov. Investment'};
f12=figure('pos',[1 50 720 540]);
q=bar(1e-6*EF_exp_sort(1:15,1:4),'stack'); % convert MJ to TJ
title(['Detailed sectors with highest Energy Footprint, expenditure and investment, ' num2str(year)])
colormap(cm);
u=legend(q(order),labels{order});
ylabel('TJ')
xticklabels(labsexEF(1:15));
xtickangle(45)
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% Detailed highest consumers and producers of CF impacts (2-way Contribution analysis results)
Ds=D_GHGt; % total GHG impacts from prod and cons
Ds(:,393)=sum(Ds,2); % sum from prod. sectors
Ds(:,394)=1:392; % number sectors in original order
DS=flipud(sortrows(Ds,393)); % sort by largest producers
D_pro_sec=DS(1:15,394); % the sector numbers of highest producers
DsT=DS'; % transpose
DsT(393:394,:)=[]; % remove the prod. totals and sector numbers
DsT(:,393)=sum(DsT,2); % sum from cons. sectors
DsT(:,394)=1:392; % original sector numbers
DsTs=flipud(sortrows(DsT,393)); % sort by largest consumers
D_con_sec=DsTs(1:15,394); % the sector numbers of highest consumers
%% Manually extract production sectors of interest
main_pr=[22 14 281 8 23 88 340 123 7]; % define sectors of interest from prod. perspective
m=size(main_pr,2); % number of interesting prod. sectors 
other=1:392; 
other(main_pr)=[]; % remove sectors of interest from list of all sector numbers
Dn=[D_GHGt(main_pr,:); D_GHGt(other,:)]; % Order production sectors by interesting, other (not in order of highest impacts)
DnT=Dn'; % transpose - put production sectors on columns, consumption on rows
DnT(:,393)=sum(DnT,2); % total consumption sectors
DnT(:,394)=1:392; % sector number of consumption sectors
DnTs=flipud(sortrows(DnT,393)); % sort by largest consumption sectors
Dn_con_sec=DnTs(1:15,394); % get sector numbers of 15 largest consumption sectors
DnTs(:,393:394)=[]; % remove totals and sector numbers
ot=sum(DnTs(1:15,m+1:end),2); % the 'other' production impacts from the highest consumption sectors, not explained by the extracted production sectors
cont=[DnTs(1:15,1:m) ot]; % matrix of contributions from prod. sectors of interest, and other production sectors
cs=sum(cont,2); % row sums, total of consumption sectors
c=diag(cs)\cont; % get pc of contributions from prod. sectors to cons. sectors impacts
c(:,m+3)=c(:,m+1); % cont from 'other' prod. sectors incl. direct, save numbers here
c(:,m+(1:2))=zeros(15,2); % make space in columns m+1, m+2
for i = 1:15 % fill m+1 column with Direct impacts
c(i,m+1)=D_GHGt(D_con_sec(i),D_con_sec(i))/cs(i); 
if sum(c(i,1:m+1))>1% if direct were already included, make zero, avoid double counting
    c(i,m+1)=0;
end
c(i,m+2)=c(i,m+3)-c(i,m+1); % contr. from 'other' prod. not incl. Direct
end
c(:,m+3)=[]; % remove 'other' + Direct impacts column
order=fliplr(1:m+2);
%% contribution to 'other' consumption sectors
otr2c1=DnTs(16:392,1:m); % inputs to other consumption sectors, from extraction prod sectors
otr2c2=DnTs(16:392,m+1:392); % inputs to other cons sectors, from other prod sectors
D_con_sec_r=DsTs(16:end,394); % sector numbers of highest consumption impact sectors outside top 15, i.e. 16:392
contr2=[sum(otr2c1,1) sum(sum(otr2c2))]; % contr of int prod sectors to other cons sectors; contr of other prod sectors to other cons sectors
csr2=sum(contr2); % total 'other' impacts
dir=zeros(1,377); % define vector to store direct impacts from other sectors
for i=1:377 % production inputs to consumption impacts 16:392 (in order of highest impacts)
dir(i)=D_GHGt(D_con_sec_r(i),D_con_sec_r(i))/csr2; % each sectors cont. to overall other impacts
end
contr2n=contr2/csr2; % make vector of contribution pc to 'other' consumption sector
contr2n(m+3)=contr2n(m+1); % move contr from 'other' prod sectors incl. direct to new column
contr2n(m+1)=sum(dir); % include direct impacts
contr2n(m+2)=contr2n(m+3)-contr2n(m+1); % 'other' impacts not incl. direct (self) impacts
contr2n(m+3)=[]; % remove other + direct column
ctot=[c;contr2n]; % total contribution from to top consumption sectors, other consumption sectors
f13=figure;
q=bar(ctot,'stack');
xlab=[Z(D_con_sec,3);{'Other'}];
xticks(1:16);
xticklabels(xlab)
xtickangle(45)
labels=[Z(main_pr,3) ;{'Own*'};{'Other'}];
title(['Contribution from production sectors to CF of consumption, ' num2str(year)])
u=legend(q(order),labels{order},'Location','northeastoutside');
title(u,'Production sectors')
colormap(jet(m+12))
ylim([0 1])
xlabel('\bfConsumption sectors')
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% cont to capital related CF (similar 2-way contribution analysis) 
k_pr=[22 281 133 14 123 20 29 23 21 36 35 34 31]; % most important production sectors for K assets
mk=size(k_pr,2); % number of production sectors analysed
otherk=1:392; 
otherk(k_pr)=[]; % remove numbers of important prod sectors
Ddk=[D_GHGdiff(k_pr,:); D_GHGdiff(otherk,:)]; % put important prod sectors to highest rows
DkT=Ddk'; % Transpose, put consumption on rows, production on columns
DkT(:,393)=sum(DkT,2); % total of consum sectors
DkT(:,394)=1:392; % sec num of cons sectors
DkTs=flipud(sortrows(DkT,393)); % sort by highest cap. consumption sectors
Dk_con_sec=DkTs(1:12,394); % sector numbers of highest cap consumption 
Dk_con_sec_r=DkTs(13:end,394); % sec numbers of rest of cap consumption
DkTs(:,393:394)=[]; % remove totals and sec numbers
otk=sum(DkTs(1:12,mk+1:end),2); % inputs to highest cap cons sectors from 'other' prod sectors
contk=[DkTs(1:12,1:mk) otk];  % matrix of contributions from prod. sectors of interest, and other production sectors
csk=sum(contk,2);  % row sums, total of consumption sectors
ck=diag(csk)\contk; % pc contributions from prod. sectors to cons. sectors impacts
% don't include 'Direct' impacts for capital contribution analysis
otr2ck1=DkTs(13:392,1:mk);
otr2ck2=DkTs(13:392,mk+1:392);
contrk2=[sum(otr2ck1,1) sum(sum(otr2ck2))];
csrk2=sum(contrk2);
contrk2n=contrk2/csrk2;
cktot=[ck;contrk2n];
cktot_reduce=[cktot(:,1:9) sum(cktot(:,10:12),2) cktot(:,13:14)];

f14=figure;
order=fliplr(1:mk-1);
q=bar(cktot_reduce,'stack');
xlab=[Z(Dk_con_sec,3);{'Other'}];
xticks(1:13);
xticklabels(xlab)
xtickangle(45)
labels=[Z(k_pr(1:9),3); {'Residential Structures'};  Z(k_pr(13),3); {'Other'}];
title(['Contribution from production sectors to CF of capital consumption, ' num2str(year)])
u=legend(q(order),labels{order},'Location','northeastoutside');
title(u,'Production sectors')
colormap(jet(m+12))
ylim([0 1])
xlabel('\bfConsumption sectors')
ax=gca;
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% total impacts
dt_con=IO.C*IO.S*Lt*y_exp;
dt_expinv=IO.C*IO.S*L*y_exp_inv;
%% Impact intensities in 2013 USD
M_int_At=(tot_mat_char*IO.S*Lt)';
M_int_A=(tot_mat_char*IO.S*L)';
M_int_At_purch=(tot_mat_char*IO.S*Lt*IO.T)';
M_int_At_purch_392=[M_int_At_purch(1:274); zeros(3,1); M_int_At_purch(275:389)];
M_int_A_purch=(tot_mat_char*IO.S*L*IO.T)';
M_int_A_purch_392=[M_int_A_purch(1:274); zeros(3,1); M_int_A_purch(275:389)];

E_int_At=(IO.C(11,:)*IO.S*Lt)';
E_int_A=(IO.C(11,:)*IO.S*L)';
E_int_At_purch=(IO.C(11,:)*IO.S*Lt*IO.T)';
E_int_At_purch_392=[E_int_At_purch(1:274); zeros(3,1); E_int_At_purch(275:389)];
E_int_A_purch=(IO.C(11,:)*IO.S*L*IO.T)';
E_int_A_purch_392=[E_int_A_purch(1:274); zeros(3,1); E_int_A_purch(275:389)];

if isequal(year,2007) % add in intensities per 2013USD
    GHG_int_pr07=GHG_int_pr;
    GHG_int_pr=(IO.C(4,:)*IO.S_2013USD)'; % carbon intensity of production
    GHG_int_Atot07=GHG_int_Atot;
    GHG_int_Atot=(IO.C(4,:)*IO.S_2013USD*Lt)'; % carbon intensity of consumption, including capital
    GHG_int_con07=GHG_int_con;
    GHG_int_con=(IO.C(4,:)*IO.S_2013USD*L)'; % carbon intensity of cons., excluding capital
    M_int_At07=M_int_At;
    M_int_At=(tot_mat_char*IO.S_2013USD*Lt)'; % Material intensity of consumption, incl. capital
    M_int_A07=M_int_A;
    M_int_A=(tot_mat_char*IO.S_2013USD*L)'; % Material intensity of consumption, excl. capital
    E_int_At07=E_int_At;
    E_int_At=(IO.C(11,:)*IO.S_2013USD*Lt)'; % Energy intensity of consumption, incl. capital
    E_int_A07=E_int_A;
    E_int_A=(IO.C(11,:)*IO.S_2013USD*L)'; % Energy intensity of consumption, excl. capital
end
Mi_inc=M_int_At./M_int_A; % ratio of intensity with capital to without capital
Mi_inc(isnan(Mi_inc))=1;
Mi_inc(isinf(Mi_inc))=1;
Mi_inc=Mi_inc-1; % increase in intensity due to capital (proportional)

GHG_int_capcon=GHG_int_Atot-GHG_int_con; % GHG intensity of capital consumption, per Mill USD
GHGi_inc=GHG_int_Atot./GHG_int_con; % ratio of intensity with capital to without capital
GHGi_inc(isnan(GHGi_inc))=1;
GHGi_inc(isinf(GHGi_inc))=1;
GHGi_inc=GHGi_inc-1; % increase in intensity due to capital (proportional)
GHGi_cc=GHG_int_capcon./GHG_int_Atot; % proportion of total impacts coming from capital inputs

Ei_inc=E_int_At./E_int_A; % ratio of intensity with capital to without capital
Ei_inc(isnan(Ei_inc))=1;
Ei_inc(isinf(Ei_inc))=1;
Ei_inc=Ei_inc-1;  % increase in intensity due to capital (proportional)
row=[1:392]';

Gabs_inc=diff_pr+diff_gov; % total absolute increase in GHG emissions with capital
GHG_pc_inc=Gabs_inc./(d_pr_ex+d_gov_ex); % sectoral percent increase in GHG emissions with capital
GHG_pc_inc(isnan(GHG_pc_inc))=0; 
GHG_pc_inc(isinf(GHG_pc_inc))=0;
GHG_dir_pc=GHG_int_pr./GHG_int_Atot; % proportion of total impacts coming from direct impacts 

Mabs_inc=diff_MF_pr+diff_MF_gov; % total absolute increase in MF with capital
Eabs_inc=diff_EF_pr+diff_EF_gov; % total absolute increase in EF with capital
int=table(row,GHG_int_pr,GHG_int_con,GHG_int_Atot,GHG_dir_pc,GHGi_inc,GHGi_cc,GHG_int_Atot_purch_392,M_int_A,M_int_At,Mi_inc,E_int_A,E_int_At,Ei_inc,Gabs_inc,GHG_pc_inc,Mabs_inc,Eabs_inc,'RowNames',Z(:,3)); % table of intensities and changes

GHG_pc_inc_C=sum(Gabs_inc)/sum((d_pr_ex+d_gov_ex))% economy wide percent increase in GHG emissions with capital
GHG_pc_ofT_C=sum(Gabs_inc)/(sum(Gabs_inc)+sum(d_pr_ex+d_gov_ex)) % economy wide capital percent of total CF
M_pc_inc_C=sum(Mabs_inc)/sum((MF_pr_ex+MF_gov_ex)) % economy wide percent increase in MF with capital
M_pc_ofT_C=sum(Mabs_inc)/(sum(Mabs_inc)+sum(MF_pr_ex+MF_gov_ex)) % economy wide capital percent of total MF
E_pc_inc_C=sum(Eabs_inc)/sum((EF_pr_ex+EF_gov_ex)) % economy wide percent increase in EF
E_pc_ofT_C=sum(Eabs_inc)/(sum(Eabs_inc)+sum(EF_pr_ex+EF_gov_ex)) % economy wide capital percent of total EF

GHG_pc_inc_I=sum(d_pr_inv+d_fedg_inv+d_slg_inv)/sum((d_pr_ex+d_gov_ex)); % economy wide % increase in GHG emissions with investment
GHG_pc_ofT_I=sum(d_pr_inv+d_fedg_inv+d_slg_inv)/(sum(d_pr_inv+d_fedg_inv+d_slg_inv)+sum(d_pr_ex+d_gov_ex))% economy wide investment % of total CF
M_pc_inc_I=sum(MF_pr_inv+MF_fedg_inv+MF_slg_inv)/(sum(MF_pr_ex+MF_gov_ex)); % economy wide % increase in MF with investment
M_pc_ofT_I=sum(MF_pr_inv+MF_fedg_inv+MF_slg_inv)/(sum(MF_pr_inv+MF_fedg_inv+MF_slg_inv)+sum(MF_pr_ex+MF_gov_ex)) % economy wide investment % of total MF
E_pc_inc_I=sum(EF_pr_inv+EF_fedg_inv+EF_slg_inv)/(sum(EF_pr_ex+EF_gov_ex)) ; % economy wide % increase in EF with investment
E_pc_ofT_I=sum(EF_pr_inv+EF_fedg_inv+EF_slg_inv)/(sum(EF_pr_inv+EF_fedg_inv+EF_slg_inv)+sum(EF_pr_ex+EF_gov_ex)) % economy wide investment % of total EF
%% Extra plots with 2007 scatter on 2013 stacked bars
if isequal(year,2013)
load('Results_2007.mat') % to compare 2007 and 2013 impact on one chart
R07.s=sum(R07.sum_exp_inv,2);
order= [5 4 3 2 1];
labels={'2007 total','Pri. Consumption', 'Gov. Consumption', 'Pri. Investment','Gov. Investment'};
f15=figure('pos',[1 50 720 540]);
q=bar(sum_exp_inv_Mt,'stack');
hold on
r=scatter(1:25,1e-9*R07.s,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert kg to Mt
title(['A. Carbon Footprint of consumption & capital investment, ' num2str(year)]) 
u=legend([q(order(2:end)) r],labels{order});
set(u,'Fontsize',9)

colormap(cm);
ylabel('Mt CO2eq')
ylim([0 1600]); xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',12.5);
set(gca,'TitleFontSizeMultiplier',1.06);
hold off

R07.sc=sum(R07.sum_con_sec,2);
labels={'2007 total','Pri. Consumption (PC)', 'Gov. Consumption (GC)', 'Capital inputs to PC','Capital inputs to GC'};

f16=figure('pos',[1 50 720 540]);
q=bar(sum_con_sec_Mt,'stack');
hold on
r=scatter(1:25,1e-9*R07.sc,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert kg to Mt
title(['B. Carbon Footprint of consumption incl. capital inputs, ' num2str(year)])
u=legend([q(order(2:end)) r],labels{order});
set(u,'Fontsize',9)
colormap(cm);

ylabel('Mt CO2eq')
ylim([0 1600]); xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',12.5);
set(gca,'TitleFontSizeMultiplier',1.06);
hold off

% Materials
R07.m=sum(R07.MF_exp_inv,2);
labels={'2007 total','Pri. Consumption', 'Gov. Consumption', 'Pri. Investment','Gov. Investment'};
f17=figure('pos',[1 50 720 540]);
q=bar(MF_exp_inv_Mt,'stack');
hold on
r=scatter(1:25,1e-9*R07.m,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert kg to Mt
title(['E. Material Footprint of consumption & capital investment, ' num2str(year)])
u=legend([q(order(2:end)) r],labels{order});
set(u,'Fontsize',9)
colormap(cm);

ylabel('Mt material')
ylim([0 2400]); xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',12.5);
set(gca,'TitleFontSizeMultiplier',1.06);
hold off

R07.mc=sum(R07.MF_con_sum,2);
labels={'2007 total','Pri. Consumption (PC)', 'Gov. Consumption (GC)', 'Capital inputs to PC','Capital inputs to GC'};
f18=figure('pos',[1 50 720 540]);
q=bar(MF_con_sum_Mt,'stack');
hold on
r=scatter(1:25,1e-9*R07.mc,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert kg to Mt
title(['F. Material Footprint of consumption incl. capital inputs, ' num2str(year)])
u=legend([q(order(2:end)) r],labels{order});
set(u,'Fontsize',9)
colormap(cm);

ylabel('Mt material')
ylim([0 2400]); xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',12.5);
set(gca,'TitleFontSizeMultiplier',1.06);
hold off

% Energy
R07.e=sum(R07.EF_exp_inv,2);
labels={'2007 total','Pri. Consumption', 'Gov. Consumption', 'Pri. Investment','Gov. Investment'};
f19=figure('pos',[1 50 720 540]);
q=bar(1e-12*EF_exp_inv,'stack'); % convert MJ to EJ
hold on
r=scatter(1:25,1e-12*R07.e,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert MJ to EJ
title(['C. Energy Footprint of consumption & capital investment, ' num2str(year)])
u=legend([q(order(2:end)) r],labels{order});
set(u,'Fontsize',9)
colormap(cm);

ylabel('EJ')
ylim([0 25]); xlim([0 26]);
xticks(1:27)
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',12.5);
set(gca,'TitleFontSizeMultiplier',1.06);
hold off

R07.ec=sum(R07.EF_con_sum,2);
labels={'2007 total','Pri. Consumption (PC)', 'Gov. Consumption (GC)', 'Capital inputs to PC','Capital inputs to GC'};
f20=figure('pos',[1 50 720 540]);
q=bar(1e-12*EF_con_sum,'stack'); % convert MJ to EJ
hold on
r=scatter(1:25,1e-12*R07.ec,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert MJ to EJ
title(['D. Energy Footprint of consumption incl. capital inputs, ' num2str(year)])
u=legend([q(order(2:end)) r],labels{order});
set(u,'Fontsize',9)
colormap(cm);

ylabel('EJ')
ylim([0 27]); xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',12.5);
set(gca,'TitleFontSizeMultiplier',1.06);
hold off

% f21=figure('pos',[1 50 600 600]);
f21=figure;
scatter(GHG_int_Atot*1e-6,GHG_int_Atot_purch_392*1e-6,50,'.k')
title(['Carbon multipliers in Purchaser vs Producer prices, ' num2str(year)])
ylabel('kgCO2eq/2013USD Purchasers prices')
xlabel('kgCO2eq/2013USD Producers prices')
set(gca,'FontSize',12.5);
set(gca,'TitleFontSizeMultiplier',1.06);
xlim([0 5.5])
ylim([0 5.5])
%% Create stacked group bar charts, detail level, 2007 and 2013
% CF
y1=zeros(2,15,4);
y1(1,:,:)=R07.sum_con_sort_Mt(1:15,1:4); % 2007 impacts
yr=sum_con(R07.order07,:); % 2013 impacts, use 2007 highest CF consumption sector order
y1(2,:,:)=1e-9*yr(1:15,1:4); % 2013 impacts, convert kg to Mt
y2=permute(y1,[2,1,3]); % rearrange 3D array

label=R07.labsconCF07(1:15);
order=[4 3 2 1];
labels={'Pri. Consumption (PC)', 'Gov. Consumption (GC)', 'Capital inputs to PC','Capital inputs to GC'};
% using plotBarStackGroups function, figure is defined in function
q=plotBarStackGroups(y2,label);
xtickangle(45)
u=legend(q(1,order),labels{order});
title('A. Detailed sectors with highest Carbon Footprint 2007, 2013')
ylabel('Mt CO2eq')
ylim([0 1600])
set(gca,'FontSize',11.5);
colormap(cm);
set(u,'Fontsize',8.3)
box on
% EF
y1=zeros(2,15,4);
y1(1,:,:)=R07.EF_con_sort(1:15,1:4); % 2007 impacts
yr=EF_con(R07.ordere07,:); % 2013 impacts, use 2007 highest EF consumption sector order
y1(2,:,:)=yr(1:15,1:4); % 2013 impacts
y2=1e-12*permute(y1,[2,1,3]); % rearrange 3D array, convert MJ to EJ
label=R07.labsconEF07(1:15);


q=plotBarStackGroups(y2,label);
xtickangle(45)
u=legend(q(1,order),labels{order});
title('B. Detailed sectors with highest Energy Footprint 2007, 2013')
ylabel('EJ')
ylim([0 27])
set(gca,'FontSize',11.5);
colormap(cm);
set(u,'Fontsize',8.3)
box on
%MF
y1=zeros(2,15,4);
y1(1,:,:)=R07.MF_con_sort_Mt(1:15,1:4); % 2007 impacts
yr=MF_con(R07.orderm07,:); % 2013 impacts, use 2007 highest MF consumption sector order
y1(2,:,:)=1e-9*yr(1:15,1:4); % 2013 impacts, convert kg to Mt
y2=permute(y1,[2,1,3]); % rearrange 3D array
label=R07.labsconMF07(1:15);


q=plotBarStackGroups(y2,label);
xtickangle(45)
u=legend(q(1,order),labels{order});
title('C. Detailed sectors with highest Material Footprint 2007, 2013')
ylabel('Mt material')
ylim([0 800])
ax=gca;
set(gca,'FontSize',11.5);
colormap(cm);
set(u,'Fontsize',8.3)
box on
end

%% plot impacts by stressor type
%GHG
ghg=diag(IO.C(4,:))*IO.S*Lt*diag(y_exp);
ghg=ghg(11:25,:);
%EF
ef=diag(IO.C(11,:))*IO.S*Lt*diag(y_exp);
ef=ef(2:10,:);
%MF
mf=IO.C(16:19,:)*IO.S*Lt*diag(y_exp);

if year == 2007
ghg_sort = ghg(:,order07);
xlabg=[Z(order07(1:12),3);{'Other'}];
e_sort=ef(:,ordere07);
xlabe=[Z(ordere07(1:12),3);{'Other'}];
m_sort=mf(:,orderm07);
xlabm=[Z(orderm07(1:12),3);{'Other'}];
end

if year == 2013
ghg_sort = ghg(:,order13);
xlabg=[Z(order13(1:12),3);{'Other'}];
e_sort=ef(:,ordere13);
xlabe=[Z(ordere13(1:12),3);{'Other'}];
m_sort=mf(:,orderm13);
xlabm=[Z(orderm13(1:12),3);{'Other'}];
end  

f=figure('pos',[1 50 720 540]);
ghg_all=1e-9*[ghg_sort(:,1:12) sum(ghg_sort(:,13:end),2)];
ghg_all=[ghg_all(1:3,:); sum(ghg_all(4:end,:),1)];

q=bar(ghg_all','stack');
order=fliplr(1:size(ghg_all,1));
ylabel('Mt CO2eq')
if year ==2007
title(['A. Detailed sectors with highest CF by GHG type, ', num2str(year)]);
end
if year ==2013
title(['B. Detailed sectors with highest CF by GHG type, ', num2str(year)]);
end
xticks(1:13)
xticklabels(xlabg)
xtickangle(45)
labels=[{'CO2'}, {'N2O'},{'CH4'}, {'Other'}];
u=legend(q(order),labels{order});
set(u,'Fontsize',9)
colormap('parula')
set(gca,'FontSize',11.5);
set(gca,'TitleFontSizeMultiplier',1.08);
% EF
f=figure('pos',[1 50 720 540]);
e_all=1e-12*[e_sort(:,1:12) sum(e_sort(:,13:end),2)];

q=bar(e_all','stack');
order=fliplr(1:size(e_all,1));
ylabel('EJ')
if year == 2007
title(['C. Detailed sectors with highest EF by energy resource, ', num2str(year)]);
end
if year == 2013
title(['D. Detailed sectors with highest EF by energy resource, ', num2str(year)]);
end
xticks(1:13)
xticklabels(xlabe)
xtickangle(45)
labels=[{'Coal'}, {'Nat gas'},{'Nat gas liquids'}, {'Oil'},{'Nuclear'},{'Biomass'},{'Hydro'},{'Solar'},{'Wind'}];
u=legend(q(order),labels{order});
set(u,'Fontsize',9)
ylim([0 42]);
colormap(jet(15))
set(gca,'FontSize',11.5);
set(gca,'TitleFontSizeMultiplier',1.08);
%MF
f=figure('pos',[1 50 720 540]);
m_all=1e-9*[m_sort(:,1:12) sum(m_sort(:,13:end),2)];
q=bar(m_all','stack');
order=fliplr(1:size(m_all,1));
ylabel('Mt')
if year == 2007
title(['E. Detailed sectors with highest MF by material class, ', num2str(year)]);
end
if year == 2013
title(['F. Detailed sectors with highest MF by material class, ', num2str(year)]);
end
xticks(1:13)
xticklabels(xlabm)
xtickangle(45)
labels=[{'Metals'}, {'Fossil fuels'},{'Other minerals'}, {'Biomass'}];
u=legend(q(order),labels{order});
set(u,'Fontsize',9)
ylim([0 2200]);
colormap('jet')
set(gca,'FontSize',11.5);
set(gca,'TitleFontSizeMultiplier',1.08);
%% Summary results
if year == 2013
% CF, in Mt CO2eq    
CF_wo_K=sum(sum_con(:,1:2),2)*1e-9;
CF_w_K=sum(sum_con(:,1:4),2)*1e-9;
CF_wo_K_07=sum(R07.sum_con(:,1:2),2)*1e-9;
CF_w_K_07=sum(R07.sum_con(:,1:4),2)*1e-9;
% EF, in TJ
EF_wo_K=sum(EF_con(:,1:2),2)*1e-9;
EF_w_K=sum(EF_con(:,1:4),2)*1e-9;
EF_wo_K_07=sum(R07.EF_con(:,1:2),2)*1e-9;
EF_w_K_07=sum(R07.EF_con(:,1:4),2)*1e-9;
% MF, in Mt
MF_wo_K=sum(MF_con(:,1:2),2)*1e-9;
MF_w_K=sum(MF_con(:,1:4),2)*1e-9;
MF_wo_K_07=sum(R07.MF_con(:,1:2),2)*1e-9;
MF_w_K_07=sum(R07.MF_con(:,1:4),2)*1e-9;
end
% CI kg/$
CI_wo_k=GHG_int_con*1e-6;
CI_w_k=GHG_int_Atot*1e-6;
CI_wo_k_purch_392=GHG_int_con_purch_392*1e-6;
CI_w_k_purch_392=GHG_int_Atot_purch_392*1e-6;
% EI MJ/$
EI_wo_k=E_int_A*1e-6;
EI_w_k=E_int_At*1e-6;
EI_wo_k_purch_392=E_int_A_purch_392*1e-6;
EI_w_k_purch_392=E_int_At_purch_392*1e-6;
% MI kg/$
MI_wo_k=M_int_A*1e-6;
MI_w_k=M_int_At*1e-6;
MI_wo_k_purch_392=M_int_A_purch_392*1e-6;
MI_w_k_purch_392=M_int_At_purch_392*1e-6;

if year == 2013
int_tot = table(row,CI_wo_k,CI_w_k,CI_wo_k_purch_392,CI_w_k_purch_392,EI_wo_k,EI_w_k,EI_wo_k_purch_392,EI_w_k_purch_392,MI_wo_k,MI_w_k,MI_wo_k_purch_392,MI_w_k_purch_392,'RowNames',Z(:,3)); 
end

if year == 2007
    % CI kg/$
    CI_wo_k=GHG_int_con07*1e-6;
    CI_w_k=GHG_int_Atot07*1e-6;
%     CI_wo_k_purch_392=GHG_int_con_purch_392*1e-6;
%     CI_w_k_purch_392=GHG_int_Atot_purch_392*1e-6;
    % EI MJ/$
    EI_wo_k=E_int_A07*1e-6;
    EI_w_k=E_int_At07*1e-6;
%     EI_wo_k_purch_392=E_int_A_purch_392*1e-6;
%     EI_w_k_purch_392=E_int_At_purch_392*1e-6;
    % MI kg/$
    MI_wo_k=M_int_A07*1e-6;
    MI_w_k=M_int_At07*1e-6;
%     MI_wo_k_purch_392=M_int_A_purch_392*1e-6;
%     MI_w_k_purch_392=M_int_At_purch_392*1e-6;
    
    int_tot = table(row,CI_wo_k,CI_w_k,CI_wo_k_purch_392,CI_w_k_purch_392,EI_wo_k,EI_w_k,EI_wo_k_purch_392,EI_w_k_purch_392,MI_wo_k,MI_w_k,MI_wo_k_purch_392,MI_w_k_purch_392,'RowNames',Z(:,3)); 
end


%% Save
switch year
    case 2007
        R07.D_GHGt=D_GHGt;
        R07.dt_con=dt_con;
        R07.dt_expinv=dt_expinv;
        R07.sum_exp_inv=sum_exp_inv;
        R07.MF_exp_inv=MF_exp_inv;
        R07.MF_con_sum=MF_con_sum;
        R07.EF_exp_inv=EF_exp_inv;
        R07.EF_con_sum=EF_con_sum;
        R07.sum_con_sec=sum_con_sec;
        R07.MF_sum_con=MF_sum_con;
        R07.EF_sum_con=EF_sum_con;
        R07.ZLab25=ZLab25;
        R07.int=int;
        R07.sum_con=sum_con;
        R07.MF_con=MF_con;
        R07.EF_con=EF_con;
        R07.order07=order07;
        R07.ordere07=ordere07;
        R07.orderm07=orderm07;
        R07.sum_con_sort_Mt=sum_con_sort_Mt;
        R07.EF_con_sort=EF_con_sort;
        R07.MF_con_sort_Mt=MF_con_sort_Mt;
        R07.labsconCF07=labsconCF;
        R07.labsconEF07=labsconEF;
        R07.labsconMF07=labsconMF;
        save(['Results_' question1{:}],'R07')
    case 2013
        R13.D_GHGt=D_GHGt;
        R13.dt_con=dt_con;
        R13.dt_expinv=dt_expinv;
        R13.sum_exp_inv=sum_exp_inv;
        R13.MF_exp_inv=MF_exp_inv;
        R13.MF_con_sum=MF_con_sum;
        R13.EF_exp_inv=EF_exp_inv;
        R13.EF_con_sum=EF_con_sum;
        R13.sum_con_sec=sum_con_sec;
        R13.MF_sum_con=MF_sum_con;
        R13.EF_sum_con=EF_sum_con;
        R13.ZLab25=ZLab25;
        R13.sum_con=sum_con;
        R13.MF_con=MF_con;
        R13.EF_con=EF_con;
        R13.int=int;
        save(['Results_' question1{:}],'R13')
end


