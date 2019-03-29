% File to analyse GHG, Energy, and Material Impacts of US consumption
% using USEEIO and capital flow matrices
% For Capital in US Footprints paper
% March 28 2019
% Peter Berrill

% Required input files:
% USEEIO_*year*.mat where *year* = 2012 or 2007

% Original functions called by this script:
% sector_reduce
% External functions called by this scripts:
% plotBarStackGroups
%% Load data
clc; clear;
close all
addpath('Input_Files')
question1 = inputdlg('Do you want to view impacts for 2007 or 2012??  ');
year=str2double(question1{:});
switch year
    case 2007
        load(['USEEIO_2007.mat'])
      
    case 2012
        load(['USEEIO_2012.mat'])
end
exp=[1, 9, 13, 17]; % all expenditure categories
gov_exp=[9,13,17]; % public expenditure
feddef_exp=9;
fednondef_exp=13;
slg_exp=17;
priv_inv=2:5; % private investment categories
fed_gov_inv=[10:12, 14:16]; % federal government investment categories
fed_def_inv=10:12;
fed_nondef_inv=14:16;
sl_gov_inv=18:20; % state and local government investment categories
gov_inv=[fed_gov_inv sl_gov_inv]; % government investmenst categories
exp_inv=[1:5, 9:20]; % expenditure and investment, excluding trade and changes in stocks
invest=[2:5, 10:12, 14:16, 18:20]; % all investment categories of final demand

IO.Y(IO.Y(:,exp_inv)<0)=0; % remove negative final demand for used/scrap goods (i.e. inputs to reuse) and RoW adjustment
IO.YPur(IO.YPur(:,exp_inv)<0)=0;

y_exp=sum(IO.Y(:,exp),2); % personal and government expenditure component of final demand 
y_exp_pur=sum(IO.YPur(:,exp),2);
y_exp_inv=sum(IO.Y(:,exp_inv),2); % all expenditure and investment spending, excl. trade and changes in stocks
y_gov_exp=sum(IO.Y(:,gov_exp),2); % government expenditure
y_feddef_exp=sum(IO.Y(:,feddef_exp),2); % federal defense expenditure
y_fednondef_exp=sum(IO.Y(:,fednondef_exp),2); % federal nondefense expenditure
y_slg_exp=sum(IO.Y(:,slg_exp),2); % SL govt expenditure
y_invest=sum(IO.Y(:,invest),2); % total investment spending
y_priv_inv=sum(IO.Y(:,priv_inv),2); % private investment spending
y_gov_inv=sum(IO.Y(:,gov_inv),2); % government investment spending
y_fedg_inv=sum(IO.Y(:,fed_gov_inv),2); % total federal government investment spending
y_feddef_inv=sum(IO.Y(:,fed_def_inv),2); % federal defense investment spending
y_fednondef_inv=sum(IO.Y(:,fed_nondef_inv),2); % federal defense investment spending
y_slg_inv=sum(IO.Y(:,sl_gov_inv),2); % state and local government investment spending
y_hh=IO.Y(:,1); % household consumption expenditures
%% GHG intensities
GHG_int_pr=(IO.C(2,:)*IO.S)'; % Carbon intensity of production, AR5 method without feedbacks
GHG_int_con=(IO.C(2,:)*IO.S*IO.L)'; % Carbon intensity of consumption, no capital
GHG_int_con_purch=(IO.C(2,:)*IO.S*IO.L*IO.T)';% Carbon intensity of consumption, no capital, in purchasers prices
GHG_int_con_purch_408=[GHG_int_con_purch(1:281); zeros(7,1); GHG_int_con_purch(282); 0; GHG_int_con_purch(283:400)];% Carbon intensity of consumption, no capital, in purchasers prices
GHG_int_Atot=(IO.C(2,:)*IO.S*IO.Lt)'; % Carbon inensity of consumption, incl. capital
GHG_int_Atot_purch=(IO.C(2,:)*IO.S*IO.Lt*IO.T)'; % Carbon inensity of consumption, incl. capital, in purchasers prices
GHG_int_Atot_purch_408=[GHG_int_Atot_purch(1:281); zeros(7,1); GHG_int_Atot_purch(282); 0; GHG_int_Atot_purch(283:400)];
% Matrices of GHG intensities. Column = production, row = consumption.
% Column sums equal consumption intensities.
% Each row element corresponds to the contribution of that production 
% sector to the overall intensity. 
M_GHG0=diag(GHG_int_pr)*IO.L;
M_GHGt=diag(GHG_int_pr)*IO.Lt;
M_GHGt_purch=diag(GHG_int_pr)*IO.Lt*IO.T;
M_GHGdiff=M_GHGt-M_GHG0; % increase in carbon intensity due to capital
% Matrices of total emissions from consumption expenditure
D_GHG0=M_GHG0*diag(y_exp); 
D_GHGt=M_GHGt*diag(y_exp);
D_GHGdiff=D_GHGt-D_GHG0;
D_GHG_t_purch=M_GHGt_purch*diag(y_exp_pur);
%% private and public expenditure and investment CF in-year impacts 
d_pr_ex=(IO.C(2,:)*IO.S*IO.L*diag(y_hh))'; % CF from private (household) expenditures
d_gov_ex=(IO.C(2,:)*IO.S*IO.L*diag(y_gov_exp))'; % CF from government expenditures
d_feddef_ex=(IO.C(2,:)*IO.S*IO.L*diag(y_feddef_exp))';% CF from federal defense expenditures
d_fednondef_ex=(IO.C(2,:)*IO.S*IO.L*diag(y_fednondef_exp))';% CF from federal nondefense expenditures
d_slg_ex=(IO.C(2,:)*IO.S*IO.L*diag(y_slg_exp))'; % CF from sl govt expenditures
d_pr_inv=(IO.C(2,:)*IO.S*IO.L*diag(y_priv_inv))'; % CF from private investment
d_fedg_inv=(IO.C(2,:)*IO.S*IO.L*diag(y_fedg_inv))'; % CF from federal government investment
d_feddef_inv=(IO.C(2,:)*IO.S*IO.L*diag(y_feddef_inv))';
d_fednondef_inv=(IO.C(2,:)*IO.S*IO.L*diag(y_fednondef_inv))';
d_slg_inv=(IO.C(2,:)*IO.S*IO.L*diag(y_slg_inv))';  % CF from state and local government investment
%% purch price footprints
d_ex_purch=(IO.C(2,:)*IO.S*IO.L*IO.T*diag(y_exp_pur))';
d_con_t_purch=(IO.C(2,:)*IO.S*IO.Lt*IO.T*diag(y_exp_pur))';
diff_d_purch=d_con_t_purch-d_ex_purch;
sum_con_purch_Mt=1e-9*[d_ex_purch diff_d_purch];
sum_con_purch_Mt_408=[sum_con_purch_Mt(1:281,:); zeros(7,2); sum_con_purch_Mt(282,:); zeros(1,2); sum_con_purch_Mt(283:end,:)];

EF_ex_purch=(IO.C(6,:)*IO.S*IO.L*IO.T*diag(y_exp_pur))';
EF_t_purch=(IO.C(6,:)*IO.S*IO.Lt*IO.T*diag(y_exp_pur))';
diff_EF_purch=EF_t_purch-EF_ex_purch;
sum_EF_purch_PJ=1e-9*[EF_ex_purch diff_EF_purch];
sum_EF_purch_PJ_408=[sum_EF_purch_PJ(1:281,:); zeros(7,2); sum_EF_purch_PJ(282,:); zeros(1,2); sum_EF_purch_PJ(283:end,:)];

MF_ex_purch=(IO.C(11,:)*IO.S*IO.L*IO.T*diag(y_exp_pur))';
MF_t_purch=(IO.C(11,:)*IO.S*IO.Lt*IO.T*diag(y_exp_pur))';
diff_MF_purch=MF_t_purch-MF_ex_purch;
sum_MF_purch_Mt=1e-9*[MF_ex_purch diff_MF_purch];
sum_MF_purch_Mt_408=[sum_MF_purch_Mt(1:281,:); zeros(7,2); sum_MF_purch_Mt(282,:); zeros(1,2); sum_MF_purch_Mt(283:end,:)];
%% concatenate total expenditure results, Split up government
sum_all=[d_pr_ex d_feddef_ex d_fednondef_ex d_slg_ex d_pr_inv d_feddef_inv d_fednondef_inv d_slg_inv]; % CF from all expenditure and investment activity
sum_all(:,size(sum_all,2)+1)=sum(sum_all,2); % sum of exp. and inv. CF by consumption sector
sum_all(:,size(sum_all,2)+1)=1:size(sum_all,1); % commodity sector number
sum_all_sort=flipud(sortrows(sum_all,size(sum_all,2)-1)); % sort CF by largest to smallest consumption sector
labsexCF=meta.ZLabs_short(sum_all_sort(:,size(sum_all,2)),2); % commodity lablels in order of largest CF
%% CF of expenditure and capital consumption including capital
d_pr_con_t=(IO.C(2,:)*IO.S*IO.Lt*diag(y_hh))'; % total CF of private expenditures, including capital inputs
diff_pr=d_pr_con_t-d_pr_ex; % increase in private CF from consideration of capital inputs
d_feddef_con_t=(IO.C(2,:)*IO.S*IO.Lt*diag(y_feddef_exp))';
diff_fd=d_feddef_con_t-d_feddef_ex;
d_fednondef_con_t=(IO.C(2,:)*IO.S*IO.Lt*diag(y_fednondef_exp))';
diff_fnd=d_fednondef_con_t-d_fednondef_ex;
d_slg_con_t=(IO.C(2,:)*IO.S*IO.Lt*diag(y_slg_exp))';
diff_slg=d_slg_con_t-d_slg_ex;
%% concatenate expenditure and capital consumption results, Split up government
sum_con=[d_pr_ex d_feddef_ex d_fednondef_ex d_slg_ex diff_pr diff_fd diff_fnd diff_slg]; % consumption based total CF, broken into in-year, and capital contributions
sum_con(:,size(sum_con,2)+1)=sum(sum_con,2); % sum of total CF by consumption sector
sum_con(:,size(sum_con,2)+1)=1:size(sum_con,1); % commodity number
sum_con_Mt=1e-9*[sum(sum_con(:,1:4),2) sum(sum_con(:,5:8),2)]; % summed into overall impacts with and w/o capital, in Mt
sum_con_sort=flipud(sortrows(sum_con,size(sum_con,2)-1)); % sort CF by largest to smallest consumption sector
labsconCF=meta.ZLabs_short(sum_con_sort(:,size(sum_con,2)),2); % commodity lablels in order of largest total CF
%% Summary sector results investment CF, don't publish
order=fliplr(1:4); % needed for plotting figure legend
labels={'Pers Inv','Fed-def Inv','Fed-nondef Inv', 'SLG inv'}; % legend labels
code2=cell(408,1);
for i =1:408
    code2{i,1}=meta.ZLabs{i,1}(1:2);
        if  string(code2(i,1))=='GS'
            code2{i,1}='S0';
        end
        if  any(string(code2(i,1))==string({'44','45','4B'}))
            code2{i,1}='4X'; % combine retail trade sectors
        end
end
% distinguish residential construction sectors
code2{30}='2334';
code2{31}='2334';
code2{35}='2334';

[~,~,sector_label]=xlsread('agg_desc_new.csv');
sector_label(1,:)=[]; sector_label(:,2)=[];
sec_sum=[string(meta.ZLabs_short(:,1)) string(code2) sum_all]; 

[sum_exp_inv, ZLab25] = sector_reduce(string(sector_label),sec_sum,sum_all); % aggregate detailed level exp. inv. impacts to summary level
sum_exp_inv_Mt=1e-9*sum_exp_inv; % convert impacts from kg to Mega tons (metric), Mt
f1=figure('pos',[1 50 720 540]);
q=bar(sum_exp_inv_Mt(:,5:size(sum_exp_inv,2)-2),'stack');

switch year
    case 2007
        title(['A. Carbon Footprint of capital investment, ' num2str(year)])
        u=legend(q(order),labels{order});
        set(u,'Fontsize',8.3)
    case 2012
        title(['B. Carbon Footprint of consumption & capital investment, ' num2str(year)]) 
        u=legend(q(order),labels{order});
        set(u,'Fontsize',8.3)
end
colormap('jet');
ylabel('Mt CO2eq')
ylim([0 600]); xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',11.5);
set(gca,'TitleFontSizeMultiplier',1.06);
%% Summary sector results consumption CF results, don't publish
labels={'Pers CC','Fed-def CC','Fed-nondef CC', 'SLG CC'};
sec_sum_con=[string(meta.ZLabs_short(:,1)) string(code2) sum_con];
[sum_con_sec, ZLab25] = sector_reduce(string(sector_label),sec_sum_con,sum_con); % aggregate capital inclusive consumption impacts to summary level 
sum_con_sec_Mt=1e-9*sum_con_sec; % convert kg to MegaTon
f2=figure('pos',[1 50 720 540]);
q3=bar(sum_con_sec_Mt(:,5:size(sum_con_sec,2)-2),'stack');
switch year
    case 2007
        title(['C. Carbon Footprint of consumption incl. capital inputs, ' num2str(year)])
        u=legend(q3(order),labels{order});
        set(u,'Fontsize',8.3)
    case 2012 
        title(['D. Carbon Footprint of consumption incl. capital inputs, ' num2str(year)])
        u=legend(q3(order),labels{order});
        set(u,'Fontsize',8.3)
end
colormap('jet');
ylabel('Mt CO2eq')
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',11.5);
%% new graph inv/cons of capital only, include in main manuscript Fig 1A
order=fliplr(1:8);
K=zeros(size(sum_exp_inv,1)*3,8);
Lab=cell(size(sum_exp_inv,1)*3,1);
Lab(:)={''};
ylabels={'Private', 'Federal def', 'Federal non-def', 'State/local gov', 'Personal','Federal def', 'Federal non-def  ', 'State/local gov'};
for i=1:size(sum_exp_inv,1)
K(((i-1)*3)+1,1:4)=sum_exp_inv_Mt(i,5:8);
K(((i-1)*3)+2,5:8)=sum_con_sec_Mt(i,5:8);
Lab(((i-1)*3)+2)={ZLab25(i)};
end
rm=[34:36 49:51];
K(rm,:)=[]; % remov Delivery/Warehousing and Management
% vertical bar version
c=copper(64);
p=winter(64);
cm=zeros(8,3);
cm(1:4,:)=c(32:10:62,:);
cm(5:8,:)=p(32:10:62,:);

k1=[K(:,1:4), zeros(size(K,1),4)];
k2=[zeros(size(K,1),4) K(:,5:8)];
% graph investment and consumption impacts together
order4=fliplr(1:4);
f1a=figure('pos',[-1500 -830 1000 700]); 
hAx(1) = axes();
q81=bar(k1,1,'stack','Parent', hAx(1));
set(hAx(1), 'Box','on','Position',[.13 .25 .77 .68])
colormap(cm)
u1=legend(q81(order4),ylabels(order4),'Location','North','FontSize',9);
title(u1,'Capital Investment  ')
xlim([0 size(K,1)+1])
xticks(1.5:3:70.5);
xticklabels(ZLab25([1:11 13:16 18:end]));
xtickangle(41)
title(['A. Carbon Footprint of capital investment and consumption, ' num2str(year)])
ylabel('Mt CO2eq')
set(gca,'FontSize',12.5);
set(gca,'TickDir','both')
box off % remove outer ticks
%% second axis/legend
hAx(2) = copyobj(hAx(1),gcf);
q82=bar(k2,1,'stack','Parent',hAx(2));
set(hAx(2), 'Color','none', 'XTick',[], 'YTick', [], ...
    'Box','on','GridColor','none')
colormap(cm)
u2=legend(q82(order(1:4)),ylabels(order(1:4)),'Location','NorthEast','FontSize', 9, 'Color','w');
u1.Position=u1.Position+[0 0 0.008 0];
u2.Position=u2.Position+[0 0 0.008 0];
u1.Position=u2.Position-[.15 0 0 0];
title(u2,'Capital Consumption  ')
hAx(2).YLim=hAx(1).YLim;
hAx(2).XLim=hAx(1).XLim;
hAx(2).YTickLabel=[];
hAx(2).Position=hAx(1).Position; % this may need to be repeated after resizing the figure
set(gca,'FontSize',12.5);
u2.FontSize=u1.FontSize;
%% Highest detail level CF sectors
order2=[2 1];
labels={'Consumption Expenditures', 'Capital Consumption'};
f5=figure('pos',[1 50 720 540]);
sum_con_sort_Mt=1e-9*[sum(sum_con_sort(:,1:4),2) sum(sum_con_sort(:,5:8),2)]; % sum and convert kg to Mt
q=bar(sum_con_sort_Mt(1:15,:),'stack');
switch year
    case 2007
        title(['A. Max Carbon Footprint of detailed consumption sectors, ' num2str(year)])
        order07=sum_con_sort(:,10);
    case 2012
        title(['B. Max Carbon Footprint of detailed consumption sectors, ' num2str(year)])
        u=legend(q(order2),labels{order2});
        set(u,'Fontsize',8.3)
        order12=sum_con_sort(:,10);
end
colormap(cm);
ylabel('Mt CO2eq')
xticklabels(labsconCF(1:15));
xtickangle(45)
ylim([0 1600])
set(gca,'FontSize',11.5);
%% Highest detail level CF sectors with combined residential sector,probably don't publish
s=zeros(15,2); % preallocate matrix for detail CF results with combined res. sector
res=[2 4 5 13 24]; % Check on labsconCF. these should correspond to Electricity, Res. nat. gas, O-O Housing, Res. petroleum fuels, and T-O Housing
scs = sum_con_sort_Mt(1:17,:); % highest 17 CF sectors
other = sum_con_sort_Mt(18:end,:); % all other CF sectors
sum_other=sum(other,1)-sum_con_sort_Mt(24,:); % sum of other CF sectors
s(1,:)= sum(sum_con_sort_Mt(res,:)); % sum of the residential related sectors
scs(res(1:4),:)=[]; % remove the individual residential related sectors
s(2:14,:)=scs; % non-residential top CF sectors
s(15,:)=sum_other; % all other CF sectors
l=labsconCF(1:17);
l(res(1:4))=[];
reslab=[{'Housing & home energy'};l;{'All other sectors'}]; % xtick labels
f6=figure('pos',[1 50 720 540]);
q2=bar(s,'stack');
title(['Detailed sectors with highest consumption based Carbon Footprint, ' num2str(year)])
colormap(cm);
u=legend(q2(order2),labels{order2});
ylabel('Mt CO2eq')
xticklabels(reslab);
xtickangle(45)
ylim([0 2500])
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% Table 2 summary of Fig 1A
Tab2=[sum(sum_exp_inv_Mt(:,1:4),2) sum(sum_exp_inv_Mt(:,5:8),2)];
Tab2(:,3)=Tab2(:,2)./sum_exp_inv_Mt(:,9);
Tab2(:,4)=sum_exp_inv_Mt(:,9);
Tab2(:,5)=sum(sum_con_sec_Mt(:,5:8),2);
Tab2(:,6)=Tab2(:,5)./sum_con_sec_Mt(:,9);
Tab2(:,7)=sum_con_sec_Mt(:,9);
Tab2(isnan(Tab2))=0;
Tab2(27,:)=sum(Tab2);
Tab2(27,3)=Tab2(27,2)/(Tab2(27,1)+Tab2(27,2));
Tab2(27,6)=Tab2(27,5)/(Tab2(27,1)+Tab2(27,5));
%% private and public expenditure and investment Material Footprint in-year impacts 
MF_pr_ex=(IO.C(11,:)*IO.S*IO.L*diag(y_hh))'; % MF from private consumption expenditures, total materials
MF_feddef_ex=(IO.C(11,:)*IO.S*IO.L*diag(y_feddef_exp))';
MF_fednondef_ex=(IO.C(11,:)*IO.S*IO.L*diag(y_fednondef_exp))';
MF_slg_ex=(IO.C(11,:)*IO.S*IO.L*diag(y_slg_exp))';
MF_pr_inv=(IO.C(11,:)*IO.S*IO.L*diag(y_priv_inv))'; % MF from private investment
MF_feddef_inv=(IO.C(11,:)*IO.S*IO.L*diag(y_feddef_inv))'; % MF from fed def investment
MF_fednondef_inv=(IO.C(11,:)*IO.S*IO.L*diag(y_fednondef_inv))'; % MF from fed non-def investment
MF_slg_inv=(IO.C(11,:)*IO.S*IO.L*diag(y_slg_inv))'; % MF from state/local govt investment
%% concatenate total expenditure MF results
MF_exp=[MF_pr_ex MF_feddef_ex MF_fednondef_ex MF_slg_ex MF_pr_inv MF_feddef_inv MF_fednondef_inv MF_slg_inv]; % consumption and investment total MF, excl. capital inputs
MF_exp(:,size(MF_exp,2)+1)=sum(MF_exp,2); % total by commodity consumption/investment sector
MF_exp(:,size(MF_exp,2)+1)=1:size(MF_exp,1); % commodity numbers
MF_exp_sort=flipud(sortrows(MF_exp,size(MF_exp,2)-1)); % sort by largest consumption sectors
labsexMF=meta.ZLabs_short(MF_exp_sort(:,size(MF_exp,2)),2); % commodity sector labels in order of largest to smallest MF
%% MF of expenditure and capital consumption including capital
MF_pr_con_t=(IO.C(11,:)*IO.S*IO.Lt*diag(y_hh))'; % total MF of private consumption, incl. capital inputs
diff_MF_pr=MF_pr_con_t-MF_pr_ex; % increase in MF from capital inputs
MF_feddef_con_t=(IO.C(11,:)*IO.S*IO.Lt*diag(y_feddef_exp))'; % fed defense
diff_MF_fd=MF_feddef_con_t-MF_feddef_ex;
MF_fednondef_con_t=(IO.C(11,:)*IO.S*IO.Lt*diag(y_fednondef_exp))'; % fed non-defense
diff_MF_fnd=MF_fednondef_con_t-MF_fednondef_ex;
MF_slg_con_t=(IO.C(11,:)*IO.S*IO.Lt*diag(y_slg_exp))'; % sl govt
diff_MF_slg=MF_slg_con_t-MF_slg_ex;
%% concatenate expenditure and capital consumption MF results
MF_con=[MF_pr_ex MF_feddef_ex MF_fednondef_ex MF_slg_ex diff_MF_pr diff_MF_fd diff_MF_fnd diff_MF_slg]; % total consumption MF, incl capital
MF_con(:,size(MF_con,2)+1)=sum(MF_con,2); % sum by consumption commodidty sector
MF_con(:,size(MF_con,2)+1)=1:size(MF_con,1); % commodity number 
MF_con_Mt=1e-9*[sum(MF_con(:,1:4),2) sum(MF_con(:,5:8),2)];
MF_con_sort=flipud(sortrows(MF_con,size(MF_con,2)-1)); % sort by largest MF commodity
labsconMF=meta.ZLabs_short(MF_con_sort(:,size(MF_exp,2)),2); %  commodity sector labels in order of largest to smallest MF
%% Summary sector results expenditure/investment MF results - not publishing
order=fliplr(1:size(MF_exp,2)-2);
labels={'Pers Cons', 'Fed-def Cons', 'Fed-nondef Cons', 'SLG cons', 'Pers Inv','Fed-def Inv','Fed-nondef Inv', 'SLG inv'}; % legend labels
MF_sum_exp=[string(meta.ZLabs_short(:,1)) string(code2) MF_exp];
[MF_exp_inv, ZLab25] = sector_reduce(string(sector_label),MF_sum_exp,MF_exp); % aggregate results to summary sector 
MF_exp_inv_Mt=1e-9*MF_exp_inv; % convert to megaTon
f3=figure('pos',[1 50 720 540]);
q=bar(MF_exp_inv_Mt(:,1:8),'stack');
switch year
    case 2007
        title(['E. Material Footprint of consumption & capital investment, ' num2str(year)])
    case 2012
        title(['F. Material Footprint of consumption & capital investment, ' num2str(year)])
        u=legend(q(order),labels{order});
        set(u,'Fontsize',8.3)
end
colormap(cm);
ylabel('Mt material')
xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
%% Summary sector results consumption MF results - don't publish
labels={'Pers Cons', 'Fed-def Cons', 'Fed-nondef Cons', 'SLG cons', 'Pers CC','Fed-def CC','Fed-nondef CC', 'SLG CC'};
MF_sum_con=[string(meta.ZLabs_short(:,1)) string(code2) MF_con];
[MF_con_sum, ZLab25] = sector_reduce(string(sector_label),MF_sum_con,MF_con); % aggregate results to summary sector 
MF_con_sum_Mt=1e-9*MF_con_sum; % convert to megaTon
f4=figure('pos',[1 50 720 540]);
q5=bar(MF_con_sum_Mt(:,1:8),'stack');
switch year
    case 2007
        title(['G. Material Footprint of consumption incl. capital inputs, ' num2str(year)])
    case 2012
        title(['H. Material Footprint of consumption incl. capital inputs, ' num2str(year)])
        u=legend(q5(order),labels{order});
        set(u,'Fontsize',8.3)
end
colormap(cm);
ylabel('Mt material')
xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
%% summary sector MF results capital cons/inv only Figure 1
MK=zeros(size(MF_exp_inv_Mt,1)*3,8);
Lab=cell(size(MF_exp_inv_Mt,1)*3,1);
Lab(:)={''};
ylabels={'Private', 'Federal def', 'Federal non-def', 'State/local gov', 'Personal','Federal def', 'Federal non-def  ', 'State/local gov'};
for i=1:size(MF_exp_inv_Mt,1)
MK(((i-1)*3)+1,1:4)=MF_exp_inv_Mt(i,5:8);
MK(((i-1)*3)+2,5:8)=MF_con_sum_Mt(i,5:8);
Lab(((i-1)*3)+2)={ZLab25(i)};
end
MK(rm,:)=[]; % remov Delivery/Warehousing and Management

% vertical bar version
c=copper(64);
p=winter(64);
cm=zeros(8,3);
cm(1:4,:)=c(32:10:62,:);
cm(5:8,:)=p(32:10:62,:);
colormap(cm)

% with two legends
mk1=[MK(:,1:4), zeros(size(MK,1),4)];
mk2=[zeros(size(MK,1),4) MK(:,5:8)];
% graph investment and consumption impacts together
order4=fliplr(1:4);
f1c=figure('pos',[-1500 -830 1000 700]);
hAx(1) = axes();
q41=bar(mk1,1,'stack','Parent', hAx(1));
set(hAx(1), 'Box','on','Position',[.13 .25 .77 .68])
colormap(cm)
u1=legend(q41(order4),ylabels(order4),'Location','North','FontSize',9);
title(u1,'Capital Investment')
xlim([0 size(MK,1)+1])
ylim([0 1100])
xticks(1.5:3:70.5);
xticklabels(ZLab25([1:11 13:16 18:end]));
xtickangle(41)
title(['C. Material Footprint of capital investment and consumption, ' num2str(year)])
ylabel('Mt')
set(gca,'FontSize',12.5);
set(gca,'TickDir','both')
box off % remove outer ticks
%% second axis/legend
hAx(2) = copyobj(hAx(1),gcf);
q42=bar(mk2,1,'stack','Parent',hAx(2));
set(hAx(2), 'Color','none', 'XTick',[], 'YTick', [], ...
    'Box','on','GridColor','none')
colormap(cm)
u2=legend(q42(order(1:4)),ylabels(order(1:4)),'Location','NorthEast','FontSize', 9, 'Color','w');
u1.Position=u1.Position+[0 0 0.008 0];
u2.Position=u2.Position+[0 0 0.008 0];
u1.Position=u2.Position-[.15 0 0 0];
title(u2,'Capital Consumption  ')
hAx(2).YLim=hAx(1).YLim;
hAx(2).XLim=hAx(1).XLim;
hAx(2).YTickLabel=[];
hAx(2).Position=hAx(1).Position; % this may need to be repeated if/after resizing the figure
set(gca,'FontSize',12.5);
u2.FontSize=u1.FontSize;
%% Highest detail level MF consumption sectors, don't publish
order8=fliplr(1:8);
labels={'Pers Cons', 'Fed-def Cons', 'Fed-nondef Cons', 'SLG cons', 'Pers CC','Fed-def CC','Fed-nondef CC', 'SLG CC'};
f9=figure('pos',[1 50 720 540]);
MF_con_sort_Mt=1e-9*MF_con_sort; % convert from kg to Mt
q=bar(MF_con_sort_Mt(1:15,1:8),'stack');
switch year
    case 2007
        title(['C. Max Material Footprint of detailed consumption sectors, ' num2str(year)])
        orderm07=MF_con_sort(:,10);
    case 2012
        title(['D. Max Material Footprint of detailed consumption sectors, ' num2str(year)])
        u=legend(q(order8),labels{order8});
        set(u,'Fontsize',8.3)
        orderm12=MF_con_sort(:,10);
end
colormap(cm);
ylabel('Mt')
xticklabels(labsconMF(1:15));
xtickangle(45)
set(gca,'FontSize',11.5);
%% Highest detail level MF exp and inv sectors, don't publish
order8=fliplr(1:8);
labels={'Pers Cons', 'Fed-def Cons', 'Fed-nondef Cons', 'SLG cons', 'Pers Inv','Fed-def Inv','Fed-nondef Inv', 'SLG Inv'};
f10=figure('pos',[1 50 720 540]);
MF_exp_sort_Mt=1e-9*MF_exp_sort; % convert from kg to Mt
q=bar(MF_exp_sort_Mt(1:15,1:8),'stack');
title(['Detailed sectors with highest Material Footprint, expenditure and investment, ' num2str(year)])
colormap(cm);
u=legend(q(order8),labels{order8});
ylabel('Mt material')
xticklabels(labsexMF(1:15));
xtickangle(45)
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% private and public expenditure and investment EF in-year impacts 
EF_pr_ex=(IO.C(6,:)*IO.S*IO.L*diag(y_hh))'; % EF from private consumption expenditures, total materials
EF_feddef_ex=(IO.C(6,:)*IO.S*IO.L*diag(y_feddef_exp))';
EF_fednondef_ex=(IO.C(6,:)*IO.S*IO.L*diag(y_fednondef_exp))';
EF_slg_ex=(IO.C(6,:)*IO.S*IO.L*diag(y_slg_exp))';
EF_pr_inv=(IO.C(6,:)*IO.S*IO.L*diag(y_priv_inv))'; % EF from private investment
EF_feddef_inv=(IO.C(6,:)*IO.S*IO.L*diag(y_feddef_inv))'; % EF from fed def investment
EF_fednondef_inv=(IO.C(6,:)*IO.S*IO.L*diag(y_fednondef_inv))'; % EF from fed non-def investment
EF_slg_inv=(IO.C(6,:)*IO.S*IO.L*diag(y_slg_inv))'; % EF from state/local govt investment
%% concatenate total expenditure EF results
EF_exp=[EF_pr_ex EF_feddef_ex EF_fednondef_ex EF_slg_ex EF_pr_inv EF_feddef_inv EF_fednondef_inv EF_slg_inv]; % total consumption and investment expenditure EF 
EF_exp(:,size(EF_exp,2)+1)=sum(EF_exp,2); % sum by commodity type
EF_exp(:,size(EF_exp,2)+1)=1:size(EF_exp,1); % commodity number
EF_exp_sort=flipud(sortrows(EF_exp,size(EF_exp,2)-1)); % sort by largest EF commodities
labsexEF=meta.ZLabs_short(EF_exp_sort(:,size(EF_exp,2)),2); % commodity labels in order of highest EF
%% Summary sector results expenditure/investment EF results, don't publish
labels={'Pers Cons', 'Fed-def Cons', 'Fed-nondef Cons', 'SLG cons', 'Pers Inv','Fed-def Inv','Fed-nondef Inv', 'SLG inv'}; % legend labels
EF_sum_exp=[string(meta.ZLabs_short(:,1)) string(code2) EF_exp];
[EF_exp_inv, ZLab25] = sector_reduce(string(sector_label),EF_sum_exp,EF_exp); % aggregate impacts to summary level
f7=figure('pos',[1 50 720 540]);
q=bar(1e-12*EF_exp_inv(:,1:8),'stack'); % convert MJ to EJ
switch year
    case 2007
        title(['I. Energy Footprint, consumption & capital investment, ' num2str(year)])
    case 2012
        title(['J. Energy Footprint, consumption & capital investment, ' num2str(year)])
        u=legend(q(order8),labels{order8});
        set(u,'Fontsize',8.3)
end
colormap(cm);

ylabel('EJ')
xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
%% EF of expenditure and capital consumption including capital
EF_pr_con_t=(IO.C(6,:)*IO.S*IO.Lt*diag(y_hh))'; % EF from private consumption, including capital inputs
diff_EF_pr=EF_pr_con_t-EF_pr_ex; % increase in private EF from capital inputs
EF_feddef_con_t=(IO.C(6,:)*IO.S*IO.Lt*diag(y_feddef_exp))'; % fed defense
diff_EF_fd=EF_feddef_con_t-EF_feddef_ex;
EF_fednondef_con_t=(IO.C(6,:)*IO.S*IO.Lt*diag(y_fednondef_exp))'; % fed non-defense
diff_EF_fnd=EF_fednondef_con_t-EF_fednondef_ex;
EF_slg_con_t=(IO.C(6,:)*IO.S*IO.Lt*diag(y_slg_exp))'; % sl govt
diff_EF_slg=EF_slg_con_t-EF_slg_ex;
%% concatenate expenditure and capital consumption EF results
EF_con=[EF_pr_ex EF_feddef_ex EF_fednondef_ex EF_slg_ex diff_EF_pr diff_EF_fd diff_EF_fnd diff_EF_slg]; % total consumption EF including capital
EF_con(:,size(EF_con,2)+1)=sum(EF_con,2); % sum by consumption commodity
EF_con(:,size(EF_con,2)+1)=1:size(EF_con,1); % commodity number
EF_con_PJ=1e-9*[sum(EF_con(:,1:4),2) sum(EF_con(:,5:8),2)];
EF_con_sort=flipud(sortrows(EF_con,size(EF_con,2)-1)); % sort by largest EF commodities
labsconEF=meta.ZLabs_short(EF_con_sort(:,size(EF_con,2)),2); % labels in order of largest EF
%% Summary sector results consumption EF results, don't publish
labels={'Pers Cons', 'Fed-def Cons', 'Fed-nondef Cons', 'SLG cons', 'Pers CC','Fed-def CC','Fed-nondef CC', 'SLG CC'};
EF_sum_con=[string(meta.ZLabs_short(:,1)) string(code2) EF_con];
[EF_con_sum, ZLab25] = sector_reduce(string(sector_label),EF_sum_con,EF_con); % aggregate impacts to summary level
f8=figure('pos',[1 50 720 540]);
q5=bar(1e-12*EF_con_sum(:,1:8),'stack'); % convert MJ to EJ
switch year
    case 2007
        title(['K. Energy Footprint of consumption incl. capital inputs, ' num2str(year)])
    case 2012
        title(['L. Energy Footprint of consumption incl. capital inputs, ' num2str(year)])
        u=legend(q5(order8),labels{order8});
        set(u,'Fontsize',8.3)
end
colormap(cm);
ylabel('EJ')
xlim([0 26]);
xticks(1:25)
xticklabels(ZLab25)
xtickangle(45)
%% summary sector EF results capital cons/inv only 
EK=zeros(size(EF_exp_inv,1)*3,8);
Lab=cell(size(EF_exp_inv,1)*3,1);
Lab(:)={''};
ylabels={'Private', 'Federal def', 'Federal non-def', 'State/local gov', 'Personal','Federal def', 'Federal non-def  ', 'State/local gov'};
for i=1:size(EF_exp_inv,1)
EK(((i-1)*3)+1,1:4)=1e-12*EF_exp_inv(i,5:8);
EK(((i-1)*3)+2,5:8)=1e-12*EF_con_sum(i,5:8);
Lab(((i-1)*3)+2)={ZLab25(i)};
end
EK(rm,:)=[];
% vertical bar version
c=copper(64);
p=winter(64);
cm=zeros(8,3);
cm(1:4,:)=c(32:10:62,:);
cm(5:8,:)=p(32:10:62,:);
colormap(cm)
% with two legends
ek1=[EK(:,1:4), zeros(size(EK,1),4)];
ek2=[zeros(size(EK,1),4) EK(:,5:8)];
% graph investment and consumption impacts together
f1b=figure('pos',[-1500 -830 1000 700]);
hAx(1) = axes();
q51=bar(ek1,1,'stack','Parent', hAx(1));
set(hAx(1), 'Box','on','Position',[.13 .25 .77 .68])
colormap(cm)
u1=legend(q51(order4),ylabels(order4),'Location','North','FontSize',9);
title(u1,'Capital Investment')
xlim([0 size(EK,1)+1])

% ylim([0 600])
xticks(1.5:3:70.5);
xticklabels(ZLab25([1:11 13:16 18:end]));
xtickangle(41)
title(['B. Energy Footprint of capital investment and consumption, ' num2str(year)])
ylabel('EJ')
set(gca,'FontSize',12.5);
set(gca,'TickDir','both')
box off % remove outer ticks
%% second axis/legend
hAx(2) = copyobj(hAx(1),gcf);
q52=bar(ek2,1,'stack','Parent',hAx(2));
set(hAx(2), 'Color','none', 'XTick',[], 'YTick', [], ...
    'Box','on','GridColor','none')
colormap(cm)
u2=legend(q52(order(1:4)),ylabels(order(1:4)),'Location','NorthEast','FontSize', 9, 'Color','w');
u1.Position=u1.Position+[-0.01 0 0.015 0];
u2.Position=u2.Position+[-0.01 0 0.015 0];
u1.Position=u2.Position-[.15 0 0 0];
title(u2,'Capital Consumption')
hAx(2).YLim=hAx(1).YLim;
hAx(2).XLim=hAx(1).XLim;
hAx(2).YTickLabel=[];
hAx(2).Position=hAx(1).Position;
set(gca,'FontSize',12.5);
u2.FontSize=u1.FontSize;
%% Highest detail level EF consumption sectors, don't publish
labels={'Pers Cons', 'Fed-def Cons', 'Fed-nondef Cons', 'SLG cons', 'Pers CC','Fed-def CC','Fed-nondef CC', 'SLG CC'};
f11=figure('pos',[1 50 720 540]);
q=bar(1e-12*EF_con_sort(1:15,1:8),'stack'); % convert MJ to EJ
switch year
    case 2007
        title(['E. Max Energy Footprint of detailed consumption sectors, ' num2str(year)])
        ordere07=EF_con_sort(:,10);
    case 2012
        title(['F. Max Energy Footprint of detailed consumption sectors, ' num2str(year)])
        u=legend(q(order8),labels{order8});
        set(u,'Fontsize',8.3)
        ordere12=EF_con_sort(:,10);
end
colormap(cm);
ylabel('EJ')
xticklabels(labsconEF(1:15));
xtickangle(45)
set(gca,'FontSize',11.5);
%% Highest detail level EF exp and inv sectors, don't publish
labels={'Pers Cons', 'Fed-def Cons', 'Fed-nondef Cons', 'SLG cons', 'Pers Inv','Fed-def Inv','Fed-nondef Inv', 'SLG inv'}; % legend labels
f12=figure('pos',[1 50 720 540]);
q=bar(1e-6*EF_exp_sort(1:15,1:8),'stack'); % convert MJ to TJ
title(['Detailed sectors with highest Energy Footprint, expenditure and investment, ' num2str(year)])
colormap(cm);
u=legend(q(order8),labels{order8});
ylabel('TJ')
xticklabels(labsexEF(1:15));
xtickangle(45)
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% Detailed highest consumers and producers of CF impacts (2-way Contribution analysis results)
Ds=D_GHGt; % total GHG impacts from prod and cons
Ds(:,size(Ds,2)+1)=sum(Ds,2); % sum from prod. sectors
Ds(:,size(Ds,2)+1)=1:408; % number sectors in original order
DS=flipud(sortrows(Ds,409)); % sort by largest producers
D_pro_sec=DS(1:15,410); % the sector numbers of highest producers
DsT=DS'; % transpose
DsT(409:410,:)=[]; % remove the prod. totals and sector numbers
DsT(:,409)=sum(DsT,2); % sum from cons. sectors
DsT(:,410)=1:408; % original sector numbers
DsTs=flipud(sortrows(DsT,409)); % sort by largest consumers
D_con_sec=DsTs(1:15,410); % the sector numbers of highest consumers
%% Manually extract production sectors of interest
% main_pr=[22 14 281 8 23 88 340 123 7]; % define sectors of interest from prod. perspective, informed by sumps
sump=sum(D_GHGt,2); % sum of production emissions, changed from M to D
sump(:,2)=1:408; % sec numbers
sumps=flipud(sortrows(sump,1)); % sort sump
sumps1=sumps;
% main_pr=[22 14 53 294 2 43 7 8 15]; % define sectors of interest from prod. perspective, informed by sumps
main_pr=sumps(1:11,2)';
main_pr(main_pr==408)=[];
main_pr(main_pr==406)=[];

m=size(main_pr,2); % number of interesting prod. sectors 
other=1:408; 
other(main_pr)=[]; % remove sectors of interest from list of all sector numbers
Dn=[D_GHGt(main_pr,:); D_GHGt(other,:)]; % Order production sectors by interesting, other (not in order of highest impacts)
DnT=Dn'; % transpose - put production sectors on columns, consumption on rows
DnT(:,409)=sum(DnT,2); % total consumption sectors
DnT(:,410)=1:408; % sector number of consumption sectors
DnTs=flipud(sortrows(DnT,409)); % sort by largest consumption sectors
Dn_con_sec=DnTs(1:15,410); % get sector numbers of 15 largest consumption sectors
DnTs(:,409:410)=[]; % remove totals and sector numbers
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
otr2c1=DnTs(16:408,1:m); % inputs to other consumption sectors, from extraction prod sectors
otr2c2=DnTs(16:408,m+1:408); % inputs to other cons sectors, from other prod sectors
D_con_sec_r=DsTs(16:end,410); % sector numbers of highest consumption impact sectors outside top 15, i.e. 16:392
contr2=[sum(otr2c1,1) sum(sum(otr2c2))]; % contr of int prod sectors to other cons sectors; contr of other prod sectors to other cons sectors
csr2=sum(contr2); % total 'other' impacts
dir=zeros(1,408-15); % define vector to store direct impacts from other sectors
for i=1:393 % production inputs to consumption impacts 16:392 (in order of highest impacts)
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
xlab=[meta.ZLabs_short(D_con_sec,2);{'Other'}];
xticks(1:16);
xticklabels(xlab)
xtickangle(45)
labels=[meta.ZLabs_short(main_pr,2) ;{'Own*'};{'Other'}];
title(['Contribution from production sectors to CF of consumption, ' num2str(year)])
u=legend(q(order),labels{order},'Location','northeastoutside');
title(u,'Production sectors')
colormap(jet(m+12))
ylim([0 1])
xlabel('\bfConsumption sectors')
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% PURCHASERS PRICES Detailed highest consumers and producers of CF impacts (2-way Contribution analysis results)
Ds=D_GHG_t_purch; % total GHG impacts from prod and cons
Ds(:,size(Ds,2)+1)=sum(Ds,2); % sum from prod. sectors
Ds(:,size(Ds,2)+1)=1:408; % number sectors in original order
DS=flipud(sortrows(Ds,401)); % sort by largest producers
D_pro_sec=DS(1:15,402); % the sector numbers of highest producers
DsT=DS'; % transpose
DsT(401:402,:)=[]; % remove the prod. totals and sector numbers
DsT(:,409)=sum(DsT,2); % sum from cons. sectors
DsT(:,410)=1:400; % original sector numbers
DsTs=flipud(sortrows(DsT,409)); % sort by largest consumers
D_con_sec=DsTs(1:15,410); % the sector numbers of highest consumers
%% PURCHASERS PRICES Manually extract production sectors of interest
% main_pr=[22 14 281 8 23 88 340 123 7]; % define sectors of interest from prod. perspective, informed by sumps
sump=sum(D_GHG_t_purch,2); % sum of production emissions, changed from M to D
sump(:,2)=1:408; % sec numbers
sumps=flipud(sortrows(sump,1)); % sort sump
% main_pr=[22 14 53 294 2 43 7 8 15]; % define sectors of interest from prod. perspective, informed by sumps
main_pr=sumps(1:11,2)';
main_pr(main_pr==408)=[];
main_pr(main_pr==406)=[];
m=size(main_pr,2); % number of interesting prod. sectors 
other=1:408; 
other(main_pr)=[]; % remove sectors of interest from list of all sector numbers
Dn=[D_GHG_t_purch(main_pr,:); D_GHG_t_purch(other,:)]; % Order production sectors by interesting, other (not in order of highest impacts)
DnT=Dn'; % transpose - put production sectors on columns, consumption on rows
DnT(:,409)=sum(DnT,2); % total consumption sectors
DnT(:,410)=1:400; % sector number of consumption sectors
DnTs=flipud(sortrows(DnT,409)); % sort by largest consumption sectors
Dn_con_sec=DnTs(1:15,410); % get sector numbers of 15 largest consumption sectors
DnTs(:,409:410)=[]; % remove totals and sector numbers
ot=sum(DnTs(1:15,m+1:end),2); % the 'other' production impacts from the highest consumption sectors, not explained by the extracted production sectors
cont=[DnTs(1:15,1:m) ot]; % matrix of contributions from prod. sectors of interest, and other production sectors
cs=sum(cont,2); % row sums, total of consumption sectors
c=diag(cs)\cont; % get pc of contributions from prod. sectors to cons. sectors impacts
c(:,m+3)=c(:,m+1); % cont from 'other' prod. sectors incl. direct, save numbers here
c(:,m+(1:2))=zeros(15,2); % make space in columns m+1, m+2
for i = 1:15 % fill m+1 column with Direct impacts
    if D_con_sec(i)<282
        c(i,m+1)=D_GHG_t_purch(D_con_sec(i),D_con_sec(i))/cs(i); 
    elseif D_con_sec(i)==282
        c(i,m+1)=D_GHG_t_purch(D_con_sec(i)+7,D_con_sec(i))/cs(i); 
    elseif D_con_sec(i)>282
        c(i,m+1)=D_GHG_t_purch(D_con_sec(i)+8,D_con_sec(i))/cs(i); 
    end
if sum(c(i,1:m+1))>1% if direct were already included, make zero, avoid double counting
    c(i,m+1)=0;
end
c(i,m+2)=c(i,m+3)-c(i,m+1); % contr. from 'other' prod. not incl. Direct
end
c(:,m+3)=[]; % remove 'other' + Direct impacts column
order=fliplr(1:m+2);
%% PURCHASERS PRICES contribution to 'other' consumption sectors
otr2c1=DnTs(16:end,1:m); % inputs to other consumption sectors, from extraction prod sectors
otr2c2=DnTs(16:end,m+1:408); % inputs to other cons sectors, from other prod sectors
D_con_sec_r=DsTs(16:end,410); % sector numbers of highest consumption impact sectors outside top 15, i.e. 16:392
contr2=[sum(otr2c1,1) sum(sum(otr2c2))]; % contr of int prod sectors to other cons sectors; contr of other prod sectors to other cons sectors
csr2=sum(contr2); % total 'other' impacts
dir=zeros(1,400-15); % define vector to store direct impacts from other sectors
for i=1:385 % production inputs to consumption impacts 16:400 (in order of highest impacts)
    if D_con_sec_r(i)<282
        dir(i)=D_GHG_t_purch(D_con_sec_r(i),D_con_sec_r(i))/csr2; % each sectors cont. to overall other impacts
    elseif D_con_sec_r(i)==282  
        dir(i)=D_GHG_t_purch(D_con_sec_r(i)+7,D_con_sec_r(i))/csr2; % each sectors cont. to overall other impacts
    elseif D_con_sec_r(i)>282
        dir(i)=D_GHG_t_purch(D_con_sec_r(i)+8,D_con_sec_r(i))/csr2; % each sectors cont. to overall other impacts
    end
end
contr2n=contr2/csr2; % make vector of contribution pc to 'other' consumption sector
contr2n(m+3)=contr2n(m+1); % move contr from 'other' prod sectors incl. direct to new column
contr2n(m+1)=sum(dir); % include direct impacts
contr2n(m+2)=contr2n(m+3)-contr2n(m+1); % 'other' impacts not incl. direct (self) impacts
contr2n(m+3)=[]; % remove other + direct column
ctot=[c;contr2n]; % total contribution from to top consumption sectors, other consumption sectors
f13=figure;
q=bar(ctot,'stack');
xlab=[meta.ZLabspur(D_con_sec,3);{'Other'}];
xticks(1:16);
xticklabels(xlab)
xtickangle(45)
labels=[meta.ZLabs_short(main_pr,2) ;{'Own*'};{'Other'}];
title(['Contribution from production sectors to CF of consumption, purch. price ' num2str(year)])
u=legend(q(order),labels{order},'Location','northeastoutside');
title(u,'Production sectors')
colormap(jet(m+12))
ylim([0 1])
xlabel('\bfConsumption sectors')
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% cont to capital related CF (similar 2-way contribution analysis) 
sumpk=sum(D_GHGdiff,2); % sum of production emissions for capital consumption
sumpk(:,2)=1:408; % sec numbers
sumpks=flipud(sortrows(sumpk,1)); % sort sumpk
k_pr=[sumpks(1:8,2)' 30 31 35 36]; % most important production sectors for K assets
mk=size(k_pr,2); % number of production sectors analysed
otherk=1:408; 
otherk(k_pr)=[]; % remove numbers of important prod sectors
Ddk=[D_GHGdiff(k_pr,:); D_GHGdiff(otherk,:)]; % put important prod sectors to highest rows
DkT=Ddk'; % Transpose, put consumption on rows, production on columns
DkT(:,409)=sum(DkT,2); % total of consum sectors
DkT(:,410)=1:408; % sec num of cons sectors
DkTs=flipud(sortrows(DkT,409)); % sort by highest cap. consumption sectors
Dk_con_sec=DkTs(1:12,410); % sector numbers of highest cap consumption 
Dk_con_sec_r=DkTs(13:end,410); % sec numbers of rest of cap consumption
DkTs(:,409:410)=[]; % remove totals and sec numbers
otk=sum(DkTs(1:12,mk+1:end),2); % inputs to highest cap cons sectors from 'other' prod sectors
contk=[DkTs(1:12,1:mk) otk];  % matrix of contributions from prod. sectors of interest, and other production sectors
csk=sum(contk,2);  % row sums, total of consumption sectors
ck=diag(csk)\contk; % pc contributions from prod. sectors to cons. sectors impacts
% don't include 'Direct' impacts for capital contribution analysis
otr2ck1=DkTs(13:408,1:mk);
otr2ck2=DkTs(13:408,mk+1:408);
contrk2=[sum(otr2ck1,1) sum(sum(otr2ck2))];
csrk2=sum(contrk2);
contrk2n=contrk2/csrk2;
cktot=[ck;contrk2n];
cktot_reduce=[cktot(:,1:8) sum(cktot(:,9:11),2) cktot(:,12:13)];

f14=figure;
order=fliplr(1:mk-1);
q=bar(cktot_reduce,'stack');
xlab=[meta.ZLabs_short(Dk_con_sec,2);{'Other'}];
xticks(1:13);
xticklabels(xlab)
xtickangle(45)
labels=[meta.ZLabs_short(k_pr(1:8),2); {'Residential construction'};  meta.ZLabs_short(k_pr(12),2); {'Other'}];
% labels=[meta.ZLabs_short(k_pr(1:9),2); {'Other'}];
title(['B. Contribution of production sectors to CF of capital consumption, ' num2str(year)])
u=legend(q(order),labels{order},'Location','northeastoutside');
title(u,'Production sectors')
colormap(jet(m+12))
ylim([0 1])
xlabel('\bfConsumption sectors')
ax=gca;
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% capital contribution from assets, hypothetical extraction
A=zeros(816);
A(1:408,1:408)=IO.A;
A(409:end,1:408)=IO.Ak;
A(409:end,409:end)=(IO.A+IO.Ak);
A0=A;
I=eye(816);
L=inv(I-A);

y=zeros(816,1);
y(1:408)=y_exp;
x=L*y;
X=L*diag(y);
GHGk=diag(IO.C(2,:)*IO.S)*X(409:end,1:408);
%% hyp extraction
asset_cont=zeros(9,13);
tot_asset_cont=zeros(9,1);
mine=[14:21]+408;
A=A0; 
A(mine,1:408)=0;
L=inv(I-A);
X=L*diag(y);
GHGk_he=diag(IO.C(2,:)*IO.S)*X(409:end,1:408); 
for i = 1:12
    asset_cont(1,i)=1-(sum(GHGk_he(:,Dk_con_sec(i)))/ sum(GHGk(:,Dk_con_sec(i)))); % mining
end
asset_cont(1,13)=1-sum(sum(GHGk_he(:,Dk_con_sec_r)))/sum(sum(GHGk(:,Dk_con_sec_r))); % mining inputs to all other
tot_asset_cont(1)=1-sum(GHGk_he)/sum(GHGk);
res=[30,31,35]+408;
A=A0; 
A(res,1:408)=0;
L=inv(I-A);
X=L*diag(y);
GHGk_he=diag(IO.C(2,:)*IO.S)*X(409:end,1:408); 
for i = 1:12
    asset_cont(2,i)=1-sum(GHGk_he(:,Dk_con_sec(i)))/ sum(GHGk(:,Dk_con_sec(i))); % residential structures
end
asset_cont(2,13)=1-sum(sum(GHGk_he(:,Dk_con_sec_r)))/sum(sum(GHGk(:,Dk_con_sec_r))); % res inputs to all other
tot_asset_cont(2)=1-sum(GHGk_he)/sum(GHGk);
nonres=[25:29, 32:34, 36]+408;
A=A0; 
A(nonres,1:408)=0;
L=inv(I-A);
X=L*diag(y);
GHGk_he=diag(IO.C(2,:)*IO.S)*X(409:end,1:408); 
for i = 1:12
    asset_cont(3,i)=1-sum(GHGk_he(:,Dk_con_sec(i)))/ sum(GHGk(:,Dk_con_sec(i))); % nonres strucures
end
asset_cont(3,13)=1-sum(sum(GHGk_he(:,Dk_con_sec_r)))/sum(sum(GHGk(:,Dk_con_sec_r))); % nonres inputs to all other
tot_asset_cont(3)=1-sum(GHGk_he)/sum(GHGk);
mvm=[53:190]+408;
A=A0; 
A(mvm,1:408)=0;
L=inv(I-A);
X=L*diag(y);
GHGk_he=diag(IO.C(2,:)*IO.S)*X(409:end,1:408); 
for i = 1:12
    asset_cont(4,i)=1-sum(GHGk_he(:,Dk_con_sec(i)))/ sum(GHGk(:,Dk_con_sec(i))); % metal, vehicles machinery
end
asset_cont(4,13)=1-sum(sum(GHGk_he(:,Dk_con_sec_r)))/sum(sum(GHGk(:,Dk_con_sec_r))); % mvm inputs to all other
tot_asset_cont(4)=1-sum(GHGk_he)/sum(GHGk);
bcm_text=[37:52,191:227,228:270]+408;
A=A0; 
A(bcm_text,1:408)=0;
L=inv(I-A);
X=L*diag(y);
GHGk_he=diag(IO.C(2,:)*IO.S)*X(409:end,1:408); 
for i = 1:12
    asset_cont(5,i)=1-sum(GHGk_he(:,Dk_con_sec(i)))/ sum(GHGk(:,Dk_con_sec(i))); % bio chem minerals textiles
end
asset_cont(5,13)=1-sum(sum(GHGk_he(:,Dk_con_sec_r)))/sum(sum(GHGk(:,Dk_con_sec_r))); % bcmt inputs to all other
tot_asset_cont(5)=1-sum(GHGk_he)/sum(GHGk);
mar=[271:299]+408;
A=A0; 
A(mar,1:408)=0;
L=inv(I-A);
X=L*diag(y);
GHGk_he=diag(IO.C(2,:)*IO.S)*X(409:end,1:408); 
for i = 1:12
    asset_cont(6,i)=1-sum(GHGk_he(:,Dk_con_sec(i)))/ sum(GHGk(:,Dk_con_sec(i))); % margins
end
asset_cont(6,13)=1-sum(sum(GHGk_he(:,Dk_con_sec_r)))/sum(sum(GHGk(:,Dk_con_sec_r))); % margins inputs to all other
tot_asset_cont(6)=1-sum(GHGk_he)/sum(GHGk);
infart=[300:322,370:377]+408;
A=A0; 
A(infart,1:408)=0;
L=inv(I-A);
X=L*diag(y);
GHGk_he=diag(IO.C(2,:)*IO.S)*X(409:end,1:408); 
for i = 1:12
    asset_cont(7,i)=1-sum(GHGk_he(:,Dk_con_sec(i)))/ sum(GHGk(:,Dk_con_sec(i))); % information art industries
end
asset_cont(7,13)=1-sum(sum(GHGk_he(:,Dk_con_sec_r)))/sum(sum(GHGk(:,Dk_con_sec_r))); % information inputs to all other
tot_asset_cont(7)=1-sum(GHGk_he)/sum(GHGk);
re=[323:329]+408;
A=A0; 
A(re,1:408)=0;
L=inv(I-A);
X=L*diag(y);
GHGk_he=diag(IO.C(2,:)*IO.S)*X(409:end,1:408); 
for i = 1:12
    asset_cont(8,i)=1-sum(GHGk_he(:,Dk_con_sec(i)))/ sum(GHGk(:,Dk_con_sec(i))); % real estate 
end
asset_cont(8,13)=1-sum(sum(GHGk_he(:,Dk_con_sec_r)))/sum(sum(GHGk(:,Dk_con_sec_r))); % re inputs to all other
tot_asset_cont(8)=1-sum(GHGk_he)/sum(GHGk);
rnd=[330:343]+408;
A=A0; 
A(rnd,1:408)=0;
L=inv(I-A);
X=L*diag(y);
GHGk_he=diag(IO.C(2,:)*IO.S)*X(409:end,1:408); 
for i = 1:12
    asset_cont(9,i)=1-sum(GHGk_he(:,Dk_con_sec(i)))/ sum(GHGk(:,Dk_con_sec(i))); % rnd
end
asset_cont(9,13)=1-sum(sum(GHGk_he(:,Dk_con_sec_r)))/sum(sum(GHGk(:,Dk_con_sec_r))); % rnd inputs to all other
tot_asset_cont(9)=1-sum(GHGk_he)/sum(GHGk);
%%
f14b=figure;
order=fliplr(1:size(asset_cont,1));
q=bar(asset_cont','stack');
xlab=[meta.ZLabs_short(Dk_con_sec,2);{'Other'}];
xticks(1:13);
xticklabels(xlab)
xtickangle(45)
labels=[sector_label([2 4 5 8],2); {'Other durables'}; {'Trade, Transport'}; {'Information, Entertainment'};
    {'Real Estate'};sector_label(16,2)];
title(['A. Contribution of asset types to CF of capital consumption, ' num2str(year)])
u=legend(q(order),labels{order},'Location','northeastoutside');
title(u,'Asset types')
colormap(jet(m+12))
ylim([0 1])
xlabel('\bfConsumption sectors')
ax=gca;
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% total impacts
dt_con=IO.C*IO.S*IO.Lt*y_exp;
dt_expinv=IO.C*IO.S*IO.L*y_exp_inv;
%% Impact intensities in 2012 USD
M_int_At=(IO.C(11,:)*IO.S*IO.Lt)';
M_int_A=(IO.C(11,:)*IO.S*IO.L)';
M_int_At_purch=(IO.C(11,:)*IO.S*IO.Lt*IO.T)';
M_int_At_purch_408=[M_int_At_purch(1:281); zeros(7,1); M_int_At_purch(282); 0; M_int_At_purch(283:400)];
M_int_A_purch=(IO.C(11,:)*IO.S*IO.L*IO.T)';
M_int_A_purch_408=[M_int_A_purch(1:281); zeros(7,1); M_int_A_purch(282); 0; M_int_A_purch(283:400)];

E_int_At=(IO.C(6,:)*IO.S*IO.Lt)';
E_int_A=(IO.C(6,:)*IO.S*IO.L)';
E_int_At_purch=(IO.C(6,:)*IO.S*IO.Lt*IO.T)';
E_int_At_purch_408=[E_int_At_purch(1:281); zeros(7,1); E_int_At_purch(282); 0; E_int_At_purch(283:400)];
E_int_A_purch=(IO.C(6,:)*IO.S*IO.L*IO.T)';
E_int_A_purch_408=[E_int_A_purch(1:281); zeros(7,1); E_int_A_purch(282); 0; E_int_A_purch(283:400)];

if isequal(year,2007) % add in intensities per 2012USD
    GHG_int_pr07=GHG_int_pr;
    GHG_int_pr=(IO.C(2,:)*IO.S2012USD)'; % carbon intensity of production
    GHG_int_Atot07=GHG_int_Atot;
    GHG_int_Atot=(IO.C(2,:)*IO.S2012USD*IO.Lt)'; % carbon intensity of consumption, including capital
    GHG_int_con07=GHG_int_con;
    GHG_int_con=(IO.C(2,:)*IO.S2012USD*IO.L)'; % carbon intensity of cons., excluding capital
    M_int_At07=M_int_At;
    M_int_At=(IO.C(11,:)*IO.S2012USD*IO.Lt)'; % Material intensity of consumption, incl. capital
    M_int_A07=M_int_A;
    M_int_A=(IO.C(11,:)*IO.S2012USD*IO.L)'; % Material intensity of consumption, excl. capital
    E_int_At07=E_int_At;
    E_int_At=(IO.C(6,:)*IO.S2012USD*IO.Lt)'; % Energy intensity of consumption, incl. capital
    E_int_A07=E_int_A;
    E_int_A=(IO.C(6,:)*IO.S2012USD*IO.L)'; % Energy intensity of consumption, excl. capital
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
row=[1:408]';

Gabs_inc=diff_pr+diff_fd+diff_fnd+diff_slg; % total absolute increase in GHG emissions with capital
GHG_pc_inc=Gabs_inc./(d_pr_ex+d_gov_ex); % sectoral percent increase in GHG emissions with capital
GHG_pc_inc(isnan(GHG_pc_inc))=0; 
GHG_pc_inc(isinf(GHG_pc_inc))=0;
GHG_dir_pc=GHG_int_pr./GHG_int_Atot; % proportion of total impacts coming from direct impacts 

Mabs_inc=diff_MF_pr+diff_MF_fd+diff_MF_fnd+diff_MF_slg; % total absolute increase in MF with capital
Eabs_inc=diff_EF_pr+diff_EF_fd+diff_EF_fnd+diff_EF_slg; % total absolute increase in EF with capital
int=table(row,GHG_int_pr,GHG_int_con,GHG_int_Atot,GHG_dir_pc,GHGi_inc,GHGi_cc,GHG_int_Atot_purch_408,M_int_A,M_int_At,Mi_inc,E_int_A,E_int_At,Ei_inc,Gabs_inc,GHG_pc_inc,Mabs_inc,Eabs_inc,'RowNames',meta.ZLabs_short(:,2)); % table of intensities and changes

GHG_pc_inc_C=sum(Gabs_inc)/sum((d_pr_ex+d_feddef_ex+d_fednondef_ex+d_slg_ex))% economy wide percent increase in GHG emissions with capital
GHG_pc_ofT_C=sum(Gabs_inc)/(sum(Gabs_inc)+sum(d_pr_ex+d_feddef_ex+d_fednondef_ex+d_slg_ex)) % economy wide capital percent of total CF
M_pc_inc_C=sum(Mabs_inc)/sum((MF_pr_ex+MF_feddef_ex+MF_fednondef_ex+MF_slg_ex)) % economy wide percent increase in MF with capital
M_pc_ofT_C=sum(Mabs_inc)/(sum(Mabs_inc)+sum(MF_pr_ex+MF_feddef_ex+MF_fednondef_ex+MF_slg_ex)) % economy wide capital percent of total MF
E_pc_inc_C=sum(Eabs_inc)/sum((EF_pr_ex+EF_feddef_ex+EF_fednondef_ex+EF_slg_ex)) % economy wide percent increase in EF
E_pc_ofT_C=sum(Eabs_inc)/(sum(Eabs_inc)+sum(EF_pr_ex+EF_feddef_ex+EF_fednondef_ex+EF_slg_ex)) % economy wide capital percent of total EF

GHG_pc_inc_I=sum(d_pr_inv+d_feddef_inv+d_fednondef_inv+d_slg_inv)/sum((d_pr_ex+d_feddef_ex+d_fednondef_ex+d_slg_ex)); % economy wide % increase in GHG emissions with investment
GHG_pc_ofT_I=sum(d_pr_inv+d_feddef_inv+d_fednondef_inv+d_slg_inv)/(sum(d_pr_inv+d_feddef_inv+d_fednondef_inv+d_slg_inv)+sum(d_pr_ex+d_feddef_ex+d_fednondef_ex+d_slg_ex))% economy wide investment % of total CF
M_pc_inc_I=sum(MF_pr_inv+MF_feddef_inv+MF_fednondef_inv+MF_slg_inv)/(sum(MF_pr_ex+MF_feddef_ex+MF_fednondef_ex+MF_slg_ex)); % economy wide % increase in MF with investment
M_pc_ofT_I=sum(MF_pr_inv+MF_feddef_inv+MF_fednondef_inv+MF_slg_inv)/(sum(MF_pr_inv+MF_feddef_inv+MF_fednondef_inv+MF_slg_inv)+sum(MF_pr_ex+MF_feddef_ex+MF_fednondef_ex+MF_slg_ex)) % economy wide investment % of total MF
E_pc_inc_I=sum(EF_pr_inv+EF_feddef_inv+EF_fednondef_inv+EF_slg_inv)/(sum(EF_pr_ex+EF_feddef_ex+EF_fednondef_ex+EF_slg_ex)); % economy wide % increase in EF with investment
E_pc_ofT_I=sum(EF_pr_inv+EF_feddef_inv+EF_fednondef_inv+EF_slg_inv)/(sum(EF_pr_inv+EF_feddef_inv+EF_fednondef_inv+EF_slg_inv)+sum(EF_pr_ex+EF_feddef_ex+EF_fednondef_ex+EF_slg_ex)) % economy wide investment % of total EF
%% Extra plots with 2007 scatter on 2012 stacked bars
if isequal(year,2012)
load('Results_2007.mat') % to compare 2007 and 2012 impact on one chart, not sure if i'll do this for the revision
R07.s=sum(R07.sum_exp_inv,2);
order= [5 4 3 2 1];
labels={'2007 total','Pri. Consumption', 'Gov. Consumption', 'Pri. Investment','Gov. Investment'};
f15=figure('pos',[1 50 720 540]);
q=bar(sum_exp_inv_Mt,'stack');
hold on
r=scatter(1:size(R07.s,1),1e-9*R07.s,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert kg to Mt
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
r=scatter(1:size(R07.s,1),1e-9*R07.sc,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert kg to Mt
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
r=scatter(1:size(R07.s,1),1e-9*R07.m,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert kg to Mt
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
r=scatter(1:size(R07.s,1),1e-9*R07.mc,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert kg to Mt
title(['F. Material Footprint of consumption incl. capital inputs, ' num2str(year)])
u=legend([q(order(2:end)) r],labels{order});
set(u,'Fontsize',9)
colormap(cm);

ylabel('Mt material')
ylim([0 2400]); xlim([0 27]);
xticks(1:size(R07.s,1))
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
r=scatter(1:size(R07.s,1),1e-12*R07.e,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert MJ to EJ
title(['C. Energy Footprint of consumption & capital investment, ' num2str(year)])
u=legend([q(order(2:end)) r],labels{order});
set(u,'Fontsize',9)
colormap(cm);

ylabel('EJ')
ylim([0 25]); xlim([0 27]);
xticks(1:size(R07.s,1))
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
r=scatter(1:size(R07.s,1),1e-12*R07.ec,23,'filled','MarkerFaceColor','g','MarkerEdgeColor','k'); % 2007 totals, convert MJ to EJ
title(['D. Energy Footprint of consumption incl. capital inputs, ' num2str(year)])
u=legend([q(order(2:end)) r],labels{order});
set(u,'Fontsize',9)
colormap(cm);

ylabel('EJ')
ylim([0 27]); xlim([0 27]);
xticks(1:size(R07.s,1))
xticklabels(ZLab25)
xtickangle(45)
set(gca,'FontSize',12.5);
set(gca,'TitleFontSizeMultiplier',1.06);
hold off

f21=figure;
scatter(GHG_int_Atot*1e-6,GHG_int_Atot_purch_408*1e-6,50,'.k')
pbaspect([1 1 1])
title(['Carbon multipliers in Purchaser vs Producer prices, ' num2str(year)])
ylabel('kgCO2eq/2012USD Purchasers prices')
xlabel('kgCO2eq/2012USD Producers prices')
set(gca,'FontSize',12.5);
set(gca,'TitleFontSizeMultiplier',1.06);
xlim([0 5.5])
ylim([0 5.5])
box on

figure
scatter(R07.int.GHG_int_Atot*1e-6,GHG_int_Atot*1e-6,50,'.k')
pbaspect([1 1 1])
title(['Carbon multipliers in 2012 vs 2007'])
ylabel('2012 CM kgCO2eq/2012USD')
xlabel('2007 CM kgCO2eq/2012USD')
set(gca,'FontSize',12.5);
set(gca,'TitleFontSizeMultiplier',1.06);
hold on
line([0:8],[0:8],'Color','k');
hold off
box on
%% Create stacked group bar charts, detail level, 2007 and 2012
% CF
y1=zeros(2,15,2);
y1(1,:,:)=R07.sum_con_sort_Mt(1:15,1:2); % 2007 impacts
yr=sum_con(R07.order07,:); % 2012 impacts, use 2007 highest CF consumption sector order
yr=1e-9*[sum(yr(:,1:4),2) sum(yr(:,5:8),2)]; % convert kg to Mt
y1(2,:,:)=yr(1:15,1:2); % 2012 impacts
y2=permute(y1,[2,1,3]); % rearrange 3D array
y3(:,:,1)=y2(:,:,2);
y3(:,:,2)=y2(:,:,1); % put capital on bottom

label=R07.labsconCF07(1:15);
order=[2 1];
labels={'Capital Consumption','Consumption'};
% using plotBarStackGroups function, figure is defined in function
q=plotBarStackGroups(y3,label);
xtickangle(45)
u=legend(q(1,order),labels{order});
title('A. Detailed sectors with highest Carbon Footprint 2007, 2012')
ylabel('Mt CO2eq')
set(gca,'FontSize',11.5);
cm=[0 0.4 1; 0.8 0.1 0.1];
colormap(cm);
box on
%% sensitivity with 2007 intensity for capital layer
% calculate diff in 2007 intensites for 2012 capital consumption
Ldiff=IO.Lt-IO.L;
diff_all=(IO.C(2,:)*R07.S2012USD*Ldiff*diag(y_exp))';

y1=zeros(2,15,2);
y1(1,:,1)=sum_con_sort_Mt(1:15,1); % 2012 impacts from consumption only (no capital);
y1(1,:,2)=1e-9*diff_all(order12(1:15)); % 2012 impacts from consumption only (no capital);
yr=sum_con(order12,:); % 2012 impacts, use 2007 highest CF consumption sector order
yr=1e-9*[sum(yr(:,1:4),2) sum(yr(:,5:8),2)]; % convert kg to Mt
y1(2,:,:)=yr(1:15,1:2); % 2012 impacts
y2=permute(y1,[2,1,3]); % rearrange 3D array
y3(:,:,1)=y2(:,:,2);
y3(:,:,2)=y2(:,:,1); % put capital on bottom

label=labsconCF(1:15);
order=[2 1];
labels={'Capital Consumption','Consumption'};
% using plotBarStackGroups function, figure is defined in function
q=plotBarStackGroups(y3,label);
xtickangle(45)
u=legend(q(1,order),labels{order});
title('A. Detailed sectors with highest Carbon Footprint 2012')
ylabel('Mt CO2eq')
set(gca,'FontSize',11.5);
cm=[0 0.4 1; 0.8 0.1 0.1];
colormap(cm);
box on
% differences of capital layer intensity
pfuel=(y3(1,1,1)+y3(1,1,2))/(y3(1,2,1)+y3(1,2,2));
oohous=(y3(8,1,1)+y3(8,1,2))/(y3(8,2,1)+y3(8,2,2));
con_tot=sum(sum_con_sort_Mt(:,1));
cap12_tot=sum(sum_con_sort_Mt(:,2));
cap07_tot=sum(diff_all)*1e-9;
cap_diff=cap07_tot/cap12_tot;
tot_diff=(con_tot+cap07_tot)/(con_tot+cap12_tot);
%% EF
y1=zeros(2,15,2);
y1(1,:,:)=[sum(R07.EF_con_sort(1:15,1:4),2) sum(R07.EF_con_sort(1:15,5:8),2)]; % 2007 impacts
yr=EF_con(R07.ordere07,:); % 2012 impacts, use 2007 highest EF consumption sector order
yr=[sum(yr(:,1:4),2) sum(yr(:,5:8),2)]; % sum to just consumption and capital consumption
y1(2,:,:)=yr(1:15,1:2); % 2012 impacts
y2=1e-12*permute(y1,[2,1,3]); % rearrange 3D array, convert MJ to EJ
y3(:,:,1)=y2(:,:,2);
y3(:,:,2)=y2(:,:,1); % put capital on bottom

label=R07.labsconEF07(1:15);
q=plotBarStackGroups(y3,label);
xtickangle(45)
u=legend(q(1,order),labels{order});
title('B. Detailed sectors with highest Energy Footprint 2007, 2012')
ylabel('EJ')
% ylim([0 27])
set(gca,'FontSize',11.5);
colormap(cm);
% set(u,'Fontsize',8.3)
box on
%% sensitivity with 2007 intensity for capital layer
% calculate diff in 2007 intensites for 2012 capital consumption
diff_all_E=(IO.C(6,:)*R07.S2012USD*Ldiff*diag(y_exp))';

y1=zeros(2,15,2);
y1(1,:,1)=sum(EF_con_sort(1:15,1:4),2); % 2012 impacts from consumption only (no capital);
y1(1,:,2)=diff_all_E(ordere12(1:15)); % 2012 impacts from capital only
yr=EF_con(ordere12,:); % 2012 impacts, use 2012 highest CF consumption sector order
yr=[sum(yr(:,1:4),2) sum(yr(:,5:8),2)]; % convert kg to Mt
y1(2,:,:)=yr(1:15,1:2); % 2012 impacts
y2=1e-12*permute(y1,[2,1,3]); % rearrange 3D array
y3(:,:,1)=y2(:,:,2);
y3(:,:,2)=y2(:,:,1); % put capital on bottom
label=labsconEF(1:15);
order=[2 1];
labels={'Capital Consumption','Consumption'};
% using plotBarStackGroups function, figure is defined in function
q=plotBarStackGroups(y3,label);
xtickangle(45)
u=legend(q(1,order),labels{order});
title('B. Detailed sectors with highest Energy Footprint 2012')
ylabel('EJ')
% ylim([0 1600])
set(gca,'FontSize',11.5);
cm=[0 0.4 1; 0.8 0.1 0.1];
colormap(cm);
box on
% differences of capital layer intensity
Econ_tot=sum(sum(EF_con(:,1:4)));
Ecap12_tot=sum(sum(EF_con(:,5:8)));
Ecap07_tot=sum(diff_all_E);
Ecap_diff=Ecap07_tot/Ecap12_tot;
Etot_diff=(Econ_tot+Ecap07_tot)/(Econ_tot+Ecap12_tot);
%% MF
y1=zeros(2,15,2);
% y1(1,:,:)=R07.MF_con_sort_Mt(1:15,1:4); % 2007 impacts
y1(1,:,:)=[sum(R07.MF_con_sort_Mt(1:15,1:4),2) sum(R07.MF_con_sort_Mt(1:15,5:8),2)]; % 2007 impacts
yr=1e-9*MF_con(R07.orderm07,:); % 2012 impacts, use 2007 highest MF consumption sector order
yr=[sum(yr(:,1:4),2) sum(yr(:,5:8),2)];
y1(2,:,:)=yr(1:15,1:2); % 2012 impacts, convert kg to Mt
y2=permute(y1,[2,1,3]); % rearrange 3D array
y3(:,:,1)=y2(:,:,2);
y3(:,:,2)=y2(:,:,1); % put capital on bottom
label=meta.ZLabs_short(R07.orderm07(1:15),2);

q=plotBarStackGroups(y3,label);
xtickangle(45)
u=legend(q(1,order),labels{order});
title('C. Detailed sectors with highest Material Footprint 2007, 2012')
ylabel('Mt')
% ylim([0 800])
ax=gca;
set(gca,'FontSize',11.5);
colormap(cm);
box on
%% sensitivity with 2007 intensity for capital layer
% calculate diff in 2007 intensites for 2012 capital consumption
diff_all_M=(IO.C(11,:)*R07.S2012USD*Ldiff*diag(y_exp))'; % capital consumption impacts in 2012 assuming 2007 intensities

y1=zeros(2,15,2);
y1(1,:,1)=sum(MF_con_sort(1:15,1:4),2); % 2012 impacts from consumption only (no capital);
y1(1,:,2)=diff_all_M(orderm12(1:15)); % 2012 impacts from capital only
yr=MF_con(orderm12,:); % 2012 impacts, use 2012 highest CF consumption sector order
yr=[sum(yr(:,1:4),2) sum(yr(:,5:8),2)]; % convert kg to Mt
y1(2,:,:)=yr(1:15,1:2); % 2012 impacts
y2=1e-9*permute(y1,[2,1,3]); % rearrange 3D array
y3(:,:,1)=y2(:,:,2);
y3(:,:,2)=y2(:,:,1); % put capital on bottom
label=labsconMF(1:15);
order=[2 1];
labels={'Capital Consumption','Consumption'};
% using plotBarStackGroups function, figure is defined in function
q=plotBarStackGroups(y3,label);
xtickangle(45)
u=legend(q(1,order),labels{order});
title('C. Detailed sectors with highest Material Footprint 2012')
ylabel('Mt')
set(gca,'FontSize',11.5);
cm=[0 0.4 1; 0.8 0.1 0.1];
colormap(cm);
box on
% differences of capital layer intensity
pfuelm=(y3(1,1,1)+y3(1,1,2))/(y3(1,2,1)+y3(1,2,2));
oohousm=(y3(3,1,1)+y3(3,1,2))/(y3(3,2,1)+y3(3,2,2));
Mcon_tot=sum(sum(MF_con(:,1:4)));
Mcap12_tot=sum(sum(MF_con(:,5:8)));
Mcap07_tot=sum(diff_all_M);
Mcap_diff=Mcap07_tot/Mcap12_tot;
Mtot_diff=(Mcon_tot+Mcap07_tot)/(Mcon_tot+Mcap12_tot);
end
%% plot impacts by stressor type
%GHG
ghg=diag(IO.C(2,:))*IO.S*IO.Lt*diag(y_exp);
ghg=ghg(1:15,:);
%EF
ef=diag(IO.C(6,:))*IO.S*IO.Lt*diag(y_exp);
ef=ef(16:25,:);
%MF
mf=IO.C(7:10,:)*IO.S*IO.Lt*diag(y_exp);
MF=(IO.C(11,:)*IO.S*IO.Lt*diag(y_exp))';

if year == 2007
ghg_sort = ghg(:,order07);
xlabg=[meta.ZLabs_short(order07(1:12),2);{'Other'}];
e_sort=ef(:,ordere07);
xlabe=[meta.ZLabs_short(ordere07(1:12),2);{'Other'}];
m_sort=mf(:,orderm07);
xlabm=[meta.ZLabs_short(orderm07(1:12),2);{'Other'}];
end

if year == 2012
ghg_sort = ghg(:,order12);
xlabg=[meta.ZLabs_short(order12(1:12),2);{'Other'}];
e_sort=ef(:,ordere12);
xlabe=[meta.ZLabs_short(ordere12(1:12),2);{'Other'}];
m_sort=mf(:,orderm12);
xlabm=[meta.ZLabs_short(orderm12(1:12),2);{'Other'}];
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
if year ==2012
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
if year == 2012
title(['D. Detailed sectors with highest EF by energy resource, ', num2str(year)]);
end
xticks(1:13)
xticklabels(xlabe)
xtickangle(45)
labels=[{'Coal'}, {'Oil'},{'Nat gas liquids'}, {'Nat gas'}, {'Biomass'}, {'Geothermal'},{'Hydro'},{'Nuclear'},{'Solar'},{'Wind'}];
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
if year == 2012
title(['F. Detailed sectors with highest MF by material class, ', num2str(year)]);
end
xticks(1:13)
xticklabels(xlabm)
xtickangle(45)
labels=[{'Metals'}, {'Other minerals'}, {'Fossil fuels'}, {'Biomass'}];
u=legend(q(order),labels{order});
set(u,'Fontsize',9)
ylim([0 2200]);
colormap('jet')
set(gca,'FontSize',11.5);
set(gca,'TitleFontSizeMultiplier',1.08);
%% Summary results
if year == 2012
% CF, in Mt CO2eq    
CF_wo_K=sum(sum_con(:,1:4),2)*1e-9;
CF_w_K=sum(sum_con(:,1:8),2)*1e-9;
CF_wo_K_07=sum(R07.sum_con(:,1:4),2)*1e-9;
CF_w_K_07=sum(R07.sum_con(:,1:8),2)*1e-9;
% EF, in TJ
EF_wo_K=sum(EF_con(:,1:4),2)*1e-9;
EF_w_K=sum(EF_con(:,1:8),2)*1e-9;
EF_wo_K_07=sum(R07.EF_con(:,1:4),2)*1e-9;
EF_w_K_07=sum(R07.EF_con(:,1:8),2)*1e-9;
% MF, in Mt
MF_wo_K=sum(MF_con(:,1:4),2)*1e-9;
MF_w_K=sum(MF_con(:,1:8),2)*1e-9;
MF_wo_K_07=sum(R07.MF_con(:,1:4),2)*1e-9;
MF_w_K_07=sum(R07.MF_con(:,1:8),2)*1e-9;
% reduction in footprints between 2007 and 2012
red_CF=1-sum(CF_w_K)/sum(CF_w_K_07)
red_MF=1-sum(MF_w_K)/sum(MF_w_K_07)
red_EF=1-sum(EF_w_K)/sum(EF_w_K_07)
end
% CI kg/$
CI_wo_k=GHG_int_con*1e-6;
CI_w_k=GHG_int_Atot*1e-6;
CI_wo_k_purch_408=GHG_int_con_purch_408*1e-6;
CI_w_k_purch_408=GHG_int_Atot_purch_408*1e-6;
% EI MJ/$
EI_wo_k=E_int_A*1e-6;
EI_w_k=E_int_At*1e-6;
EI_wo_k_purch_408=E_int_A_purch_408*1e-6;
EI_w_k_purch_408=E_int_At_purch_408*1e-6;
% MI kg/$
MI_wo_k=M_int_A*1e-6;
MI_w_k=M_int_At*1e-6;
MI_wo_k_purch_408=M_int_A_purch_408*1e-6;
MI_w_k_purch_408=M_int_At_purch_408*1e-6;

if year == 2012
int_tot = table(row,CI_wo_k,CI_w_k,CI_wo_k_purch_408,CI_w_k_purch_408,EI_wo_k,EI_w_k,EI_wo_k_purch_408,EI_w_k_purch_408,MI_wo_k,MI_w_k,MI_wo_k_purch_408,MI_w_k_purch_408,'RowNames',meta.ZLabs_short(:,2)); 
end

if year == 2007
    % CI kg/$
    CI_wo_k=GHG_int_con07*1e-6;
    CI_w_k=GHG_int_Atot07*1e-6;
    % EI MJ/$
    EI_wo_k=E_int_A07*1e-6;
    EI_w_k=E_int_At07*1e-6;
    % MI kg/$
    MI_wo_k=M_int_A07*1e-6;
    MI_w_k=M_int_At07*1e-6;
    
    int_tot = table(row,CI_wo_k,CI_w_k,CI_wo_k_purch_408,CI_w_k_purch_408,EI_wo_k,EI_w_k,EI_wo_k_purch_408,EI_w_k_purch_408,MI_wo_k,MI_w_k,MI_wo_k_purch_408,MI_w_k_purch_408,'RowNames',meta.ZLabs_short(:,2)); 
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
        R07.int_tot=int_tot;
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
        R07.S2012USD=IO.S2012USD;
        save(['Results_' question1{:}],'R07')
    case 2012
        R12.D_GHGt=D_GHGt;
        R12.dt_con=dt_con;
        R12.dt_expinv=dt_expinv;
        R12.sum_exp_inv=sum_exp_inv;
        R12.MF_exp_inv=MF_exp_inv;
        R12.MF_con_sum=MF_con_sum;
        R12.EF_exp_inv=EF_exp_inv;
        R12.EF_con_sum=EF_con_sum;
        R12.sum_con_sec=sum_con_sec;
        R12.MF_sum_con=MF_sum_con;
        R12.EF_sum_con=EF_sum_con;
        R12.ZLab25=ZLab25;
        R12.sum_con=sum_con;
        R12.MF_con=MF_con;
        R12.EF_con=EF_con;
        R12.int=int;
        R12.int_tot=int_tot;
        save(['Results_' question1{:}],'R12')
end