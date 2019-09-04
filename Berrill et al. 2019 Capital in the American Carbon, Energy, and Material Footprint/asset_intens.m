% clc; clear;
% load('USEEIO_2012.mat')
% %%
% Zk=IO.Ak*diag(IO.x);
% Zs=sum(Zk,2);
% mine=[14:21];
% yhe=zeros(408,1);
% yhe(mine)=Zs(mine);
% GHG_mine=IO.C(2,:)*IO.S*IO.Lt*yhe;
% CM_mine=GHG_mine/sum(yhe)*1e-6
% %%
% res=[30,31,35];
% yhe=zeros(408,1);
% yhe(res)=Zs(res);
% GHG_res=IO.C(2,:)*IO.S*IO.Lt*yhe;
% CM_res=GHG_res/sum(yhe)*1e-6
% 
% 
% %%
% nonres=[25:29, 32:34, 36];
% yhe=zeros(408,1);
% yhe(nonres)=Zs(nonres);
% GHG_nonres=IO.C(2,:)*IO.S*IO.Lt*yhe;
% CM_nonres=GHG_nonres/sum(yhe)*1e-6
% 
% %%
% mvm=[53:190];
% yhe=zeros(408,1);
% yhe(mvm)=Zs(mvm);
% GHG_mvm=IO.C(2,:)*IO.S*IO.Lt*yhe;
% CM_mvm=GHG_mvm/sum(yhe)*1e-6
% %%
% oth=[37:52,191:227,228:270];
% yhe=zeros(408,1);
% yhe(oth)=Zs(oth);
% GHG_oth=IO.C(2,:)*IO.S*IO.Lt*yhe;
% CM_oth=GHG_oth/sum(yhe)*1e-6
% %%
% mar=[271:299];
% yhe=zeros(408,1);
% yhe(mar)=Zs(mar);
% GHG_mar=IO.C(2,:)*IO.S*IO.Lt*yhe;
% CM_mar=GHG_mar/sum(yhe)*1e-6
% %%
% infart=[300:322,370:377];
% yhe=zeros(408,1);
% yhe(infart)=Zs(infart);
% GHG_infart=IO.C(2,:)*IO.S*IO.Lt*yhe;
% CM_infart=GHG_infart/sum(yhe)*1e-6
% %%
% re=[323:329];
% yhe=zeros(408,1);
% yhe(re)=Zs(re);
% GHG_re=IO.C(2,:)*IO.S*IO.Lt*yhe;
% CM_re=GHG_re/sum(yhe)*1e-6
% %%
% rnd=[330:343];
% yhe=zeros(408,1);
% yhe(rnd)=Zs(rnd);
% GHG_rnd=IO.C(2,:)*IO.S*IO.Lt*yhe;
% CM_rnd=GHG_rnd/sum(yhe)*1e-6

%% Detailed highest consumers and producers of EF impacts (2-way Contribution analysis results)
Ds=DE_t; % total EF impacts from prod and cons
Ds(:,size(Ds,2)+1)=sum(Ds,2); % sum from prod. sectors
Ds(:,size(Ds,2)+1)=1:408; % number sectors in original order
DS=flipud(sortrows(Ds,409)); % sort by largest producers
DsT=DS'; % transpose
DsT(409:410,:)=[]; % remove the prod. totals and sector numbers
DsT(:,409)=sum(DsT,2); % sum from cons. sectors
DsT(:,410)=1:408; % original sector numbers
DsTs=flipud(sortrows(DsT,409)); % sort by largest consumers
D_con_sec=DsTs(1:15,410); % the sector numbers of highest consumers
%% largest proportional contributors to footprints
sump=sum(DE_t,2); % sum of production emissions
sump(:,2)=1:408; % sec numbers
sumps=flipud(sortrows(sump,1)); % sort sump
main_pr=sumps(1:8,2)';
m=size(main_pr,2); % number of interesting prod. sectors 
other=1:408; 
other(main_pr)=[]; % remove sectors of interest from list of all sector numbers
Dn=[DE_t(main_pr,:); DE_t(other,:)]; % Order production sectors by interesting, other (not in order of highest impacts)
DnT=Dn'; % transpose - put production sectors on columns, consumption on rows
DnT(:,409)=sum(DnT,2); % total consumption sectors
DnT(:,410)=1:408; % sector number of consumption sectors
DnTs=flipud(sortrows(DnT,409)); % sort by largest consumption sectors
DnTs(:,409:410)=[]; % remove totals and sector numbers
ot=sum(DnTs(1:15,m+1:end),2); % the 'other' production impacts from the highest consumption sectors, not explained by the extracted production sectors
cont=[DnTs(1:15,1:m) ot]; % matrix of contributions from prod. sectors of interest, and other production sectors
cs=sum(cont,2); % row sums, total of consumption sectors
c=diag(cs)\cont; % get pc of contributions from prod. sectors to cons. sectors impacts
order=fliplr(1:m+1);
%% contribution to 'other' consumption sectors
otr2c1=DnTs(16:408,1:m); % inputs to other consumption sectors, from extraction prod sectors
otr2c2=DnTs(16:408,m+1:408); % inputs to other cons sectors, from other prod sectors
contr2=[sum(otr2c1,1) sum(sum(otr2c2))]; % contr of int prod sectors to other cons sectors; contr of other prod sectors to other cons sectors
csr2=sum(contr2); % total 'other' impacts
contr2n=contr2/csr2; % make vector of contribution pc to 'other' consumption sector
ctot=[c;contr2n]; % total contribution from to top consumption sectors, other consumption sectors
f13=figure;
q=bar(ctot,'stack');
xlab=[meta.ZLabs_short(D_con_sec,2);{'Other'}];
xticks(1:16);
xticklabels(xlab)
xtickangle(45)
labels=[meta.ZLabs_short(main_pr,2) ;{'Other'}];
title(['B. Contribution from production sectors to EF of consumption, ' num2str(year)])
u=legend(q(order),labels{order},'Location','northeastoutside');
title(u,'Production sectors')
colormap(jet(m+12))
ylim([0 1])
xlabel('\bfConsumption sectors')
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% Detailed highest consumers and producers of MF impacts (2-way Contribution analysis results)
Ds=DM_t; % total MF impacts from prod and cons
Ds(:,size(Ds,2)+1)=sum(Ds,2); % sum from prod. sectors
Ds(:,size(Ds,2)+1)=1:408; % number sectors in original order
DS=flipud(sortrows(Ds,409)); % sort by largest producers
DsT=DS'; % transpose
DsT(409:410,:)=[]; % remove the prod. totals and sector numbers
DsT(:,409)=sum(DsT,2); % sum from cons. sectors
DsT(:,410)=1:408; % original sector numbers
DsTs=flipud(sortrows(DsT,409)); % sort by largest consumers
D_con_sec=DsTs(1:15,410); % the sector numbers of highest consumers
%% largest proportional contributors to material footprint
sump=sum(DM_t,2); % sum of production emissions
sump(:,2)=1:408; % sec numbers
sumps=flipud(sortrows(sump,1)); % sort sump
main_pr=sumps(1:8,2)';
m=size(main_pr,2); % number of interesting prod. sectors 
other=1:408; 
other(main_pr)=[]; % remove sectors of interest from list of all sector numbers
Dn=[DM_t(main_pr,:); DM_t(other,:)]; % Order production sectors by interesting, other (not in order of highest impacts)
DnT=Dn'; % transpose - put production sectors on columns, consumption on rows
DnT(:,409)=sum(DnT,2); % total consumption sectors
DnT(:,410)=1:408; % sector number of consumption sectors
DnTs=flipud(sortrows(DnT,409)); % sort by largest consumption sectors
DnTs(:,409:410)=[]; % remove totals and sector numbers
ot=sum(DnTs(1:15,m+1:end),2); % the 'other' production impacts from the highest consumption sectors, not explained by the extracted production sectors
cont=[DnTs(1:15,1:m) ot]; % matrix of contributions from prod. sectors of interest, and other production sectors
cs=sum(cont,2); % row sums, total of consumption sectors
c=diag(cs)\cont; % get pc of contributions from prod. sectors to cons. sectors impacts
order=fliplr(1:m+1);
%% contribution to 'other' consumption sectors
otr2c1=DnTs(16:408,1:m); % inputs to other consumption sectors, from extraction prod sectors
otr2c2=DnTs(16:408,m+1:408); % inputs to other cons sectors, from other prod sectors
contr2=[sum(otr2c1,1) sum(sum(otr2c2))]; % contr of int prod sectors to other cons sectors; contr of other prod sectors to other cons sectors
csr2=sum(contr2); % total 'other' impacts
contr2n=contr2/csr2; % make vector of contribution pc to 'other' consumption sector
ctot=[c;contr2n]; % total contribution from to top consumption sectors, other consumption sectors
f13=figure;
q=bar(ctot,'stack');
xlab=[meta.ZLabs_short(D_con_sec,2);{'Other'}];
xticks(1:16);
xticklabels(xlab)
xtickangle(45)
labels=[meta.ZLabs_short(main_pr,2) ;{'Other'}];
title(['C. Contribution from production sectors to MF of consumption, ' num2str(year)])
u=legend(q(order),labels{order},'Location','northeastoutside');
title(u,'Production sectors')
colormap(jet(m+12))
ylim([0 1])
xlabel('\bfConsumption sectors')
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% Cont to capital EF
Ds=DE_k; % total EF impacts from prod and cons
Ds(:,size(Ds,2)+1)=sum(Ds,2); % sum from prod. sectors
Ds(:,size(Ds,2)+1)=1:408; % number sectors in original order
DS=flipud(sortrows(Ds,409)); % sort by largest producers
DsT=DS'; % transpose
DsT(409:410,:)=[]; % remove the prod. totals and sector numbers
DsT(:,409)=sum(DsT,2); % sum from cons. sectors
DsT(:,410)=1:408; % original sector numbers
DsTs=flipud(sortrows(DsT,409)); % sort by largest consumers
D_con_sec=DsTs(1:15,410); % the sector numbers of highest consumers
%% largest proportional contributors to footprints
sump=sum(DE_k,2); % sum of production emissions
sump(:,2)=1:408; % sec numbers
sumps=flipud(sortrows(sump,1)); % sort sump
main_pr=sumps(1:8,2)';
m=size(main_pr,2); % number of interesting prod. sectors 
other=1:408; 
other(main_pr)=[]; % remove sectors of interest from list of all sector numbers
Dn=[DE_k(main_pr,:); DE_k(other,:)]; % Order production sectors by interesting, other (not in order of highest impacts)
DnT=Dn'; % transpose - put production sectors on columns, consumption on rows
DnT(:,409)=sum(DnT,2); % total consumption sectors
DnT(:,410)=1:408; % sector number of consumption sectors
DnTs=flipud(sortrows(DnT,409)); % sort by largest consumption sectors
DnTs(:,409:410)=[]; % remove totals and sector numbers
ot=sum(DnTs(1:15,m+1:end),2); % the 'other' production impacts from the highest consumption sectors, not explained by the extracted production sectors
cont=[DnTs(1:15,1:m) ot]; % matrix of contributions from prod. sectors of interest, and other production sectors
cs=sum(cont,2); % row sums, total of consumption sectors
c=diag(cs)\cont; % get pc of contributions from prod. sectors to cons. sectors impacts
order=fliplr(1:m+1);
%% contribution to 'other' consumption sectors
otr2c1=DnTs(16:408,1:m); % inputs to other consumption sectors, from extraction prod sectors
otr2c2=DnTs(16:408,m+1:408); % inputs to other cons sectors, from other prod sectors
contr2=[sum(otr2c1,1) sum(sum(otr2c2))]; % contr of int prod sectors to other cons sectors; contr of other prod sectors to other cons sectors
csr2=sum(contr2); % total 'other' impacts
contr2n=contr2/csr2; % make vector of contribution pc to 'other' consumption sector
ctot=[c;contr2n]; % total contribution from to top consumption sectors, other consumption sectors
f13=figure;
q=bar(ctot,'stack');
xlab=[meta.ZLabs_short(D_con_sec,2);{'Other'}];
xticks(1:16);
xticklabels(xlab)
xtickangle(45)
labels=[meta.ZLabs_short(main_pr,2) ;{'Other'}];
title(['B. Contribution from production sectors to EF of capital consumption, ' num2str(year)])
u=legend(q(order),labels{order},'Location','northeastoutside');
title(u,'Production sectors')
colormap(jet(m+12))
ylim([0 1])
xlabel('\bfConsumption sectors')
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
%% Contribution to capital MF
Ds=DM_k; % total MF impacts from prod and cons
Ds(:,size(Ds,2)+1)=sum(Ds,2); % sum from prod. sectors
Ds(:,size(Ds,2)+1)=1:408; % number sectors in original order
DS=flipud(sortrows(Ds,409)); % sort by largest producers
DsT=DS'; % transpose
DsT(409:410,:)=[]; % remove the prod. totals and sector numbers
DsT(:,409)=sum(DsT,2); % sum from cons. sectors
DsT(:,410)=1:408; % original sector numbers
DsTs=flipud(sortrows(DsT,409)); % sort by largest consumers
D_con_sec=DsTs(1:15,410); % the sector numbers of highest consumers
%% largest proportional contributors to material footprint
sump=sum(DM_k,2); % sum of production emissions
sump(:,2)=1:408; % sec numbers
sumps=flipud(sortrows(sump,1)); % sort sump
sumps1=sumps;
main_pr=sumps(1:8,2)';
m=size(main_pr,2); % number of interesting prod. sectors 
other=1:408; 
other(main_pr)=[]; % remove sectors of interest from list of all sector numbers
Dn=[DM_k(main_pr,:); DM_k(other,:)]; % Order production sectors by interesting, other (not in order of highest impacts)
DnT=Dn'; % transpose - put production sectors on columns, consumption on rows
DnT(:,409)=sum(DnT,2); % total consumption sectors
DnT(:,410)=1:408; % sector number of consumption sectors
DnTs=flipud(sortrows(DnT,409)); % sort by largest consumption sectors
DnTs(:,409:410)=[]; % remove totals and sector numbers
ot=sum(DnTs(1:15,m+1:end),2); % the 'other' production impacts from the highest consumption sectors, not explained by the extracted production sectors
cont=[DnTs(1:15,1:m) ot]; % matrix of contributions from prod. sectors of interest, and other production sectors
cs=sum(cont,2); % row sums, total of consumption sectors
c=diag(cs)\cont; % get pc of contributions from prod. sectors to cons. sectors impacts
order=fliplr(1:m+1);
%% contribution to 'other' consumption sectors
otr2c1=DnTs(16:408,1:m); % inputs to other consumption sectors, from extraction prod sectors
otr2c2=DnTs(16:408,m+1:408); % inputs to other cons sectors, from other prod sectors
contr2=[sum(otr2c1,1) sum(sum(otr2c2))]; % contr of int prod sectors to other cons sectors; contr of other prod sectors to other cons sectors
csr2=sum(contr2); % total 'other' impacts
contr2n=contr2/csr2; % make vector of contribution pc to 'other' consumption sector
ctot=[c;contr2n]; % total contribution from to top consumption sectors, other consumption sectors
f13=figure;
q=bar(ctot,'stack');
xlab=[meta.ZLabs_short(D_con_sec,2);{'Other'}];
xticks(1:16);
xticklabels(xlab)
xtickangle(45)
labels=[meta.ZLabs_short(main_pr,2) ;{'Other'}];
title(['C. Contribution from production sectors to MF of capital consumption, ' num2str(year)])
u=legend(q(order),labels{order},'Location','northeastoutside');
title(u,'Production sectors')
colormap(jet(m+12))
ylim([0 1])
xlabel('\bfConsumption sectors')
set(gca,'FontSize',11.5);
set(u,'Fontsize',9.5)
