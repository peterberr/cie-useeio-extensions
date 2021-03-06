%% FD Changes function
% Script to alter some columns of 2013 Final Demand estimates produced by a 
% Quadratic Programme, to come closer in line with estimates of 2013 Final 
% Demand from various sources

% Peter Berrill
% Sep 4 2013

% Required input files:

% 'PCEBridge_2007_Detail.xlsx'
% 'PCE 2013.xls' % PCE data
% 'sum_inv_2013.xls' % Investment data
% '2013 exports.xlsx' % Export data
% '2013 imports.xlsx' % Import data
% 'balance_us_sut_07r_basic.xlsx' % Estimated Make and Use tables from QP

function [MV_changes, FD_struc]= FD_changes(Zsort)
[PCE_bridge, ~, PCE_raw]=xlsread('PCEBridge_2007_Detail.xlsx','2007');
%%
NIPA_Purch=[PCE_bridge(1:704,1) PCE_bridge(1:704,9)]; % NIPA Line and Expenditure in Purchasers value (2007)
NIPA_uniq=unique(PCE_bridge(1:704,1)); % unique list of NIPA lines
NIPA_uniq(:,2)=0;
for i=1:size(NIPA_uniq,1)
    f=find(NIPA_uniq(i,1)==NIPA_Purch(:,1));
    NIPA_uniq(i,2)=sum(NIPA_Purch(f,2)); % sume consumption expenditures in NIPA Line-Categories
end
%%
NIPA_PTT=[PCE_bridge(1:704,1) PCE_bridge(1:704,5:8)]; % NIPA Lines and Expenditure in Producers value, then Transport, Wholesale and Retail Trade margins
NIPA_ratio=NIPA_PTT;
NIPA_ratio(:,2:end)=0;
for j=1:704
    for k=1:size(NIPA_uniq,1)
        if NIPA_ratio(j,1)==NIPA_uniq(k,1)
           NIPA_ratio(j,2:5)=NIPA_PTT(j,2:5)/NIPA_uniq(k,2); % Assemble matrix of ratios of producers value, transport, wholesale, retail margins to purchasers value (2007)
        end
    end
end
%%
PCE_2013=xlsread('PCE 2013.xls');
pce13=PCE_2013(NIPA_uniq(:,1)); % PCE 13 in  NIPA Line Categories
pce13(find(NIPA_uniq(:,2)<0))=pce13(find(NIPA_uniq(:,2)<0))*-1; % Make expenditure by nonresidents negative
pce13(:,2)=pce13;
pce13(:,1)=NIPA_uniq(:,1); % Add in NIPA Line numbers
%%
NIPA_PTT13=NIPA_PTT;
NIPA_PTT13(:,2:5)=0;
for j=1:size(NIPA_PTT13,1)
    for k=1:size(pce13,1)
        if NIPA_PTT13(j,1)==pce13(k,1)
            NIPA_PTT13(j,2:5)=NIPA_ratio(j,2:5)*pce13(k,2); % Create matrix of producers value, transport, wholesale, retail margins in 2013 based on 2013 purchasers values, and 2007 margins
        end
    end
end

PCE_bea13=[string(PCE_raw(6:709,3)) string(NIPA_PTT13(:,2))]; % create list of PCE in 2013 producers values in BEA commodity classifications 

%% Investment
[Inv, ~, Inv_raw]=xlsread('sum_inv_2013.xls');
%% FD
Ycomp=zeros(389,20); % Predefine Y (or Final Demand) matrix consisting of compiled data for comparison with Y output from QP
Ycomp(1:389,2:5)=Inv(:,3:6); % Input investment columns
Ycomp(1:389,10:12)=Inv(:,7:9);
Ycomp(1:389,14:16)=Inv(:,10:12);
Ycomp(1:389,18:20)=Inv(:,13:15);

Y=[string(Inv_raw(2:390,1)) string(Ycomp)];

for i=1:389
     q=strmatch(Y(i,1),PCE_bea13(:,1),'exact');
     Y(i,2)=string(sum(str2double((PCE_bea13(q,2))))); % Add in PCE column
end
%% Exports 2013
% Create Transformation matrix from Purc. to Prod. prices for exports (Tex)
[Ma, ~, Maraw] = xlsread('Margins_Before_Redefinitions_2007_Detail.xlsx','2007','A54330:J55971'); % import Margins tables related to final demand

M7=Ma(725:1060,:); % margins for column 7 of final demand, exports
m7r=M7(:,3:6)./M7(:,7); % margins as ratios of purchasers prices
m7r(isnan(m7r))=0; m7r(isinf(m7r))=1; % remove NaNs and Infs
Tex_str=[string(Zsort(:,1)) string(zeros(389,4))]; % predefine Transformation matrix, in sorted rows
for i=1:389
for j=1:size(M7,1)
if Tex_str(i,1)==string(Maraw(724+j,4)) % match commodity label
Tex_str(i,2:5)=m7r(j,1:4); % margins for appropriate commodity
end
end
tex=str2double(Tex_str(:,2:5)); % Convert margins matrix to numeric, columns of 'tex' are Prod. value; Transp; Wholesale; Retail; Ratio of purchasers value 
end
tex(tex>1)=1; % convert retail trade and transport ratios to 1
texs=tex;
texs(275:277,:)=[];% remove the three retail commodity sectors that do not exist at purch prices, i.e. Motor vehicle/parts dealers; Food and bev stores; General merch stores

Tex=zeros(389,386); % predfine final margins Transformation matrix
% allocate transport margins to transport sectors
air=M7(263,3)-M7(263,7); % difference between Producers and Purchasers value of Air transportation
rail=M7(264,3)-M7(264,7); % difference for rail transportation
water=M7(265,3)-M7(265,7); % difference for water transportation
truck=M7(266,3)-M7(266,7); % difference for truck transportation
pipe=M7(267,3)-M7(267,7); % difference for truck transportation
sum_Trans=air+rail+water+truck+pipe; % total margin from transport sectors
Tex(278,:)=(air/sum_Trans)*texs(:,2)'; % allocate a certain portion of transportation margins to air transport based on air transports input to total transport 
Tex(279,:)=(rail/sum_Trans)*texs(:,2)'; % same for rail transport
Tex(280,:)=(water/sum_Trans)*texs(:,2)'; % same for water transport
Tex(281,:)=(truck/sum_Trans)*texs(:,2)'; % same for truck transport
Tex(283,:)=(pipe/sum_Trans)*texs(:,2)'; % same for pipe transport

% allocate wholesale margins
Tex(262,:)=texs(:,3)'; % Wholesale trade

% need to add rows to T so that its compatible with 392 sectors.
tex=[tex;repmat([1,0,0,0],3,1)]; % three new fuel use sectors, all have no margins
Tex=[Tex zeros(389,3); zeros(3,389)]; % three new fuel use sectors, all have no margins

for k=1:274
    Tex(k,k)=tex(k,1); % add tex values to diagonals of Tex
end

for k=278:392
    Tex(k,k-3)=tex(k,1); % add tex values to 'diagonals' of Tex
end

load('General_meta.mat')
[~, ~, ex13_raw]=xlsread('2013 exports.xlsx','Sheet4');
ex13r=ex13_raw(2:251,:);
for j=1:250
ex13r(j,3)={cell2mat(ex13r(j,3))/1000}; % load in export values, convert from thousand USD to million USD
end
ExLab13=Z; % BEA commodity codes and labels
ExLab13(:,3)={0};

for i=1:392
    for j=1:250
    if strmatch(string(ex13r(j,1)),ExLab13(i,1))
        ExLab13(i,3)=ex13r(j,3); % add exports in appropriate BEA rows, purchasers prices
    end
    end
end

exnum13=cell2mat(ExLab13(:,3)); 
exnum13(275:277)=[];

exnum_prod_13=Tex*exnum13; % convert from purchasers prices to producers prices

for u=1:274
ExLab13(u,4)={exnum_prod_13(u)}; % add column of exports in producers prices to ExLab13
end
for u=278:392
    ExLab13(u,4)={exnum_prod_13(u)}; % add column of exports in producers prices to ExLab13
end
ExLab13(275:277,4)={0};

%% Imports 2013
[~, ~, im13_raw]=xlsread('2013 imports.xlsx','Sheet1');

im13r=im13_raw(2:250,:);
for j=1:249
im13r(j,3)={cell2mat(im13r(j,3))/1000};% load in import values, convert from thousand USD to million USD
end
ImLab13=Z;
ImLab13(:,3)={0};

for i=1:392
    for j=1:249
    if strmatch(string(im13r(j,1)),ImLab13(i,1))
        ImLab13(i,3)=im13r(j,3);  % add imports in appropriate BEA rows, already in prod prices
    end
    end
end

% Incorporate imports and exports data in compiled Y matrix
ex=string(ExLab13);
for i=1:389
     q=strmatch(Y(i,1),ex(:,1),'exact');
     Y(i,8)=string(str2double((ex(q,4)))); % add column of exports , in row order of Y i.e. unsorted
end
im=string(ImLab13);
for i=1:389
     q=strmatch(Y(i,1),im(:,1),'exact');
     Y(i,9)=string(-1*str2double((im(q,3)))); % add column of imports, in row order of Y i.e. unsorted
end
%% Compare
[QP7, ~, ~]=xlsread('balance_us_sut_07r_basic.xlsx','Detail_QP_Comb');
Y_QP=QP7(4:392,782:801);
Y_comp=str2double(Y(:,2:21));

Final_uses=sum(Y_QP,2);
Y_comp(:,6)=Final_uses-sum(Y_comp,2);
%% 'RAS' structures
struc_comp=str2double(Y(27:36,2:21));
Z0=Y_QP(27:36,:);
u_1=sum(Z0,2);
v_1=sum(Z0,1)';
lim=70; % number of times for RAS procedure to run

Zn=zeros([size(struc_comp),lim+1]);
Zn(:,:,1)=struc_comp;

[u,r]=deal(zeros(size(Z0,1),lim));
[v,s]=deal(zeros(size(Z0,2),lim));

for i=1:lim
    
    u(:,i)=sum(Zn(:,:,i),2); % initial estimate of u

    r(:,i)=u_1./u(:,i);
    r(isnan(r))=0;
    
    Ztemp=diag(r(:,i))*Zn(:,:,i);

    v(:,i)=sum(Ztemp,1)';
    s(:,i)=v_1./v(:,i);
    s(isnan(s))=0;

    Zn(:,:,i+1)=Ztemp*diag(s(:,i));
end
FD_struc=Zn(:,:,lim+1); % FD for structures, from final iteration of RAS. This should replace the structures (i.e. Construction excluding repair) rows of Y before sorting
%% Alter Motor Vehicles Manually
MV_QP=Y_QP(150:163,:);
MV_comp=Y_comp(150:163,:);
MV_diff=MV_QP-MV_comp;

MV_changes=zeros(14,20);
reduce_pce=[1 7 9 10]; % Sectors: Automobile manuf., Travel trailer and camper manuf., MV electrical and electronic equipment manuf., & MV steering, suspension component (except spring), and brake systems manuf.
MV_changes(reduce_pce,1)=MV_diff(reduce_pce,1); % change sectors by their difference from compiled data
% balance remaning cells
MV_changes(2,1)=sum(MV_changes(reduce_pce,1))*-1;
MV_changes(1,7)=-7000;
MV_changes(1,6)=sum(MV_changes(1,:))*-1;
MV_changes(7,6)=MV_changes(7,1)*-1;
MV_changes(9,7)=MV_diff(9,7);
MV_changes(9,6)=sum(MV_changes(9,:))*-1;
MV_changes(10,7)=MV_changes(10,1)*-1;
MV_changes(2,6)=sum(MV_changes(:,6))*-1;
MV_changes(2,7)=sum(MV_changes(:,7))*-1; % Final MV Changes matrix to be subtracted to the MV rows of Y before sorting

end
