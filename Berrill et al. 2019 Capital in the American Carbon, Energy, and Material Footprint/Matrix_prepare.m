%% Finalised script for preparing USEEIO matrices with capital for 2007 and 2012

% Peter Berrill, Mar 28 2019

% Required input files:
% From US BEA, download at https://www.bea.gov/industry/io_annual.htm
% IOMake_Before_Redefinitions_2007_Detail.xlsx 
% IOUse_Before_Redefinitions_PRO_2007_Detail.xlsx 
% IOUse_Before_Redefinitions_PUR_2007_Detail.xlsx
% From US BEA, download at https://www.bea.gov/industry/more.htm
% Margins_Before_Redefinitions_2007_Detail.xlsx
%
% From Miller et al. 2019 
% Uk_det_*year*.csv, year = 2007/2012. 
% 
% From USEEIO publications, edited excel files
% Satellite files See SI, modified version of Ingwersen et al. 2017 (https://doi.org/10.23719/1365565)
% GHG_Satellite_1990-2016 2007IO.xlsm
% GHG_Satellite_1990-2016 2012IO.xlsm
% Energy_Satellite.xlsx
% Satellite_Minerals_2007.xlsx
% Satellite_Minerals_2012.xlsx

% clc; clear; close all
question1 = inputdlg('Do you want to make IO matrices for 2007 or 2012?');
year=str2double(question1{:});
addpath('Input_Files')
switch year
    case 2007
        [make_data] = xlsread('IOMake_Before_Redefinitions_DET.xlsx','2007');
        [use_data, txtUse, rawUse] = xlsread('IOUse_Before_Redefinitions_PRO_DET.xlsx','2007');
        use_data(isnan(use_data))=0;
        make_data(isnan(make_data))=0;
    case 2012
        [make_data] = xlsread('IOMake_Before_Redefinitions_DET.xlsx','2012');
        [use_data, txtUse, rawUse] = xlsread('IOUse_Before_Redefinitions_PRO_DET.xlsx','2012');
        use_data(isnan(use_data))=0;
        make_data(isnan(make_data))=0;        
end
%% extract data of interest from make and use files 
make=make_data(2:406,3:407); 
use=use_data(2:406,3:407);
comm_fd = use_data(2:406,409:428); % final demand for products/commodities
value_added=use_data(408:410,3:407);
ind_code=rawUse(6,3:407)';
com_code=rawUse(7:411,1);
ind_name=rawUse(5,3:407)';
com_name=rawUse(7:411,2);
com_name_short=rawUse(7:411,431);
labsZ=[string(com_code) com_name]; % commodities labels and names
labsInd=[string(ind_code) ind_name]; % industries labels and names
Zsort=cellstr(sortrows(labsZ,1));
labsV=rawUse(413:415,1:2);
labsY=txtUse(5,409:428);
y=[string(com_code) com_name num2cell(comm_fd)]; % final demand with sector numbers and names
q = use_data(2:406,430); % total output of products (commodities)
qzero=find(~q);
q(qzero)=1; % allows D to be created
x = use_data(412,3:407)'; % total output of industry
%% Define Coefficient Matrices
B = use/diag(x); % comm. by ind. coefficients: inputs of products to industry per output of industry
D =  make/diag(q); % in Miller Blair, D=V/(diag(q)) V = make. Market shares matrix 
Df=[D zeros(405,3);zeros(3,405) eye(3)];
q(qzero)=0; % reset commodities with zero output as zeros
%% Industry technology construct
A_itc=B*D; % product x product A matrix using the industry technology construct
Z_itc=A_itc*diag(q); % interindustry flow matrix
%% Prepare Ak matrix using fixed assets data
CFM = xlsread(['Uk_det_' question1{:} '.csv']); % load in Capital Use matrix, pe
CFM=CFM(2:406,3:407);
Bk=CFM/(diag(x));
Ak=Bk*D; % Capital Ak matrix. constructed using the ITC, A=B*D. It would be 
% preferable to have a D (market shares) matrix which represents the year the
% capital was formed, but that is not available to use at this point.
Akf=[Ak zeros(405,3); zeros(3,408)]; % new capital direct requirements matrix, 
% no capital consumption by household fuel consumption    
%% Load satellite file/stressor matrices
S=zeros(70,408); % 70 stressors (15 GHG, 10 energy, 45 materials)
[GHG, txtGHG, rawGHG] = xlsread(['GHG_Satellite_1990-2016 ' question1{:} 'IO.xlsm'],'US_all');
[GWP,~,rawGWP] = xlsread(['GHG_Satellite_1990-2016 ' question1{:} 'IO.xlsm'],'Spec');
PetRef_heat=GHG(416,5);
PetRef_fuel=GHG(417,5);
S(1:15,:)=GHG(5:412,35:49)';
SLabs=txtGHG(4,3:17)';
SLabs(:,2)={'kg/MillUSD'};

[E, txtE, rawE] = xlsread('Energy_Satellite.xlsx',['output' question1{:}]);
SLabs(16:25,1) = txtE(2,2:11)';
SLabs(16:25,2)={'MJ/MillUSD'};
for i=1:10
    for j=1:405
        for k = 1:size(rawE,1)
            if  string(rawE{k,1}) == labsInd(j,1) && string(rawE{2,1+i})==string(SLabs{15+i,1})
                S(15+i,j) = rawE{k,1+i};
            end
        end
    end
end
% Materials
[M, txtM, rawM] = xlsread(['Satellite_Minerals_' question1{:} '.xlsx'],'matrix');
SLabs(26:70,1) = txtM(2,2:46)';
SLabs(26:70,2)={'kg/MillUSD'};
for i=1:45
    for j=1:405
        for k = 1:size(rawM,1)
            if  string(rawM{k,1}) == labsInd(j,1) && string(rawM{2,1+i})==string(SLabs{25+i,1})
                S(25+i,j) = rawM{k,1+i};
            end
        end
    end
end
S(isnan(S))=0;

% 2012USD denominated stressors for 2007
if year==2007
    S12=zeros(70,408);
   [GHG2012USD, ~, ~] = xlsread(['GHG_Satellite_1990-2016 ' question1{:} 'IO.xlsm'],'US_all_2012USD');
   S12(1:15,:)=GHG2012USD(5:412,35:49)';
%    S12(1:15,:)=g12*Df;
   [E12, ~, rawE12] = xlsread('Energy_Satellite.xlsx','output2007_2012USD');
   for i=1:10
    for j=1:405
        for k = 1:size(rawE12,1)
            if  string(rawE12{k,1}) == labsInd(j,1) && string(rawE12{2,1+i})==string(SLabs{15+i,1})
                S12(15+i,j) = rawE12{k,1+i};
            end
        end
    end
   end
 
    [M12, txtM12, rawM12] = xlsread('Satellite_Minerals_2007.xlsx','matrix2012USD');
    for i=1:45
        for j=1:405
            for k = 1:size(rawM12,1)
                if  string(rawM12{k,1}) == labsInd(j,1) && string(rawM12{2,1+i})==string(SLabs{25+i,1})
                S12(25+i,j) = rawM12{k,1+i};
                end
            end
        end
    end
S12(isnan(S12))=0;
S12=S12*Df;
S12=1e6*S12;
end

S=S*Df;
S=1e6*S;
S(:,405)=0; % set impacts from RoW adjustment to Zero

% Define characterisation matrix
C=zeros(11,70); % 11 impact categories
C(1:3,1:8)=GWP(1:8,11:13)';
C(1:3,9)=GWP(11,11:13)'; % HFC-236fa
C(1:3,10:11)=GWP(13:14,11:13)';
C(1:3,12:13)=ones(3,1)*GWP(22:23,6)';
C(1:3,14:15)=GWP(17:18,11:13)';
C(4,16:19)=1; % non renewable energy
C(5,20:25)=1; % renewable and nuclear energy
C(6,16:25)=1; % total energy
C(7,26:44)=1; % metals
C(8,45:67)=1; % non-metallic minerals
C(9,68)=1; % fossil fuel peat
C(9,16)=1/22.99; % fossil fuel Coal kg/MJ converted to metric from 19.78 MMBtu/short ton https://www.eia.gov/tools/faqs/faq.php?id=72&t=2
C(9,17)=1/43.8; % fossil fuel Crude oil kg/MJ converted to metric from 5.8 MMBtu/bbl https://www.eia.gov/totalenergy/data/monthly/pdf/sec13_3.pdf
C(9,18)=1/50; % fossil fuel nat gas liq kg/MJ converted to metric, estimated by lowering nat gas dry value, due to presence of lower (MJ/kg) components such as ethane, pentane, etc
C(9,19)=1/53.23; % fossil fuel nat gas dry kg/MJ converted to metric from 1025 Btu/ft^3 https://www.eia.gov/totalenergy/data/monthly/pdf/sec13_5.pdf
C(10,69:70)=1; % biomass
C(11,26:70)=1; % all materials except fossil fuels
C(11,16:19)=C(9,16:19); % fossil fuels

CLabs=cell(11,2);
CLabs{1,1}='Climate Change impacts, AR4 impact factors';
CLabs{2,1}='Climate Change impacts, AR5 impact factors';
CLabs{3,1}='Climate Change impacts, AR5 impact factors w/ feedbacks';
CLabs(1:3,2)=[repmat({'kg CO2-eq'},3,1)];
CLabs{4,1}='Primary Energy, Non-renewable';
CLabs{5,1}='Primary Energy Equivalent, Renewable&nuclear';
CLabs{6,1}='Primary Energy, Total';
CLabs(4:6,2)=[repmat({'MJ'},3,1)];
CLabs{7,1}='Primary Material Extraction, Metal';
CLabs{8,1}='Primary Material Extraction, Non-metal mineral';
CLabs{9,1}='Primary Material Extraction, Fossil fuel';
CLabs{10,1}='Primary Material Extraction, Biomass';
CLabs{11,1}='Primary Material Extraction, Total';
CLabs(7:11,2)=[repmat({'kg'},5,1)];
%% Adapt A, Z, Y, V, x, D to include household activities
Z=[Z_itc zeros(405,3);zeros(3,408)];
Y=[comm_fd; zeros(3,20)];
Z(23,406)=Y(23,1); % allocate natural gas distribution to residential natural gas
Y(406,1)=Y(23,1); % replace natural gas distribution consumption with natural gas use consumption
Y(23,1)=0; % zero out the original consumption of natural gas distrubution
% allocate petroleum refineries to the different end uses
Z(238,407)=Y(238,1)*PetRef_heat; % res. petroleum fuel
Y(407,1)=Z(238,407); % personal consumption of res petroleum fuel
Z(238,408)=Y(238,1)*PetRef_fuel; % personal-vehicle fuel
Y(408,1)=Z(238,408); % personal consumption of personal vehicle fuel
Y(238,1)=0;
xn=sum(Z,2)+sum(Y,2); % new total commodity output vector
An=Z/diag(xn); % new direct requirements matrix
V=[value_added zeros(3,3)];
labsZ(406,1)='910010';
labsZ(406,2)='Residential natural gas';
labsZ(407,1)='910020';
labsZ(407,2)='Residential petroleum fuels';
labsZ(408,1)='920010';
labsZ(408,2)='Personal transport fuels';
labsZshort=labsZ;
labsZshort(1:405,2)=com_name_short;
%% create total requirements
I405=eye(405);
I408=eye(408);
Ls=inv(I405-A_itc);
Lf=inv(I408-An);
At=An+Akf;
Lt=inv(I408-At);
%% Extract Margins 
switch year
    case 2007
        [Ma, ~, Maraw] = xlsread('Margins_Before_Redefinitions_2007_2012_DET.xlsx','2007','A60085:AZ60366'); % import Margins tables related to final demand
        [use_pur, txtUse_pur, rawUse_pur] = xlsread('IOUse_Before_Redefinitions_PUR_Det.xlsx','2007');
    case 2012
        [Ma, ~, Maraw] = xlsread('Margins_Before_Redefinitions_2007_2012_DET.xlsx','2012','A60090:AZ60371'); % import Margins tables related to final demand
        [use_pur, txtUse_pur, rawUse_pur] = xlsread('IOUse_Before_Redefinitions_PUR_Det.xlsx','2012');
end
use_pur(isnan(use_pur))=0;
ComName=txtUse_pur(7:403,2);
ComNameShort=txtUse_pur(7:403,431);
ComLab=string(rawUse_pur(7:403,1));
comm_fd_pur=use_pur(2:398,409:428);

ComLab(398,1)='910010';
ComName(398,1)={'Residential natural gas'};
ComLab(399,1)='910020';
ComName(399,1)={'Residential petroleum fuels'};
ComLab(400,1)='920010';
ComName(400,1)={'Personal transport fuels'};

ComNameShort(398:400,1)=ComName(398:400,1);
%% Create transformation Matrix T to convert y between purchasers and producers prices
M1=Ma; % margins for column 1 of final demand, personal consumption expenditures
M1(isnan(M1))=0;
m1r=M1(:,3:6)./M1(:,7); % margins as ratios of purchasers prices
m1r(isnan(m1r))=0; m1r(isinf(m1r))=1; % remove NaNs and Infs
Tstr=[string(labsZ(:,1)) string(zeros(408,4))]; % predefine Transformation matrix
for i=1:408
for j=1:282
if Tstr(i,1)==string(Maraw(j,3)) % match commodity label
Tstr(i,2:5)=m1r(j,1:4); % margins for appropriate commodity
end
end
t=str2double(Tstr(:,2:5)); % Convert margins matrix to numeric, columns of 't' are Prod. value; Transp; Wholesale; Retail;. Ratio of purchasers value 
end
t(t>1)=1; % convert retail trade and transport ratios to 1
ts=t;
nopur=[282:288,290]; % retail commodity sectors that do not exist at purch prices: Motor vehicle/parts dealers; Food and bev stores; General merch stores; 
% %Building Mats; Health personal care stores; Gasoline; Clothing; Other RT
ts(nopur,:)=[];% remove the eight retail commodity sectors that do not exist at purch prices
% 
Ypur=[comm_fd_pur; zeros(3,20)];
Ypur(398,1)=Ypur(23,1); % allocate natural gas distribution to residential natural gas
Ypur(23,1)=0;
% allocate petroleum refineries to the different end uses
Ypur(399,1)=Ypur(238,1)*PetRef_heat; % res. petrol fuels
Ypur(400,1)=Ypur(238,1)*PetRef_fuel; % personal vehicle fuels
Ypur(238,1)=0;
T=zeros(408,400); % predfine final margins Transformation matrix
% allocate transport margins to transport sectors
air=M1(188,3)-M1(188,7); % difference between Producers and Purchasers value of Air transportation
rail=M1(189,3)-M1(189,7); % difference for rail transportation
water=M1(190,3)-M1(190,7); % difference for water transportation
truck=M1(191,3)-M1(191,7); % difference for truck transportation
pipe=M1(193,3)-M1(193,7); % difference for pipe transportation
sum_Trans=air+rail+water+truck+pipe; % total margin from transport sectors
T(291,:)=(air/sum_Trans)*ts(:,2)'; % allocate a certain portion of transportation margins to air transport based on air transports input to total transport 
T(292,:)=(rail/sum_Trans)*ts(:,2)'; % same for rail transport
T(293,:)=(water/sum_Trans)*ts(:,2)'; % same for water transport
T(294,:)=(truck/sum_Trans)*ts(:,2)'; % same for truck transport
T(296,:)=(pipe/sum_Trans)*ts(:,2)'; % same for pipe transport
%% allocate wholesale trade to sectors
Wmotor=M1(169,3);
Wprocomeq=M1(170,3);
Whhappel=M1(171,3);
Wmacheq=M1(172,3);
Wothdur=M1(173,3);
Wdrug=M1(174,3);
Wgroc=M1(175,3);
Wpetr=M1(176,3);
Wothnondur=M1(177,3);
Welmar=M1(178,3);
Wholesale=Wmotor+Wprocomeq+Whhappel+Wmacheq+Wothdur+Wdrug+Wgroc+Wpetr+Wothnondur+Welmar;
% allocate motor vehicle and parts supplies WTrade to 3361MV sectors
% Wmotor_rem=Wmotor-sum(M1(75:89,5))-M1(166,5); % this is the remainder of Wmotor which will be split among all non-accounted for sectors
margins=[1:168 197:203];
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(271,n)=(M1(m,5)*cell2mat(Maraw(m,10)))/M1(m,7);
        end
    end
end
% allocate ProfComm to appropriate sectors
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(272,n)=(M1(m,5)*cell2mat(Maraw(m,11)))/M1(m,7);
        end
    end
end
% allocate HH app to app. sectors
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(273,n)=(M1(m,5)*cell2mat(Maraw(m,12)))/M1(m,7);
        end
    end
end
% allocate Mach&eq to app. sectors
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(274,n)=(M1(m,5)*cell2mat(Maraw(m,13)))/M1(m,7);
        end
    end
end
% allocate Other durables
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(275,n)=(M1(m,5)*cell2mat(Maraw(m,14)))/M1(m,7);
        end
    end
end
% allocate Drugs
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(276,n)=(M1(m,5)*cell2mat(Maraw(m,15)))/M1(m,7);
        end
    end
end
% allocate Grocery
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(277,n)=(M1(m,5)*cell2mat(Maraw(m,16)))/M1(m,7);
        end
    end
end
% allocate Petrol
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(278,n)=(M1(m,5)*cell2mat(Maraw(m,17)))/M1(m,7);
        end
    end
end
T(278,399:400)=T(278,238);
% allocate other nondurables
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(279,n)=(M1(m,5)*cell2mat(Maraw(m,18)))/M1(m,7);
        end
    end
end
% allocate elec markets
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(280,n)=(M1(m,5)*cell2mat(Maraw(m,19)))/M1(m,7);
        end
    end
end
% remainder wholesale
for m= margins
    for n=1:397
        if string(Maraw{m,3})==ComLab(n) && M1(m,18)==0
           T(271:280,n)=M1(169:178,14)*ts(n,3);
        end
    end
end
T(271:280,394)=Ma(169:178,14)*ts(394,3); % scrap
T(271:280,395)=Ma(169:178,14)*ts(395,3); % used goods
%% allocate retail margins
Rmot=M1(179,3);
Rfood=M1(180,3);
Rgen=M1(181,3);
Rbuild=M1(182,3);
Rhealth=M1(183,3);
Rgas=M1(184,3);
Rcloth=M1(185,3);
Rnonstore=M1(186,8); % usually column 8 and 3 are the same, but not for nonstore retailers
Roth=Ma(187,3);
% mot vehicle and parts
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(282,n)=(M1(m,6)*cell2mat(Maraw(m,21)))/M1(m,7);
        end
    end
end
T(282,395)=(Rmot-M1(1:203,6)'*cell2mat(Maraw(1:203,21)))/M1(282,7); % assign as much of mot veh as possible to used goods,
% some other retail will also need to be allocated to used goods
usedrempc=1-((M1(179,3)-M1(1:168,19)'*M1(1:168,6))/M1(282,6));
% Food and beverage stores 
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(283,n)=(M1(m,6)*cell2mat(Maraw(m,22)))/(M1(m,7)*M1(180,19)/M1(180,8)); %scale down inputs by overshoot of food retail
        end
    end
end
% do gen merch below

% building and garden materials
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(285,n)=(M1(m,6)*cell2mat(Maraw(m,24)))/M1(m,7);
        end
    end
end
% health/personal care stores
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(286,n)=(M1(m,6)*cell2mat(Maraw(m,25)))/(M1(m,7)*M1(183,19)/M1(183,8)); %scale down inputs by overshoot of health stores
        end
    end
end
% gas stations
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(287,n)=(M1(m,6)*cell2mat(Maraw(m,26)))/(M1(m,7)*M1(184,19)/M1(184,8)); %scale down inputs by overshoot of gas stores
        end
    end
end
T(287,399:400)=T(287,238);
% clothing stores
for m=margins
    for n = 1:397
        if string(Maraw{m,3})==ComLab(n)
            T(288,n)=(M1(m,6)*cell2mat(Maraw(m,27)))/(M1(m,7)*M1(185,19)/M1(185,8)); %scale down inputs by overshoot of clothing stores
        end
    end
end
% make up residual/generic retail trade product
RRes=Rgen+(Rbuild-M1(1:168,22)'*M1(1:168,6))+Rnonstore+Roth;
% define portions of genertic retail trade product
rgenpc=Rgen/RRes;
rbldpc=(Rbuild-M1(1:168,22)'*M1(1:168,6))/RRes;
rnspc=Rnonstore/RRes;
rothpc=Roth/RRes;

% distribute generic retail trade to yet unassigned sectors
% general merchandise
for i = margins
    for j=1:397
        if Maraw{i,31}==0
            if string(Maraw{i,3})==ComLab(j)
                T(284,j)=rgenpc*ts(j,4);
            end
        elseif Maraw{i,31}<1
            if string(Maraw{i,3})==ComLab(j)
                T(284,j)=rgenpc*ts(j,4)*(1-Maraw{i,31});
            end
        end
    end
end
T(284,395)=rgenpc*usedrempc*ts(395,4); % used   
% Building
for i = margins
    for j=1:397
        if Maraw{i,30}==0
            if string(Maraw{i,3})==ComLab(j)
                T(285,j)=rbldpc*ts(j,4)';
            end
        elseif Maraw{i,31}<1
            if string(Maraw{i,3})==ComLab(j)
                T(285,j)=rbldpc*ts(j,4)*(1-Maraw{i,31});
            end
        end
    end
end
T(285,395)=rbldpc*usedrempc*ts(395,4); % used 
% Nonstore
for i = margins
    for j=1:397
        if Maraw{i,30}==0
            if string(Maraw{i,3})==ComLab(j)
                T(289,j)=rnspc*ts(j,4)';
            end
        elseif Maraw{i,31}<1
            if string(Maraw{i,3})==ComLab(j)
                T(289,j)=rnspc*ts(j,4)*(1-Maraw{i,31});
            end
        end
    end
end
T(289,395)=rnspc*usedrempc*ts(395,4); % used 
% Other retail
for i = margins
    for j=1:397
        if Maraw{i,30}==0
            if string(Maraw{i,3})==ComLab(j)
                T(290,j)=rothpc*ts(j,4)';
            end
        elseif Maraw{i,31}<1
            if string(Maraw{i,3})==ComLab(j)
                T(290,j)=rothpc*ts(j,4)*(1-Maraw{i,31});
            end
        end
    end
end
T(290,395)=rothpc*usedrempc*ts(395,4); % used 
% fill in diagonals of T
for k=1:281
    if t(k,1)>0
    T(k,k)=t(k,1);
    else
        T(k,k)=1;
    end
end
T(289,282)=1; % Nonstore retailers
for k=291:406
    if t(k,1)>0
    T(k,k-8)=t(k,1);
    else
       T(k,k-8)=1;
    end
end
% scrap
T(402,394)=M1(281,3)/M1(281,7);
% inputs to hh petroleum products
for k = 407:408
%     T(k,k-8)=1-sum(T(1:406,399));
    T(278:296,k-8)=T(278:296,238);
    T(k,k-8)=1-sum(T(1:406,399));
end
% check
yhh=Y(:,1);
yphh=Ypur(:,1);
yhhT=T*yphh;
ycomp=yhhT./yhh;
ycomp(isnan(ycomp))=0;
%% save basic matrices as structures
IO.A=An;
IO.Ak=Akf;
IO.At=At;
IO.L=Lf;
IO.Lt=Lt;
IO.Z=Z;
IO.Y=Y;
IO.YPur=Ypur;
IO.V=V;
IO.x=xn;
IO.S=S;
IO.C=C;
IO.F=S*diag(IO.x);
IO.T=T;
if isequal(year,2007)
    IO.S2012USD=S12;
end
meta.ZLabs=cellstr(labsZ);
meta.ZLabspur=[cellstr(ComLab) ComName ComNameShort];
meta.ZLabs_short=cellstr(labsZshort);
meta.YLabs=labsY';
meta.VLabs=labsV;
meta.CLabs=CLabs;
meta.SLabs=SLabs;
meta.Year=question1{:};
meta.IO_unit=strcat(string(year), ' Mill $');
%% clear and save
clearvars -except IO meta year 
save(['USEEIO_' num2str(year)],'IO', 'meta')