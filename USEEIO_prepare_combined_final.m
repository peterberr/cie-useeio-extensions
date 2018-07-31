% Finalised script for preparing USEEIO matrices with capital for 2007 and
% 2013 
% Peter Berrill, July 17 2018

% Required input files:
% From US BEA, download at https://www.bea.gov/industry/io_annual.htm
% IOMake_Before_Redefinitions_2007_Detail.xlsx 
% IOUse_Before_Redefinitions_PRO_2007_Detail.xlsx 
% IOUse_Before_Redefinitions_PUR_2007_Detail.xlsx
% From US BEA, download at https://www.bea.gov/industry/more.htm
% Margins_Before_Redefinitions_2007_Detail.xlsx
%
% From Miller et al. 2018 SI XXX
% Uk_det_*year*.csv, year = 2007/2013. 
% 
% From USEEIO publications
% USEEIOv1.1_Matrices_pb.xlsx, See SI1.docx for description of modifications to Ingwersen 2017(https://doi.org/10.23719/1369615)
% Satellite files See SI XXX, modified version of Ingwersen et al. 2017 (https://doi.org/10.23719/1365565)
% USEEIOv1.1_Satellite_GHG_SI_Peteredit2007.xlsx, 
% USEEIOv1.1_Satellite_GHG_SI_Peteredit2013.xlsx
% USEEIOv1.1_Satellite_Energy_Peteredit_2007.xlsx
% USEEIOv1.1_Satellite_Energy_Peteredit_2013.xlsx
% USEEIOv1.1_Satellite_Minerals_Peteredit.xlsx
% USEEIOv1.1_Satellite_Minerals_Peteredit_2013.xlsx
%
% From QP SUT projections (Kondo)
% US_SUT_balance_results_04_basic.xlsx

% Original functions called by this script:
% Square_reorder.m

clc; clear
question1 = inputdlg('Do you want to make A matrices for 2007 or 2013?  ');
year=str2double(question1{:});

switch year
    case 2007
        [make_data] = xlsread('IOMake_Before_Redefinitions_2007_Detail.xlsx','2007');
        [use_data, txtUse, rawUse] = xlsread('IOUse_Before_Redefinitions_PRO_2007_Detail.xlsx','2007');
        use_data(isnan(use_data))=0;
        make_data(isnan(make_data))=0;
    case 2013
        % insert 2013 tables here
        [SUT2013, txtSUT2013, rawSUT2013]=xlsread('US_SUT_balance_results_04_basic.xlsx','Detail_QP');
end

%% extract data of interest
switch year
    case 2007
        make=make_data(2:390,3:391); 
        use=use_data(2:390,3:391);
        comm_fd = use_data(2:390,393:412); % final demand for products/commodities
        value_added=use_data(392:394,3:391);

        ind_lab=rawUse(5,3:391)';
        com_lab=rawUse(6:394,1);
        ind_name=rawUse(4,3:391)';
        com_name=rawUse(6:394,2);
        labsZ=[string(com_lab) com_name]; % commodities labels and names
        Zsort=cellstr(sortrows(labsZ,1));
        labsV=txtUse(396:398,1:2);
        labsY=txtUse(4,393:412);
        y=[string(com_lab) com_name num2cell(comm_fd)]; % final demand with sector numbers and names

        q = use_data(2:390,414); % total output of products (commodities)
        qzero=find(~q);
        q(qzero)=1; % allows D to be created
        x = use_data(396,3:391)'; % total output of industry
    case 2013
          make=SUT2013(782:1170,4:392);
          use_dom=SUT2013(4:392,393:781);
          use_imp=SUT2013(393:781,393:781);
          use=use_dom+use_imp;
          comm_fd_dom=SUT2013(4:392,782:801);
          comm_fd_imp=SUT2013(393:781,782:801);
          comm_fd=comm_fd_dom+comm_fd_imp;
          comm_fd(:,8)=comm_fd(:,8)+((sum(use_imp,2)+sum(comm_fd_imp,2))*-1); % add imports into relevant column of final demand
          value_added=SUT2013(1171:1173,393:781);
          
          ind_lab=rawSUT2013(3,394:782)';
          com_lab=rawSUT2013(5:393,3);
          ind_name=rawSUT2013(4,394:782)';
          com_name=rawSUT2013(5:393,4);
          labsZ=[string(com_lab) com_name]; % commodities labels and names
          Zsort=cellstr(sortrows(labsZ,1));
          labsV=rawSUT2013(1172:1174,3:4);
          labsY=rawSUT2013(4,783:802);
          y=[string(com_lab) com_name num2cell(comm_fd)]; % final demand with sector numbers and names

          q = sum(use,2)+sum(comm_fd,2); % total output of products (commodities)
          qzero=find(~q);
          q(qzero)=1; % allows D to be created
          x = SUT2013(782:1170,802); % total output of industry
end  

%% Define Coefficient Matrices
B = use/diag(x); % comm. by ind. coefficients: inputs of products to industry per output of industry
D =  make/diag(q); % in Miller Blair, D=V/(diag(q)) V = make. Market shares matrix 
q(qzero)=0; % reset commodities with zero output as zeros
%% Industry technology construct
A_itc=B*D; % product x product A matrix using the industry technology construct
Z_itc=A_itc*diag(q); % interindustry flow matrix
%% Sort files        
sorty=sortrows(y,1);
Y=str2double(sorty(:,3:22));
xprep=[string(com_lab) num2cell(q)]; % 
sortx=sortrows(xprep,1); % Really sortq, but x used in later scripts to describe commodities. so x here is output of commodities whereas xind is output of industries
xind_prep=[string(ind_lab) num2cell(x)];
sortx_ind=sortrows(xind_prep,1);

va=value_added*D; % convert value added from per-industry to per-commodities
V=[string(ind_lab)'; ind_name'; num2cell(va)];
% V_unsort=V;
VT=V'; % transpose V to allow sorting by labels
Vsort=sortrows(VT,1)';
v_norm=str2double(Vsort(3:5,:))/diag(str2double(sortx(:,2)));
sortV=str2double(Vsort(3:5,:));

D_unsort=D;
Dsort=Square_reorder(D,string(ind_lab),string(com_lab)');
D=str2double(Dsort(2:390,2:390));

A_sort=Square_reorder(A_itc, string(com_lab));
A=str2double(A_sort(2:end,2:end));

Z_sort=Square_reorder(Z_itc, string(com_lab));
Z=str2double(Z_sort(2:end,2:end));

%% Prepare Ak matrix using fixed assets data
CFM = xlsread(['DIG FAA/Uk_det_' question1{:} '.csv']); % load in Capital Use matrix, pe

CFM=CFM(2:390,4:392);
CFM_sort=Square_reorder(CFM,string(com_lab),string(ind_lab));
CFM_num=str2double(CFM_sort(2:390,2:390)); % only numeric data without labels

x_sort=str2double(sortx_ind(:,2)); % outputs of industry 
Bk=CFM_num/(diag(x_sort));
Ak=Bk*D; % Capital Ak matrix. constructed using the ITC, A=B*D. It would be 
% preferable to have a D (market shares) matrix which represents the year the
% capital was formed, but that is not available to use at this point.

%% Load stressor and characterisation matrices from Yang and Wengersen source files
[C, txtC, ~] = xlsread('USEEIOv1.1_Matrices_pb.xlsx' , 'C'); % load edited characterisation matrix - includes modifications to material impact categories
S = xlsread('USEEIOv1.1_Matrices_pb.xlsx' , 'B'); % load edited stressor matrix - includes fuel wood and industrial roundwood biomass material extractions
S=[S(:,1:383) zeros(1877,1) S(:,384:388)]; % add in a stressor for scrap (all zeros)
S=[S zeros(1877,3)]; % add in stressors for the three direct emissions sectors

% Update stressor matrices
addpath('USEEIO_Satellite_Tables')

switch year 
    case 2007
        S_2013USD=S;
        %GHG
        [GHG, ~, GHGraw] = xlsread('USEEIOv1.1_Satellite_GHG_SI_Peteredit2007.xlsx','US_all');
        lab=GHGraw(3:391,2); % industry labels from satellite tables
        lab=string(lab);
        GHGmat=GHG(1:389,34:48);
        Shh=GHG(390:392,34:48); % household fuel direct emissions
        Shh(isnan(Shh))=0;
        SGHG=[lab string(GHGmat)]; % GHGmat is the numeric matrix of GHG emissions
        S_sort=sortrows(SGHG,1);
        Sghg=str2double(S_sort(:,2:16))';
        Sghg=Sghg*D; % convert from industry to commodity emissions
        Sghg=[Sghg Shh']; % add in household emissions
        order=[1;3;2;10;6;5;4;7;8;11;12;14;15;9;13]; % reorder GHGs to match with order in S file from USEEIOv1.1_Matrices
        Scsort=sortrows([order Sghg],1);
        Sc=Scsort(:,2:end);
        S(11:25,:)=Sc; % replace portion of S matrix with GHG emissions using 2007 data
    
        % create intensities per 2013USD
        [G13] = xlsread('USEEIOv1.1_Satellite_GHG_SI_Peteredit2007.xlsx','US_all_2013USD');
        G13mat=G13(1:389,34:48);
        Shh13=G13(390:392,34:48); % household fuel direct emissions
        Shh13(isnan(Shh13))=0;
        SGHG13=[lab string(G13mat)]; % GHGmat is the numeric matrix of GHG emissions
        S_sort13=sortrows(SGHG13,1);
        Sghg13=str2double(S_sort13(:,2:16))';
        Sghg13=Sghg13*D; % convert from industry to commodity emissions
        Sghg13=[Sghg13 Shh13']; % add in household emissions
        order=[1;3;2;10;6;5;4;7;8;11;12;14;15;9;13]; % reorder GHGs to match with order in original S file from USEEIOv1.1_Matrices
        Scsort13=sortrows([order Sghg13],1);
        Sc13=Scsort13(:,2:end);
        S_2013USD(11:25,:)=Sc13; % replace portion of S matrix with GHG emissions using 2007 data
        
        %Energy
        [E, ~, Eraw]= xlsread('USEEIOv1.1_Satellite_Energy_Peteredit_2007.xlsx','matrix_2007');
        Efull=zeros(389,9);
        E(isnan(E))=0;
        estr=[string(Eraw(3:end,1)) string(E(:,2:end))];
        % create matrix of stressors per sector
        for i=1:389
            for j=1:64
                for k=1:9
                   if isequal(estr(j,1),string(Zsort{i,1}))
                      Efull(i,k)=str2double(estr(j,k+1));
                   end
                end
            end
        end
        Ec=Efull'*D; % convert from ind to comm
        S(2:10,1:389)=Ec; % replace Energy portion of S with 2007 based values
        % create intensities per 2013USD
        [E13, ~, Er13]= xlsread('USEEIOv1.1_Satellite_Energy_Peteredit_2007.xlsx','matrix_2013');
        E13full=zeros(389,9);
        E13(isnan(E13))=0;
        es13=[string(Er13(3:end,1)) string(E13(:,2:end))];
        % create matrix of stressors per sector
        for i=1:389
            for j=1:64
                for k=1:9
                   if isequal(es13(j,1),string(Zsort{i,1}))
                      E13full(i,k)=str2double(es13(j,k+1));
                   end
                end
            end
        end
        Ec13=E13full'*D; % convert from ind to comm
        S_2013USD(2:10,1:389)=Ec13; % replace Energy portion of S with 2007 based values
        
        % Materials
        [M, ~, Mraw] = xlsread('USEEIOv1.1_Satellite_Minerals_Peteredit.xlsx','newmatrix_2007');
        Mfull=zeros(389,45);
        M(isnan(M))=0;
        mstr=[string(Mraw(4:end,2)) string(M(3:end,2:end))];
        %create matrix of stressors per sector
        for i=1:389
            for j=1:size(mstr,1)
                for k =1:45
                    if isequal(mstr(j,1),string(Zsort{i,1}))
                        Mfull(i,k)=str2double(mstr(j,k+1));
                    end
                end
            end
        end
        Mc=Mfull'*D;
        S(41:85,1:389)=Mc; % replace Materials portion of S with 2007 based values
        % create intensities per 2013USD
        [M13, ~, Mr13] = xlsread('USEEIOv1.1_Satellite_Minerals_Peteredit.xlsx','newmatrix_2013');
        M13full=zeros(389,45);
        M13(isnan(M13))=0;
        ms13=[string(Mr13(4:end,2)) string(M13(3:end,2:end))];
        %create matrix of stressors per sector
        for i=1:389
            for j=1:size(mstr,1)
                for k =1:45
                    if isequal(ms13(j,1),string(Zsort{i,1}))
                        M13full(i,k)=str2double(ms13(j,k+1));
                    end
                end
            end
        end
        Mc13=M13full'*D;
        S_2013USD(41:85,1:389)=Mc13;
        
    case 2013
        [GHG, ~, GHGraw] = xlsread('USEEIOv1.1_Satellite_GHG_SI_Peteredit2013.xlsx','US_all');
        lab=GHGraw(3:391,2); % industry labels from satellite tables
        lab=string(lab);
        GHGmat=GHG(1:389,34:48);
        Shh=GHG(390:392,34:48); % household fuel direct emissions
        Shh(isnan(Shh))=0;
        SGHG=[lab string(GHGmat)]; % GHGmat is the numeric matrix of GHG emissions
        S_sort=sortrows(SGHG,1);
        Sghg=str2double(S_sort(:,2:16))';
        Sghg=Sghg*D; % convert from industry to commodity emissions
        Sghg=[Sghg Shh']; % add in household emissions
        order=[1;3;2;10;6;5;4;7;8;11;12;14;15;9;13]; % reorder GHGs to match with order in original S file from USEEIOv1.1_Matrices
        Scsort=sortrows([order Sghg],1);
        Sc=Scsort(:,2:end);
        S(11:25,:)=Sc; % replace portion of S matrix with GHG emissions with 07 data    
      
        %Energy
        [E, ~, Eraw]= xlsread('USEEIOv1.1_Satellite_Energy_Peteredit_2013.xlsx','matrix');
        Efull=zeros(389,9);
        E(isnan(E))=0;
        estr=[string(Eraw(3:end,1)) string(E(:,2:end))];
        % create matrix of stressors per sector
        for i=1:389
            for j=1:64
                for k=1:9
                   if isequal(estr(j,1),string(Zsort{i,1}))
                      Efull(i,k)=str2double(estr(j,k+1));
                   end
                end
            end
        end
        Ec=Efull'*D; % convert from ind to comm
        S(2:10,1:389)=Ec;   
        %Materials
        [M, ~, Mraw] = xlsread('USEEIOv1.1_Satellite_Minerals_Peteredit_2013.xlsx','newmatrix');
        Mfull=zeros(389,45);
        M(isnan(M))=0;
        mstr=[string(Mraw(4:end,2)) string(M(3:end,2:end))];
        %create matrix of stressors per sector
        for i=1:389
            for j=1:size(mstr,1)
                for k =1:45
                    if isequal(mstr(j,1),string(Zsort{i,1}))
                        Mfull(i,k)=str2double(mstr(j,k+1));
                    end
                end
            end
        end
        Mc=Mfull'*D;
        S(41:85,1:389)=Mc;
end
S=1e6*S;
%% Adapt A, Z, Y, V, x, D to include household activities
Z=[Z zeros(389,3);zeros(3,392)];
Y=[Y; zeros(3,20)];
Z(23,390)=Y(23,1); % allocate natural gas distribution to residential natural gas
Y(390,1)=Y(23,1); % replace natural gas distribution consumption with natural gas use consumption
Y(23,1)=0; % zero out the original consumption of natural gas distrubution
% allocate petroleum refineries to the different end uses
Z(88,391)=Y(88,1)*GHG(401,6); % res. petroleum fuel
Y(391,1)=Z(88,391); % personal consumption of res petroleum fuel
Z(88,392)=Y(88,1)*GHG(400,6); % personal-vehicle fuel
Y(392,1)=Z(88,392); % personal consumption of personal vehicle fuel
Y(88,1)=0;
xn=sum(Z,2)+sum(Y,2); % new total commodity output vector
An=Z/diag(xn); % new direct requirements matrix
V=[sortV zeros(3,3)];
Ak=[Ak zeros(389,3); zeros(3,392)]; % new capital direct requirements matrix, no capital consumption by household fuel consumption    
%% Margins 
[Ma, ~, Maraw] = xlsread('Margins_Before_Redefinitions_2007_Detail.xlsx','2007','A54330:J55971'); % import Margins tables related to final demand
[use_pur, txtUse_pur, rawUse_pur] = xlsread('IOUse_Before_Redefinitions_PUR_2007_Detail.xlsx','2007');
use_pur(isnan(use_pur))=0;
CommName=txtUse_pur(6:391,2);
CommLab=string(rawUse_pur(6:391,1));
comm_fd_pur=use_pur(2:387,393:412);

M1=Ma(1:266,:); % margins for column 1 of final demand, personal consumption expenditures
m1r=M1(:,3:6)./M1(:,7); % margins as ratios of purchasers prices
m1r(isnan(m1r))=0; m1r(isinf(m1r))=1; % remove NaNs and Infs
Tstr=[string(Zsort(:,1)) string(zeros(389,4))]; % predefine Transformation matrix
for i=1:389
for j=1:266
if Tstr(i,1)==string(Maraw(j,4)) % match commodity label
Tstr(i,2:5)=m1r(j,1:4); % margins for appropriate commodity
end
end
t=str2double(Tstr(:,2:5)); % Convert margins matrix to numeric, columns of 't' are Prod. value; Transp; Wholesale; Retail;. Ratio of purchasers value 
end
t(t>1)=1; % convert retail trade and transport ratios to 1
ts=t;
ts(275:277,:)=[];% remove the three retail commodity sectors that do not exist at purch prices, i.e. Motor vehicle/parts dealers; Food and bev stores; General merch stores

ZLab=[cellstr(CommLab) CommName];
zsort=sortrows(ZLab);

Yp=[string(CommLab) string(comm_fd_pur)];
ysort=sortrows(Yp);
Ypur=[str2double(ysort(:,2:21)); zeros(3,20)];
Ypur(387,1)=Ypur(23,1); % allocate natural gas distribution to residential natural gas
Ypur(23,1)=0;
% allocate petroleum refineries to the different end uses
Ypur(388,1)=Ypur(88,1)*GHG(401,6); % res. petrol fuels
Ypur(389,1)=Ypur(88,1)*GHG(400,6); % personal vehicle fuels
Ypur(88,1)=0;

T=zeros(389,386); % predfine final margins Transformation matrix
% allocate transport margins to transport sectors
air=M1(175,3)-M1(175,7); % difference between Producers and Purchasers value of Air transportation
rail=M1(176,3)-M1(176,7); % difference for rail transportation
water=M1(177,3)-M1(177,7); % difference for water transportation
truck=M1(178,3)-M1(178,7); % difference for truck transportation
pipe=M1(180,3)-M1(180,7); % difference for truck transportation
sum_Trans=air+rail+water+truck+pipe; % total margin from transport sectors
T(278,:)=(air/sum_Trans)*ts(:,2)'; % allocate a certain portion of transportation margins to air transport based on air transports input to total transport 
T(279,:)=(rail/sum_Trans)*ts(:,2)'; % same for rail transport
T(280,:)=(water/sum_Trans)*ts(:,2)'; % same for water transport
T(281,:)=(truck/sum_Trans)*ts(:,2)'; % same for truck transport
T(283,:)=(pipe/sum_Trans)*ts(:,2)'; % same for pipe transport
% allocate retail margins to retail sectors
motor=M1(171,3);
food=M1(172,3);
general=M1(173,3);
other=M1(174,3)-M1(174,7);
sum_Retail=motor+food+general+other;
T(275,:)=(motor/sum_Retail)*ts(:,4)'; % distribute inputs of motor dealers to final demand
T(276,:)=(food/sum_Retail)*ts(:,4)'; % food and beverage stores
T(277,:)=(general/sum_Retail)*ts(:,4)'; % general merch stores
T(288,:)=(other/sum_Retail)*ts(:,4)'; % Other retail
% allocate wholesale margins
T(274,:)=ts(:,3)'; % Wholesale trade

% need to add rows to T so that its compatible with 392 sectors.
t=[t;repmat([1,0,0,0],3,1)]; % three new fuel use sectors, all have no margins
T=[T zeros(389,3); zeros(3,389)]; % three new fuel use sectors, all have no margins

for k=1:274
    T(k,k)=t(k,1);
end

for k=278:392
    T(k,k-3)=t(k,1);
end
% assign same margins to residential petroleum fuels and vehicle fuels as
% Petroluem refineries margins
T(:,388)=T(:,88);
T(391,388)=T(88,388);
T(88,388)=0;

T(:,389)=T(:,88);
T(392 ,389)=T(88,389);
T(88,389)=0;
%% sort into structures
IO.A=An;
IO.Ak=Ak;
IO.Z=Z;
IO.Y=Y;
IO.x=xn;
IO.V=V;
IO.C=C;
IO.S=S;
if isequal(year,2007)
    IO.S_2013USD=S_2013USD;
    IO.Ypur=Ypur;
end
IO.F=S*diag(IO.x);

IO.T=T;

Zsort=[Zsort; [GHGraw(392:394,2) GHGraw(392:394,1)]];
Zsortpur=[zsort; [GHGraw(392:394,2) GHGraw(392:394,1)]];
meta.ZLabs=Zsort;
meta.ZLabs_purch_price=Zsortpur;
meta.YLabs=labsY;
meta.VLabs=labsV;
% meta.MarketShares=Dsort;
meta.CLabs=txtC(2:25,1);
meta.FLabs=txtC(1,2:1878)';
meta.year=string(year);
meta.IO_unit=strcat(string(year), ' Mill $');
meta.S_unit = strcat('impact per ', string(year), ' Mill $');
meta.version='7';
meta.Capital_OwnOrUse='use';
%% save
% clearvars -except IO meta year Aug 
save(['USEEIO_v7_' num2str(year) '_'   meta.Capital_OwnOrUse])