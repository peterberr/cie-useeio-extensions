
CFinv=R07.sum_exp_inv(:,5:8)*1e-9;
CFcon=R07.sum_con_sec(:,5:8)*1e-9;
CF=R07.sum_con_sec(:,1:4)*1e-9;
EFinv=R07.EF_exp_inv(:,5:8)*1e-9;
EFcon=R07.EF_con_sum(:,5:8)*1e-9;
EF=R07.EF_con_sum(:,1:4)*1e-9;
MFinv=R07.MF_exp_inv(:,5:8)*1e-9;


load('Results_2012.mat')
CFinv=R12.sum_exp_inv(:,5:8)*1e-9;
CFcon=R12.sum_con_sec(:,5:8)*1e-9;
CF=R12.sum_con_sec(:,1:4)*1e-9;
EFinv=R12.EF_exp_inv(:,5:8)*1e-9;
EFcon=R12.EF_con_sum(:,5:8)*1e-9;
EF=R12.EF_con_sum(:,1:4)*1e-9;
MFinv=R12.MF_exp_inv(:,5:8)*1e-9;
MFcon=R12.MF_con_sum(:,5:8)*1e-9;
MF=R12.MF_con_sum(:,1:4)*1e-9;