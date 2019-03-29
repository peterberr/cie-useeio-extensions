% sector_reduce function for aggregating sectors
function [output, labels] = sector_reduce(seclab, sec_sum, sum_con)

x=size(seclab,1);
y=size(sum_con,2);
sec_imp=zeros(x,y);
for i=1:size(sum_con,1)
    for j=1:x
        if seclab(j,1)==sec_sum(i,2)
            sec_imp(j,:)=sec_imp(j,:)+sum_con(i,1:y);
        end
    end
end
output=sec_imp;
labels=seclab(:,2);
% output=[sec_imp(1:8,:); sec_imp(9,:)+sec_imp(10,:)+sec_imp(13,:); sec_imp(11:12,:); sec_imp(14:end,:)]; % add retail trade sectors together
% labels=[seclab(1:9,2); seclab(11:12,2); seclab(14:end,2)]; 
% labels=[seclab(1:9,2); seclab(11:12,2); seclab(14:end,2)];% combine labels