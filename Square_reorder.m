% script to reorder square matrices
% Peter Berrill May 24 2017
function Zfinal = Square_reorder(Z1,varargin)
% 
% Z1 = [0 1 4 2 3;1 2 4 5 2; 4 3 2 1 3; 2 1 5 4 1; 3 3 3 6 2;];

switch nargin
    case 1
% if (nargin < 2)
    row_names='';
    
Zsortr=sortrows(Z1);
ZsortrT=Zsortr';
Zsorted=sortrows(ZsortrT);
Zfinal=Zsorted';
% else 
    case 2
    row_names = varargin{1};
Z1=[num2str(0) row_names';row_names num2cell((Z1))];    
Zsortr=sortrows(Z1(2:end,:));
Zsortr=[Z1(1,:);Zsortr];
ZsortrT=Zsortr';
Zsorted=sortrows(ZsortrT(2:end,:));
Zsorted=[ZsortrT(1,:);Zsorted];
Zfinal=Zsorted';

    case 3
row_names = varargin{1};
column_names=varargin{2};
if size(column_names,2)>size(column_names,1) 
Z1=[num2str(0) column_names;row_names num2cell((Z1))];   
else
Z1=[num2str(0) column_names';row_names num2cell((Z1))];   
end
Zsortr=sortrows(Z1(2:end,:));
Zsortr=[Z1(1,:);Zsortr];
ZsortrT=Zsortr';
Zsorted=sortrows(ZsortrT(2:end,:));
Zsorted=[ZsortrT(1,:);Zsorted];
Zfinal=Zsorted';        
end
   
end

