function [t] = decay_function

global function_id b R1 R2
% function 1 
if function_id == 1
%     b = 750; 
%     R1 = 250;
%     R2 = 250;
    t = @(r,a)((1./(1+exp((a-R1)/b))).*(1./(1+exp((r-R2)/b))));
elseif function_id == 2
    'No decay function specified for this function id'
else if function_id ==3
    'No decay function specified for this function id'

end

    
%    sigma = 1000;
%      R = 10 ; 
   

%     lampda = 5;
%      t = @(r,a)(cos(deg2rad(a)).*exp((-r.^2)/sigma));
%     t = @(r,a)(cos(deg2rad(lampda*a)).*(1./(1+exp((r-R)/b))));
end

