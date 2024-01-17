%% This script is used to test the decay function. Find the maximum and minimum values. 

% To do that we use the first holloway. We go through the procedure to
% create the grid and find the neighbors and then calculate then evaluate
% the decay function at the neighboring points. 

%% 


global max_dist max_angle 

% create a 2d matrix with zeros to be used for storing the value at each
% grid point. The value is basically a number estimated u using te decay
% function. 

Z = zeros(size(grid.x,2),size(grid.y,2));  % z matrix

% Take only the first holloway

k = 10;
      
    % for each holloway we find its start and end point
    LP1 = [roads.xs(k) roads.ys(k)]; % line point 1
    LP2 = [roads.xe(k) roads.ye(k)]; % line point 2

    % then we start using the first point (the start) and we then: 
    
    % 1) find the point C which is the closest grid point to point LP1, we
    % name this point cLP1.

    % closest grid point to start point  of the hw
    [~,index_x] = min(abs(grid.x-LP1(1)));
    [~,index_y] = min(abs(grid.y-LP1(2)));
    cLP1.indcs = [index_x index_y]; % indices of the closest grid point to LP1
    cLP1.coord = [grid.x(index_x) grid.y(index_y)] ; % coordinates of the closest grid point to LP1

    % 2) find the point C closest grid point to point LP2, we name this
    % point cLP2.
    [~,index_x] = min(abs(grid.x-LP2(1))); 
    [~,index_y] = min(abs(grid.y-LP2(2)));  
    cLP2.indcs = [index_x index_y]; % indices of the closest grid point to LP2
    cLP2.coord = [grid.x(index_x) grid.y(index_y)]; % coordinates of the closest grid point to LP2

%% At each end of the holloway we find the area around that point
Z = zeros(size(grid.x,2),size(grid.y,2));  % z matrix

% and we estimate the values Z in this area. 

% At the one end of the hollow way

NP = near_points(cLP1.indcs,grid); % this function is used to find the grid points
                                   % around the point cLP1 in a square of
                                   % distance = max_distance (defined in
                                   % main)

[angle_NP,dist_NP] = calculate_angle_distance(LP1,LP2,NP,grid,roads,0);% this function calculates the angle of the neigboring 
                                                                                 % grid points (NP) to the vector from p2 to p1, 
                                                                                 % and the distance from P1 to the neighboring grid points                                                        

neigh_values = eval_decay_function(angle_NP,dist_NP);                                                         

% here we add the estimated vaues to the Z matrix which has the sum of all
% calues. 
for i=1:size(neigh_values,1)
    
Z(NP.indcs(i,1),NP.indcs(i,2)) = Z(NP.indcs(i,1),NP.indcs(i,2)) + neigh_values(i,:);
end


%% plot the values


[X,Y] = meshgrid(unique(NP.coord(:,2)),unique(NP.coord(:,1)));
ZZ = Z(unique(NP.indcs(:,1)),unique(NP.indcs(:,2)));

figure()
surf(X,Y,ZZ)
shading interp
hold on
line([roads.ys(k);roads.ye(k)],[roads.xs(k);roads.xe(k)],'Color','r','LineWidth',5)
view(90.5874,-90)
axis equal
% plot(sites.y,sites.x,'o','MarkerSize',6,'Color','k')
colorbar
title(['Max value = ' num2str(max(max(ZZ)))])






%% other end of the hollow way 

Z = zeros(size(grid.x,2),size(grid.y,2));  % z matrix

 NP = near_points(cLP2.indcs,grid);

[angle_NP,dist_NP] = calculate_angle_distance(LP2,LP1,NP,grid,roads,0);% this function calculates the angle of the neigboring 
                                                                       % grid points (NP) to the vector from p1 to p2, 
                                                                       % and the distance from P2 to the neighboring grid points                                                        

neigh_values = eval_decay_function(angle_NP,dist_NP);                                                         

% here we add the estimated vaues to the Z matrix which has the sum of all
% calues. 
for i=1:size(neigh_values,1)
    
Z(NP.indcs(i,1),NP.indcs(i,2)) = Z(NP.indcs(i,1),NP.indcs(i,2)) + neigh_values(i,:);
end



%% Plot the z values using a surface plot

[X,Y] = meshgrid(unique(NP.coord(:,2)),unique(NP.coord(:,1)));
ZZ = Z(unique(NP.indcs(:,1)),unique(NP.indcs(:,2)));

figure()
% subplot(1,2,1)
surf(X,Y,ZZ)
shading interp
hold on
line([roads.ys(k);roads.ye(k)],[roads.xs(k);roads.xe(k)],'Color','r','LineWidth',5)
view(90.5874,-90)
axis equal
% plot(sites.y,sites.x,'o','MarkerSize',6,'Color','k')
colorbar
title(['Max value = ' num2str(max(max(ZZ)))])

% subplot(1,2,2)
figure()
hist(ZZ(:))
title('Histogram of the values')