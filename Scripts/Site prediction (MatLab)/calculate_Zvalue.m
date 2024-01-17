function Z = calculate_Zvalue(grid,roads,sites)

global max_dist max_angle 

% create a 2d matrix with zeros to be used for storing the value at each
% grid point. The value is basically a number estimated u using te decay
% function. 

Z = zeros(size(grid.x,2),size(grid.y,2));  % z matrix


for k = 1:roads.n % loop through the holloways (hw)
      
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


%% other end of the hollow way 
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

end

%% Plot the z values using a surface plot

[X,Y] = meshgrid(grid.y,grid.x);

figure()
hist(Z(:)./max(Z(:)))

figure()
surf(X,Y,Z)
shading interp
hold on
line([roads.ys';roads.ye'],[roads.xs';roads.xe'],'Color','r')
view(90.5874,-90)
    plot(sites.y,sites.x,'o','MarkerSize',6,'Color','k')
    set(gca,'Color',[247 249 239]/255)
    axis equal
    colormap(autumn(5))

Z_new = Z ; 

Z_new(Z_new<(0.15*max(max(Z_new)))) = 0;

Zplot = Z_new;
Zplot(Zplot == 0) = nan;


figure()
surf(X,Y,Zplot)
shading interp
hold on
line([roads.ys';roads.ye'],[roads.xs';roads.xe'],'Color', [89 89 89]/255) %change color of hollow ways here (RGB/255)
view(90,-90)
    plot(sites.y,sites.x,'o','MarkerSize',6,'Color','k')
    %axis equal
    set(gca,'Color',[247 249 239]/255) %change background color here
    colormap(autumn) %change colormap scheme here

% 
% figure()
% plot(grid.X,grid.Y,'k.');
% axis equal
% hold on
% line([roads.xs(1) roads.xe(1)],[roads.ys(1) roads.ye(1)],'Color','g')
% plot(LP1(1),LP1(2),'g*') % start of hw
% plot(LP2(1),LP2(2),'r*') % end of hw
% % and now plot the closest grid points C2s C2e
% plot(NP.coord(:,1),NP.coord(:,2),'bo');
% plot(cLP1.coord(1),cLP1.coord(2),'go');
% plot(cLP2.coord(1),cLP2.coord(2),'ro');

end

