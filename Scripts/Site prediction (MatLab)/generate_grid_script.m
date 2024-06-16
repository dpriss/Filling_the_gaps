
    global dx dy space_frac
    
    % define the boundaries of the area/site based on the most left/right
    % roads and top/bottom roads.

    Xmin_roads = min([roads.xs ; roads.xe]); % we take the min x distance (left most holloway)
    Xmax_roads = max([roads.xs ; roads.xe]); % we take the max x distance (right most holloway)

    Ymin_roads = min([roads.ys;roads.ye]);   % we take the min y distance (lowest holloway)
    Ymax_roads = max([roads.ys;roads.ye]);   % we take the max y distance (highest holloway)
    
    % add some space/margins around the area
    % margin can be a fraction of the total length of each direction
    
    x_length = Xmax_roads-Xmin_roads;
    y_length = Ymax_roads - Ymin_roads;
    
    % add margins
    x_margin = x_length*space_frac;
    y_margin = y_length*space_frac;
    
    x_boundaries = [Xmin_roads - x_margin Xmax_roads + x_margin];
    
    y_boundaries = [Ymin_roads - y_margin Ymax_roads + y_margin];
    
    % define the resolution of the grid based on the minimum distance
    % between the sites.
    
   % Sites_loc = [sites.x sites.y]; 
    
%     Roads_starts_loc = [roads.xs roads.ys];
%     Roads_ends_loc = [roads.xe roads.ye];
%     Roads_points_loc = [Roads_starts_loc;Roads_ends_loc]; 
    
    
%     eu_dis_sites = pdist2(Sites_loc,Sites_loc);
%     eu_dis_sites(eu_dis_sites==0) = nan ;
%     min_eu_dist_sites = min(eu_dis_sites(:)) ; 
%     
%     eu_dist_between_road_pnts = pdist2(Roads_points_loc,Roads_points_loc);
%     eu_dist_between_road_pnts(eu_dist_between_road_pnts==0) = nan ; 
%     mink(eu_dist_between_road_pnts(:),1000);
%     
%     figure()
%     hist(ans)
    
%     dx = 100;
%     dy = 100;
%     
    
    x_grid = x_boundaries(1):dx:x_boundaries(2);
    y_grid = y_boundaries(1):dy:y_boundaries(2);
    
    [X,Y] = meshgrid(x_grid,y_grid);
    
    grid.x_bound = x_boundaries;
    grid.y_bound = y_boundaries ;
    grid.dx = dx;
    grid.dy = dy;
    grid.x = x_grid;
    grid.y = y_grid;
    grid.X = X;
    grid.Y = Y;
    
    if plot_grid == 1
        figure()
        plot(X,Y,'k.');
        axis equal
        hold on
        hold on
        line([roads.xs';roads.xe'],[roads.ys';roads.ye'],'Color','r')
        axis equal
        line([roads.xs(1) roads.xe(1)],[roads.ys(1) roads.ye(1)],'LineWidth',4,'Color','b')
        xlabel('x-coordinate (meters)')
        ylabel('y-coordinate (meters)')
        title(['2D Regular Grid, dx = ' num2str(dx) ' m' ' dy = ' num2str(dy) ' m'],'FontSize',18)
    end
    





%     figure()
%     plot(sites.x,sites.y,'*','MarkerSize',4)
%     hold on
%     line([roads.xs';roads.xe'],[roads.ys';roads.ye'],'Color','r')
%     xlim(x_boundaries)
%     ylim(y_boundaries)
%     axis equal
