function [NP] = near_points(CP_indcs,grid)
    
    global max_dist 

    dx = grid.dx ; 
    dy = grid.dy ; 
    
    no_points_x = ceil((max_dist+0.1*max_dist)/dx); 
    

    no_points_y = ceil((max_dist+0.1*max_dist)/dy) ; 
    
    if no_points_x>=no_points_y
        no_points = no_points_x;
    else
        no_points = no_points_y;
    end
    
    x_indcs_near = CP_indcs(1)-no_points:CP_indcs(1)+no_points;
    y_indcs_near = CP_indcs(2)-no_points:CP_indcs(2)+no_points;
    x_indcs_near = x_indcs_near((x_indcs_near>0 )&(x_indcs_near<size(grid.x,2))) ; 
    y_indcs_near = y_indcs_near((y_indcs_near>0 )&(y_indcs_near<size(grid.y,2))) ; 

    
    [m,n] = meshgrid(x_indcs_near,y_indcs_near);
    [NP.indcs(:,1),NP.indcs(:,2)] = deal(reshape(m,[],1),reshape(n,[],1));
   
    % find the coordinates of these points 
     NP.coord = [grid.x(NP.indcs(:,1))',grid.y(NP.indcs(:,2))'];
    
end

