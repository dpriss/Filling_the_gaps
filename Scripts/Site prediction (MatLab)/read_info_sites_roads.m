function [sites_info,sites,roads_info,roads] = read_info_sites_roads(filename_sites,filename_roads,plots)

sites_info = readtable(filename_sites);

roads_info = readtable(filename_roads);

sites.x = sites_info.POINT_X ;
sites.y = sites_info.POINT_Y ;
sites.n = size(sites_info,1);

roads.xs = roads_info.startX;
roads.xm = roads_info.midX ; 
roads.xe = roads_info.endX ; 

roads.ys = roads_info.startY;
roads.ym = roads_info.midY ; 
roads.ye = roads_info.endY ; 

roads.n = size(roads_info,1);

if plots==1
%     figure()
%     plot(x_s,y_s,'*','MarkerSize',4)
%     axis equal
% 
%     figure()
%     plot(xm_r,ym_r,'b.','MarkerSize',2)
%     hold on
%     line([xs_r';xe_r'],[ys_r';ye_r'],'Color','r')
%     axis equal

    figure()
    set(gcf, 'Position', get(0, 'Screensize'));
    plot(sites.x,sites.y,'*','MarkerSize',4, 'Color', 'r')
    hold on
    line([roads.xs';roads.xe'],[roads.ys';roads.ye'],'Color','b')

    axis equal
    xlabel('x-coordinate (meters)')
    ylabel('y-coordinate (meters)')
    title('Study area in cartesian coordinates')
    text(797000,4090000,'{\color{blue} --} Hollow ways','FontSize',18,'Color','k','EdgeColor','k')
    text(797000,4094500,'{\color{red} *} Known Settlements','FontSize',18,'Color','k','EdgeColor','k')

end

