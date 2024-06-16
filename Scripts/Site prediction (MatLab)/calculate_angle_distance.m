function [angle_NP,dist_NP] = calculate_angle_distance(P1,P2,neighbors,grid,roads,show_plots)

global max_angle 


% Working with P1
A = P1-P2 ; 
A = repmat(A,[size(neighbors.coord,1) 1]);
a = vecnorm(A,2,2);

EP = neighbors.coord(:,:); % EP stands for estimation point, the grid point that we want to estimate the distance and angle. 
B = EP - P1; 
b =  vecnorm(B,2,2);

theta = rad2deg(acos(dot(A,B,2)./(a.*b)));

angle_NP = theta ; 
dist_NP  = b ; 


if show_plots==1
figure()
plot(grid.X,grid.Y,'k.');
axis equal
hold on
line([roads.xs(1) roads.xe(1)],[roads.ys(1) roads.ye(1)],'Color','g','LineWidth',3)
plot(P1(1),P1(2),'g*','LineWidth',3) % start of hw
plot(P2(1),P2(2),'r*','LineWidth',3) % end of hw
% and now plot the closest grid points C2s C2e
plot(neighbors.coord(:,1),neighbors.coord(:,2),'bo');
% plot(cLP1.coord(1),cLP1.coord(2),'go');
% plot(cLP2.coord(1),cLP2.coord(2),'ro');
xlim([-5*grid.dx+min(neighbors.coord(:,1)) 5*grid.dx+max(neighbors.coord(:,1))])
ylim([-5*grid.dy+min(neighbors.coord(:,2)) 5*grid.dy+max(neighbors.coord(:,2))])
title('Maximum angle = 40')


  for i=1:size(neighbors.coord,1)

        hold on
        
        plot(EP(i,1),EP(i,2),'mo','LineWidth',2)

        if theta(i)<max_angle
        line([P1(1) EP(i,1)],[P1(2) EP(i,2)])
        end
        dim = [0.45 0.5 0.3 0.3];
        % 
        str = ['Angle = ' num2str(theta(i)) ' ^o '];
        ann = annotation('textbox',dim,'String',str,'FitBoxToText','on');
        % 
        pause()
        delete(ann)
  end
end



end

