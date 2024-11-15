clc
clear
close all
%%
% Here we give the path of the excel files with the information about the
% settlements and the holloways
filename_sites = 'Khabur_ID_merge_subset.csv';
filename_roads = 'hollow_way_subset.csv';

%% Model Parameters

% For the grid :
global dx dy space_frac 

dx = 100 ; % spacing between the grid points in x direction
dy = 100 ; % spacing between the grid points in y direction

space_frac = 0.05;

% for the decay function
global max_dist max_angle function_id b R1 R2

max_dist = 800; % in meters
max_angle = 30;  % in degrees , for exampple 30 degrees on each side, total angle of the arc will then be 60 degrees
function_id = 1 ; % ID of the function to be used as the decay function. The functions with their parameters can be 
                  % defined in the matlab function 'decay_function.m'.  Now
                  % only one is defined
                  
b = 750;
R1 = 250;
R2 = 250;

%% Run function read_info_sites
% Save the information of the hollow ways and of the settlements
% Create a plot of the site with the holloways and the settlements
[sites_info,sites,roads_info,roads] = read_info_sites_roads(filename_sites,filename_roads,1);


%% Run function generate_grid
% Creates a 2D regular grid with points 
% the boundaries of the area are defined based on the coordinates of the
% holloways (left/right most , highest/lowest) with some additional margin
% added in each side of 5% of the total length of each direction (x,y)

plot_grid =1 ; 
 %run generate_grid_script.m
[grid] = generate_grid(sites,roads,1);


%% Run test_decay_function (for one holloway to see the result of the projection of values by the function
%  run test_decay_function.m

%% Calculate Z values for all holloways. 

Z = calculate_Zvalue(grid,roads,sites);

%% Find the Non Zero Z Values and replace with Nans the zero. 
mask_only_nonzero = Z>0 ;
Z_non_Zero = Z ; 
Z_non_Zero(~mask_only_nonzero) = nan;
%% Plotting 
%% Preparation of the Mesh Grid for the Surf Plots

[X,Y] = meshgrid(grid.y./100000,grid.x./10000);

%% Preparation of the color map for the Surf Plots
% Define a custom colormap
% Define the number of colors
numColors = 256;

% Define the color points for interpolation
colorPoints = [
   247/255 249/255 239/255;    % Green (RGB)
   255/255 165/255 0;    % Yellow (RGB)
    1 0 0;    % Red (RGB)
];

% Define the position of each color point
colorPositions = [0 0.5 1];

% Interpolate to create a smooth gradient colormap
customColormap = interp1(colorPositions, colorPoints, linspace(0, 1, numColors));

%% Color for the Roads 
roadsColor = [89 89 89] / 255 ; 
%% Background Color of the Surf Plots
backgroundColor = [247 249 239]/255 ; 
%% Histogram of all Non Zero Values
figure()
hist(Z_non_Zero(:)./max(Z_non_Zero(:)))
xlabel('Z Value')
ylabel('Bin Count')
title('Histogram of all the non-zero Z values')
%% Simple Surf Plot of the Non Zero Z values with the roads and the known sites 
figure()
surf(X,Y,Z_non_Zero)
shading interp
hold on
line([roads.ys';roads.ye']./100000,[roads.xs';roads.xe']./10000,'Color', roadsColor)
view(90,-90)
plot(sites.y./100000,sites.x./10000,'o','MarkerSize',6,'Color','k')
set(gca,'Color',backgroundColor)
colormap(customColormap) % change colormap scheme here

% Set colormap limits
caxis([min(min(Z_non_Zero))*0 max(max(Z_non_Zero))]) % Set to your desired min and max values

% Add colorbar and title
colorbarHandle = colorbar;
colorbarHandle.Label.String = 'Site Likelihood [au]'; % Set your colormap title
colorbarHandle.Label.FontSize = 14; % Set font size of the colormap title

% Customizing the tick labels for more precision
ax = gca; % Get current axes
ax.XAxis.TickLabelFormat = '%,.4f'; % Format for x-axis labels with 4 decimal places
ax.YAxis.TickLabelFormat = '%,.4f'; % Format for y-axis labels with 4 decimal places

%% Select Thresholding Method

thresholding_method = '2' ; 

% Method 1: Manually Set a threshold for Z value
if strcmp(thresholding_method,'1')
    
    significantMask = Z>1.5;  % Significant values must be non-zero

% Method 2: Statistics, Set P value (Bonferonni Correction included)
elseif strcmp(thresholding_method,'2')
    
    % Step 1: Calculate Z-scores for non-zero values
    nonZeroIndices = find(Z > 0);
    nonZeroValues = Z(nonZeroIndices);
    mu = mean(nonZeroValues);
    sigma = std(nonZeroValues);
    zScores = (Z - mu) / sigma;  % Z-scores for the entire grid, including zeros

    % Apply a threshold with Bonferroni correction
    pValueThreshold = 0.05 / numel(nonZeroValues);

    zThreshold = norminv(1 - pValueThreshold);
    significantMask = zScores > zThreshold & Z > 0;  % Significant values must be non-zero
else
    error('Selected Thresholding Method Does not Exist, please choose one from the available Options')
end

%% Figure with the significant areas shown with circles. 
figure()
mainAxes = axes; % Main axes
hold on;
surf(X, Y, Z_non_Zero)
shading interp
hold on
line([roads.ys'; roads.ye'] ./ 100000, [roads.xs'; roads.xe'] ./ 10000, 'Color',roadsColor) % Change color of hollow ways here
view(90, -90)
set(gca, 'Color', backgroundColor) % Change background color here
colormap(customColormap) % Use the custom colormap defined earlier
caxis([min(Z, [], 'all'), max(Z, [], 'all')]) % Set colormap limits
colorbarHandle = colorbar;
colorbarHandle.Label.String = 'Site Likelihood [au]';
colorbarHandle.Label.FontSize = 14;

% Customizing the tick labels for more precision
ax = gca; % Get current axes
ax.XAxis.TickLabelFormat = '%,.4f'; % Format for x-axis labels with 4 decimal places
ax.YAxis.TickLabelFormat = '%,.4f'; % Format for y-axis labels with 4 decimal places


% Define constants for circle plotting
fixedRadius = 10/100; % Fixed radius for all circles
transparency = 0.5; % Transparency level of the fill color

% Processing significant areas
connectedComponents = bwconncomp(significantMask);
circleCenters = [];

for i = 1:connectedComponents.NumObjects
    idxList = connectedComponents.PixelIdxList{i};
    [rows, cols] = ind2sub(size(Z), idxList);
    
    % Find the center (maximum value point within the region)
    [~, maxIdx] = max(Z(idxList));
    centerRow = rows(maxIdx);
    centerCol = cols(maxIdx);
    
    % Store the center of the circle
     circleCenters = [circleCenters; X(centerRow, centerCol), Y(centerRow, centerCol)];
    
    % Color for the fill based on the Z value at the center
    centerValue = Z(centerRow, centerCol);
    normalizedValue = (centerValue - min(Z, [], 'all')) / (max(Z, [], 'all') - min(Z, [], 'all'));
    fillColor = interp1([0 0.5 1], colorPoints, normalizedValue);

    
    % Plot filled circle with specified radius and color
    theta = linspace(0, 2*pi, 50);
    xCircle = fixedRadius * cos(theta)/10 + X(centerRow, centerCol);
    yCircle = fixedRadius * sin(theta) + Y(centerRow, centerCol);
    fill(xCircle, yCircle, fillColor, 'FaceAlpha', transparency, 'EdgeColor', 'none');
end

% Output information
disp(['Number of circles drawn: ', num2str(numel(connectedComponents.PixelIdxList))]);
disp('Centers of the circles:');
disp(circleCenters);

% Final plot adjustments

% Customize labels
xlabel('', 'FontSize', 14, 'HorizontalAlignment', 'center')
ylabel('', 'FontSize', 14, 'HorizontalAlignment', 'center')
%title('Potential Sites Location Search - Algorithm 1', 'FontSize', 16)

% % Add the words and arrows for North, East, South, West

annotation('textarrow', [0.1, 0.9], [0.025, 0.025], 'String', '', 'Color', 'k', 'HeadStyle', 'plain')
annotation('textarrow', [0.075, 0.075], [0.1, 0.9], 'String', '', 'Color', 'k', 'HeadStyle', 'plain')

% Add directional arrows with labels
text(0.05, -0.075, '', 'Units', 'normalized', 'FontSize', 14, 'HorizontalAlignment', 'right')
text(1.0,-0.075, 'East', 'Units', 'normalized', 'FontSize', 14, 'HorizontalAlignment', 'left')
text(-0.075, -0.03, '', 'Units', 'normalized', 'FontSize', 14, 'HorizontalAlignment', 'center')
text(-0.075, 1, 'North', 'Units', 'normalized', 'FontSize', 14, 'HorizontalAlignment', 'center')

hold on;
plot(sites.y./100000,sites.x./10000,'x','MarkerSize',6,'Color',[0.5 0.5 0.5],'LineWidth', 2);


%% 

% Ask the user if they want to add an inset to the plot
insetChoice = questdlg('Would you like to add a zoomed inset to the plot?', ...
                       'Add Inset', ...
                       'Yes', 'No', 'No');  % Default to 'No'

                   
% Check the user's choice and add an inset if they chose 'Yes'
if strcmp(insetChoice, 'Yes')


    % Ask user for zoom limits
    disp('Click two diagonal corners of the zoom area on the plot.');
    [x, y] = ginput(2);  % User clicks two points

    % Add inset axes at a position of your choice
    insetAxes = axes('Position', [0.6 0.67 0.25 0.25]); % Modify as needed
    box on;

    % Repeat the plot commands for the inset
    copyobj(allchild(mainAxes), insetAxes);
    view(90, -90)
    set(gca, 'Color', backgroundColor) % Change background color here
    colormap(customColormap) % Use the custom colormap defined earlier
    

    % Set the zoom limits
    xlim(insetAxes, [min(x) max(x)]);
    ylim(insetAxes, [min(y) max(y)]);
    set(insetAxes, 'FontSize', 10); % Smaller font size for inset


    % Remove the Text South North West East from the Inset Zoom Plot 
    insetAxes.Children(2).String = ''
    insetAxes.Children(3).String = ''
    insetAxes.Children(4).String = ''
    insetAxes.Children(5).String = ''
    
    % Iterate over each child object in insetAxes
    % and remove the fill color
    for child = insetAxes.Children'
        if strcmp(child.Type, 'patch')
            child.FaceColor = 'none';  % Remove fill color
            % child.EdgeColor = 'k';     % Set edge color to black
            % or retain the original fill color
            % child.EdgeColor = child.FaceColor;
        end
    end

end


%% Ask user if they want to save the circle centers
choice = questdlg('Do you want to save the centers of the highlighted areas to an Excel file?', ...
    'Save Circle Centers', ...
    'Yes', 'No', 'No');  % Default to 'No'

% Handle response
switch choice
    case 'Yes'
        % Let the user choose where to save the file
        [file, path] = uiputfile('*.xlsx', 'Save file as');
        if isequal(file, 0) || isequal(path, 0)
            disp('User canceled file save.');
        else
            % Full file path
            fullFilePath = fullfile(path, file);
            % Save the circle centers to the chosen path
            writematrix(circleCenters, fullFilePath);
            disp(['Circle centers saved to: ', fullFilePath]);
        end
    case 'No'
        disp('User chose not to save the circle centers.');
end



%% 

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

%% Extract coordinates of potential site locations - need to run calculate_Zvalue separately to get Z_new as a variable to work with
% 
% X_newsites = X(Z_new > 1.5) ; %extract X coordinates of cells with a value > 1.5
% Y_newsites = Y(Z_new > 1.5) ; %extract Y coordinates of cells with a value > 1.5
% 
% 
% new_sites = [X_newsites Y_newsites]; %create a new array of the X and Y coordinates
% 
% new_sites_t = array2table(new_sites); %convert the array to a table
% new_sites_t.Properties.VariableNames = ["X","Y"]; %rename the columns
% 
% writetable(new_sites_t,'potential_sites_attempt1_allhw.csv'); % write table to .csv


%%
% Define the Gaussian filter
% Assuming Zplot, X, and Y are already defined

% % Step 1: Calculate Z-scores for non-zero values
% nonZeroIndices = find(Z > 0);
% nonZeroValues = Z(nonZeroIndices);
% mu = mean(nonZeroValues);
% sigma = std(nonZeroValues);
% zScores = (Z - mu) / sigma;  % Z-scores for the entire grid, including zeros
% 
% % % Apply a threshold with Bonferroni correction
% % pValueThreshold = 0.05 / numel(nonZeroValues);
% pValueThreshold = 0.05 ;
% 
% zThreshold = norminv(1 - pValueThreshold);
% significantMask = zScores > zThreshold & Z > 0;  % Significant values must be non-zero
% 
% significantMask = Z>1.5;  % Significant values must be non-zero

% 
% %
% % Initial threshold with all non-zero entries
% initialThreshold = norminv(1 - (0.05 / numel(nonZeroValues)));
% 
% % Initial significant detection
% initialSignificantPoints = zScores > initialThreshold;
% 
% % Count of initial significant points
% initialCount = sum(sum(initialSignificantPoints));
% 
% % Adjusted threshold based on initial significant count
% adjustedThreshold = norminv(1 - (0.05/ initialCount));
% 
% % Final significant detection based on adjusted threshold
% finalSignificantPoints = zScores > adjustedThreshold;
% %
% Step 2: Identify connected components of significant values
% connectedComponents = bwconncomp(significantMask);
% connectedComponents = bwconncomp(finalSignificantPoints);
% 
% % Constants for checks
% maxAllowedRadius = 100;  % Define the maximum allowed radius
% minPointsInCircle = 1;  % Define the minimum number of points to consider a region
% 
% % Initialize counters and storage
% numberOfCircles = 0;
% circleCenters = [];  % To store the coordinates of the circle centers
% 
% % Plot circles around significant areas
% for i = 1:connectedComponents.NumObjects
%     idxList = connectedComponents.PixelIdxList{i};
%     [rows, cols] = ind2sub(size(Z), idxList);
%     
%     % Check if the region has at least the minimum number of points
%     if numel(idxList) < minPointsInCircle
%         continue;  % Skip this region
%     end
% 
%     % Find the maximum value within this region and use it as the center
%     [~, maxIdx] = max(Z(idxList));
%     centerRow = rows(maxIdx);
%     centerCol = cols(maxIdx);
%     
%     % Calculate distances from the center to all points in the region
%     distances = sqrt((X(rows, cols) - X(centerRow, centerCol)).^2 + ...
%                      (Y(rows, cols) - Y(centerRow, centerCol)).^2);
%     radius = max(max(distances));
% 
%     % Check if the calculated radius exceeds the maximum allowed radius
%     if radius > maxAllowedRadius
%         radius = maxAllowedRadius;  % Limit the radius
%     end
%     
%     % Store the center of the circle
%     circleCenters = [circleCenters; X(centerRow, centerCol), Y(centerRow, centerCol)];
%     
%     % Plot the circle
%     theta = linspace(0, 2*pi, 100);
%     circX = radius * cos(theta) + X(centerRow, centerCol);
%     circY = radius * sin(theta) + Y(centerRow, centerCol);
%     plot(circX, circY, 'b-', 'LineWidth', 2);  % Blue circle
%     
%     % Increment the counter for valid circles
%     numberOfCircles = numberOfCircles + 1;
% end
% 
% % Output the number of circles and their centers
% disp(['Number of circles drawn: ', num2str(numberOfCircles)]);
% disp('Centers of the circles:');
% disp(circleCenters);
