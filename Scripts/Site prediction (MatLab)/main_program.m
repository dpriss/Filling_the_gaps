clc
clear
close all

% Here we give the path of the excel files with the information about the
% settlements and the holloways
filename_sites = 'Khabur_ID_merge.csv';
filename_roads = 'hw_all.csv';

%% Model Parameters

% For the grid :
global dx dy space_frac 

dx = 100 ; % spacing between the grid points in x direction
dy = 100 ; % spacing between the grid points in y direction

space_frac = 0.05;

% for the decay function
global max_dist max_angle function_id

max_dist = 800; % in meters
max_angle = 30;  % in degrees , for exampple 30 degrees on each side, total angle of the arc will then be 60 degrees
function_id = 1 ; % ID of the function to be used as the decay function. The functions with their parameters can be 
                  % defined in the matlab function 'decay_function.m'.  Now
                  % only one is defined

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
 run(test_decay_function)
%% Calculate Z values for all holloways. 

calculate_Zvalue(grid,roads,sites);

%% Extract coordinates of potential site locations - need to run calculate_Zvalue separately to get Z_new as a variable to work with

X_newsites = X(Z_new > 1.5) ; %extract X coordinates of cells with a value > 1.5
Y_newsites = Y(Z_new > 1.5) ; %extract Y coordinates of cells with a value > 1.5

new_sites = [X_newsites Y_newsites]; %create a new array of the X and Y coordinates

new_sites_t = array2table(new_sites); %convert the array to a table
new_sites_t.Properties.VariableNames = ["X","Y"]; %rename the columns

writetable(new_sites_t,'potential_sites_attempt1_allhw.csv'); % write table to .csv