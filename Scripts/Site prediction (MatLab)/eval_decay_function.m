function values = eval_decay_function(angle,distance)


global max_angle max_dist 


   [t] = decay_function; 

   values = t(distance,angle) ; 
   values(~(angle<max_angle & distance<max_dist)) = 0 ; 
   
   
end

