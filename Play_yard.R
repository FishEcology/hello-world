#' Getting started with IBMSs in R
#' 
#' https://bradduthie.github.io/blog/individual-based-models-in-r/
#' 
#' ## First we define the individuals

# Individuals

inds = array(data = 0, dim = c(5, 3));
colnames(inds) = c("characteristic_1", "characteristic_2", "characteristic_3");
rownames(inds) = c("ind_1", "ind_2", "ind_3", "ind_4", "ind_5");
print(inds);

# Change to interestng characters

colnames(inds) = c("body_mass", "x_loc", "y_loc");

#' Let us say that body mass is normally distributed around a value of 23 with a standard deviation of 3.

inds[, 1] = rnorm(n = dim(inds)[1], mean = 23, sd = 3);

#' We can now set individual x and y locations into columns 2 and 3, respectively, by sampling from a vector of numbers 1 to 8 with replacement (sampling with replacement ensures that more than one individual can occupy the same x or y location).

inds[, 2] = sample(x = 1:8, size = dim(inds)[1], replace = TRUE);
inds[, 3] = sample(x = 1:8, size = dim(inds)[1], replace = TRUE);

#' We can even plot the individuals’ x and y locations to see how they are spatially distributed.

plot(x = inds[,2], y = inds[,3], pch = 20, cex = 4, xlim = c(1, 8),
     ylim = c(1, 8), xlab = "x location", mar = c(5, 5, 1, 1),
     ylab = "y location", cex.lab = 1.5, cex.axis = 1.5);

#' ### ...then we move them
#' 
#' To model random movement to any of the eight cells surrounding a focal individual’s current location (thereby also allowing diagonal movement), we can sample two random integer values from the set {−1,0,1} with replacement and equal probability. 

x_move     <- sample(x = c(-1, 0, 1), size = 1);
y_move     <- sample(x = c(-1, 0, 1), size = 1);
inds[1, 2] <- inds[1, 2] + x_move;
inds[1, 3] <- inds[1, 3] + y_move;

#' This was only one individual, but we can move all of the individuals simultaneously according to our movement rule if we sample a vector of x_move and y_move that matches the number of individuals in the array.

x_move    <- sample(x = c(-1, 0, 1), size = dim(inds)[1], replace = TRUE);
y_move    <- sample(x = c(-1, 0, 1), size = dim(inds)[1], replace = TRUE);
inds[, 2] <- inds[, 2] + x_move;
inds[, 3] <- inds[, 3] + y_move;

#' ### Now do it as a function...

#' To be especially vigilant, it is often a good idea to give each unique process in an IBM its own function. There are at least three benefits of coding this way. First, it will later allow us to test each process in the IBM independently, so that if one part of the model does not appear to be working as it should, then we can narrow down the problem more easily by checking each function. Second, it makes it easier to change the order of operations in our IBM (e.g., whether movement happens before, or after, birth or death processes). Third, it makes the code easier to read; rather than having to scan through all of the code at once to understand a model, we can break things down piece by piece (this will become clearer later).

movement <- function(inds, xloc = 2, yloc = 3){
  total_inds   <- dim(inds)[1]; # Get the number of individuals in inds
  move_dists   <- c(-1, 0, 1);  # Define the possible distances to move
  x_move       <- sample(x = move_dists, size = total_inds, replace = TRUE);
  y_move       <- sample(x = move_dists, size = total_inds, replace = TRUE);
  inds[, xloc] <- inds[, xloc] + x_move;
  inds[, yloc] <- inds[, yloc] + y_move;
  return(inds);
}

inds=movement(inds);

#' ## Now, let's simulate movement over time

#' We're essentially calling the `movement` function multiple times, total times steps is called `time_steps`

time_steps <- 20;

#' To run a simulation of individuals moving for twenty time steps, we now need to use a loop. A while loop in which some variable ts (indicating ‘time step’) increases from 0 to 19 is probably the easiest way to code the simulation.
#'

ts <- 0; #start time step
while(ts < time_steps){
  inds <- movement(inds);
  ts   <- ts + 1; 
}

#' But how do we stop indiviudals from *falling off the map*?
#' Common options include : 
#' 
#'     1. Place them back onto the boundary edge (i.e., a sticky landscape edge).  
#'     2. Change their direction at the boundary edge (i.e., a reflecting landsacpe edge).  
#'     3. Have them move to the opposite side of the landscape (i.e., a torus landscape with no edge)  

## Reflective edge

movement <- function(inds, xloc = 2, yloc = 3, xmax = 8, ymax = 8){
  total_inds   <- dim(inds)[1]; # Get the number of individuals in inds
  move_dists   <- c(-1, 0, 1);  # Define the possible distances to move
  x_move       <- sample(x = move_dists, size = total_inds, replace = TRUE);
  y_move       <- sample(x = move_dists, size = total_inds, replace = TRUE);
  inds[, xloc] <- inds[, xloc] + x_move;
  inds[, yloc] <- inds[, yloc] + y_move;
  
  # =========   The reflecting boundary is added below
  for(i in 1:total_inds){               # For each individual i in the array
    if(inds[i, xloc] > xmax){         # If it moved passed the maximum xloc
      inds[i, xloc] <- xmax - 1;    # Then move it back toward the centre
    }
    if(inds[i, xloc] < 1){            # If it moved below 1 on xloc
      inds[i, xloc] <- 2;           # Move it toward the centre (2)
    }
    if(inds[i, yloc] > ymax){         # If it moved passed the maximum yloc
      inds[i, yloc] <- ymax - 1;    # Then move it back toward the centre
    }
    if(inds[i, yloc] < 1){            # If it moved below 1 on yloc
      inds[i, yloc] <- 2;           # Then move it toward the centre (2)
    }
  } 
  # =========  Now all individuals should stay on the landscape
  return(inds);
}

#' let's generate some new indidivuals

inds           <- array(data = 0, dim = c(5, 3));
colnames(inds) <- c("body_mass", "x_loc", "y_loc");
rownames(inds) <- c("ind_1", "ind_2", "ind_3", "ind_4", "ind_5");
inds[,1]       <- rnorm(n = dim(inds)[1], mean = 23, sd = 3);
inds[,2]       <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE);
inds[,3]       <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE);

#' and run the simulation again
#' 
ts         <- 0;
time_steps <- 20;
while(ts < time_steps){
  inds <- movement(inds);
  ts   <- ts + 1; 
}
print(inds);

#' how about if we want to reconstruct the movement patterns of each individual and see how the whole population moves from time step 0 to time step 20. In R, we can do this easily by creating a new list and storing the `inds` array as a list element in each time step.


ts         <- 0;
time_steps <- 20;
inds_hist  <- NULL; # Here's the list
while(ts < time_steps){
  inds            <- movement(inds);
  ts              <- ts + 1; 
  inds_hist[[ts]] <- inds; # Add to list
}
print(inds);

#' The `inds_hist` list essentially stores the entire history of the individuals moving over the course of the simulation. Storing this kind of information is very useful for reconstructing the history of a simulation to understand what is going on. As more biological processes are added (e.g., birth, death, predation, etc.), we can effectively take a perfect snapshot of each point in time in the system that we are modelling.
#' 
#' But: this can get huge with lots of information.... so we then subset the data with what we're interested in...
#' 
#' We could, for example, see where individual 1 has been over the 20 time steps by extracting the information from inds_hist and storing it in a new table.
