# Authors: Klara Strömqvist, Maya Ketuly, Hampus Sandqvist

#' #' getManhattanDistance
#' #' 
#' #' Function to calculate Manhattan distance.
#' #' @param pos1 Vector with x and y values.
#' #' @param pos2 Vector with x and y values.
#' #' @return The Manhattan distance.
#' getManhattanDistance = function(pos1, pos2){
#'   #distanceX = abs(pos1[1] - pos2[1])
#'   #distanceY = abs(pos1[2] - pos2[2])
#'   manhattanDistance = abs(pos1[1] - pos2[1]) + abs(pos1[2] - pos2[2])#distanceX + distanceY
#'   return (manhattanDistance)
#' }
#' getManhattanDistance
#' 
#' Function to calculate Manhattan distance.
#' @param pos1 Vector with x and y values.
#' @param pos2 Vector with x and y values.
#' @return The Manhattan distance.
getManhattanDistance = function(pos1, pos2){
  
  # Check if both pos1 and pos2 are numeric
  if (!is.numeric(pos1) || !is.numeric(pos2)) {
    stop("Both pos1 and pos2 must be numeric vectors.")
  }
  
  # Check if both pos1 and pos2 have at least 2 elements (x and y)
  if (length(pos1) < 2 || length(pos2) < 2) {
    stop("Both pos1 and pos2 must have at least two elements (x and y).")
  }
  
  # Calculate Manhattan distance
  manhattanDistance = abs(pos1[1] - pos2[1]) + abs(pos1[2] - pos2[2])
  
  return (manhattanDistance)
}


#' findPackage
#' 
#' Finds the package that is closest to the car's current location.
#' @param carLoc Vector with x and y values.
#' @param packages Matrix with package locations.
#' @return Vector with coordinates for the closest package
findPackage = function(carLoc, packages){
  # Vector of indices of the unpicked packages
  noUnpicked = which(packages[,5] == 0)
  packagesUnpicked = packages[noUnpicked,]
  
  # Don't need to calculate closest package if only on is left
  if(length(noUnpicked) == 1) {
    return (packagesUnpicked)
  }
  
  closestPackage = NULL
  distanceToClosestPackage = NULL
  
  # Iterate over unpicked packages
  for (i in 1:length(noUnpicked)) {
    package = packagesUnpicked[i,]
    pickupCoords = c(package[1], package[2])
    
    manDistance = getManhattanDistance(carLoc, pickupCoords)
    
    if (manDistance < distanceToClosestPackage || is.null(distanceToClosestPackage)) {
      closestPackage = package
      distanceToClosestPackage = manDistance
    }
  }
  return (closestPackage)
}

#' inSet
#' 
#' Checks if a node is in a given set by comparing the x and y coordinates.
#' @param node A vector with x and y values (coordinates) for the node you want to check.
#' @param set A node is represented by a list containing x and y coordinates.
#' @return The index of nodes, where ex of the node if found, or FALSE if the node is not in the set.
inSet = function(node, set) {
  if (length(set) == 0) {
    return (FALSE)
  }
  # Loop through each node in the set
  for (i in 1:length(set)) {
    # Check if both x and y coordinates match
    if (set[[i]]$x == node[1] && set[[i]]$y == node[2]) {
      return(i)  # Return the index of the node if found
    }
  }
  return (FALSE)  # Return FALSE if the node is not in the set
}

#' tracePath
#'
#' Traces the path from the given node back to the start node by following parent nodes in the A* algorithm.
#' @param node A list representing the final node (or the current node) with its x, y coordinates and its parent node.
#' @param set A list of nodes that have been explored by the A* algorithm.
#' @return The node that we want to move to.
tracePath = function(node, set) {
  # parentNode stores the parent of the current node (the node we are tracing from)
  parentNode = node$parentNode
  # Current node being traced
  child = node
  
  # Loop continues until the parentNode is the start node (represented by (0,0))
  while (all(parentNode != c(0,0))) {
    # Loop through all nodes in the set to find the parent of the current node
    for (node in 1:length(set)) {
      # Get the x and y coordinates of the current node in the set
      pos = c(set[[node]]$x, set[[node]]$y)
      # If the position matches the parentNode, we have found the parent node
      if (all(parentNode == pos)) {
        # If the found node's parent is not (0,0), update child to the found node
        if (all(set[[node]]$parent != c(0,0))) {
          child = set[[node]]
        }
        # Update parentNode to continue tracing back along the path
        parentNode = set[[node]]$parent
        break
      }
    }
  }
  
  # Return the second-to-last node, which is the next move in the path
  return (child)
}

#' isOutOfBounds
#'
#' Checks if a node is out of bounds to the board or not.
#' @param nodePosition Position of node which is a vector with x and y coordinate.
#' @return True if the node is inside the board, otherwise False
isOutOfBounds = function(nodePosition) {
  if (nodePosition[1] < 11 & nodePosition[1] > 0 & nodePosition[2] < 11 & nodePosition[2] > 0) {
    return (TRUE)
  }
  return (FALSE)
}

#' aStar
#' 
#' Performs A* algorithm
#' @param hroads A matrix containing traffic conditions on horizontal roads
#' @param vroads A matrix containing traffic conditions on vertical roads
#' @param packageLoc A vector with x and y values (coordinates) for the package location
#' @param carLocation A vector with x and y values (coordinates) for the car location
#' @return next move, array with x and y value
aStar = function(hroads, vroads, packageLoc, carLoc){
  frontier <- list()
  nodes <- list()
  # Our chosen heuristic: Manhattan Distance
  h = getManhattanDistance(carLoc, packageLoc)
  # Our starting node
  start <- list(x=carLoc[1], y=carLoc[2], g=0, h=h, f=h, parentNode=c(0,0))
  # Goal node
  goal = NULL
  # Add starting node to end of the frontier
  frontier[[length(frontier) + 1]] <- start
  
  while(1){
    # frontierScores now contains all f values for the frontier nodes
    frontierScores = sapply(frontier, function(element) element$f)
    
    bestScore = which.min(frontierScores)
    currNode = frontier[[bestScore]]
    
    frontier = frontier[-bestScore]
    nodes[[length(nodes) + 1]] <- currNode
    
    # Calculate h for current node
    currNodePos = c(currNode$x, currNode$y)
    h = getManhattanDistance(currNodePos, packageLoc)
    
    # We have found the goal if h = 0
    if (h == 0) {
      goal = currNode
      goalScore = goal$f
      # trace back to the start node
      nextMove = tracePath(goal, nodes)
      nodeInfo <- list(node=nextMove, goalScore=goalScore)
      return (nodeInfo)
    }
    
    # Expandera neighbors
    xCoord = currNode$x
    yCoord = currNode$y
    
    right = c(xCoord + 1, yCoord)
    left = c(xCoord - 1, yCoord)
    up = c(xCoord, yCoord + 1)
    down = c(xCoord, yCoord - 1)
    
    expandedNodes <- list(right, left, up, down)
    
    for (node in 1:length(expandedNodes)) {
      
      if (!inSet(expandedNodes[[node]], nodes) & isOutOfBounds(expandedNodes[[node]])) {
        xCoord = expandedNodes[[node]][1]
        yCoord = expandedNodes[[node]][2]
        
        nodeCoords <- c(xCoord, yCoord)
        
        h = getManhattanDistance(nodeCoords, carLoc)
        
        # expand to the right
        if (node == 1) {
          g = hroads[xCoord-1, yCoord]
        }
        # expand to the left
        else if (node == 2) {
          g = hroads[xCoord, yCoord]
        }
        # expand upwards
        else if (node == 3) {
          g = vroads[xCoord, yCoord-1]
        }
        # expand downwards
        else if (node == 4) {
          g = vroads[xCoord, yCoord]
        }
        
        # Update parent for path tracing
        parentNode = c(currNode$x, currNode$y)
        # Update g cost
        g = g + currNode$g
        # Is expanded node already present in the frontier?
        expandedNodeIndex = inSet(expandedNodes[[node]], frontier)
        
        # Create a new node to continue with A*
        newNode <- list(x=xCoord, y=yCoord, g=g, h=h, f=g+h, parentNode=parentNode)
        
        # If new node is not already in frontier
        if (expandedNodeIndex == FALSE) {
          # Add to the frontier
          frontier[[length(frontier) + 1]] <- newNode
        }
        
        else if (frontier[[expandedNodeIndex]]$f > newNode$f) {
          # Updates frontier if we find a lower f
          frontier[[expandedNodeIndex]] <- newNode
        }
        
      }
      
    }
  }
  
}

# Klaras myFunction
myFunction = function(roads, car, packages) {
  # # 0 if no package is being carried:
  # if (car$load == 0) {
  #   # Find the closest package to pick up:
  #   package = findPackage(c(car$x, car$y), packages)
  #   goalCoords = c(package[1], package[2])
  #   
  # }
  # # If load is not 0, then the car has a package that needs to be delivered:  
  # if (car$load != 0) {
  #   # Set the goal as the delivery point for the current package
  #   package = packages[car$load, ]
  #   goalCoords = c(package[3], package[4])
  # }
  # 
  # # Use A* to find the best move
  # nextMove = aStar(roads$hroads, roads$vroads, goalCoords, c(car[1], car[2]))
  # 
  # # Set the car's next move
  # 
  # car$nextMove = nextMove
  # return(car)
  
  carLoc = c(car$x, car$y)
  if (car$load == 0) {
    
    package = findPackage(carLoc, packages)
    packageLoc = c(package[1], package[2])
    
    nodeInfo = aStar(roads$hroads, roads$vroads, packageLoc, carLoc)
    move = nodeInfo$node
  }
  else {
    load = car$load
    packageLoc = c(packages[load,3], packages[load,4])
    nodeInfo = aStar(roads$hroads, roads$vroads, packageLoc, carLoc)
    move = nodeInfo$node
  }
  
  if (car$x < move[1]) {nextMove=6}
  else if (car$x > move[1]) {nextMove=4}
  else if (car$y < move[2]) {nextMove=8}
  else if (car$y > move[2]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  return (car)
}

#' dumbDM
#'
#' This control function just moves randomly, until all packages are picked up and delivered by accident!
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
dumbDM=function(roads,car,packages){
  car$nextMove=sample(c(2,4,6,8),1)
  return (car)
}
#' basicDM
#'
#' This control function will pick up and deliver the packages in the order they
#' are given (FIFO). The packages are then delivered ignoring the trafic conditions
#' by first moving horizontally and then vertically.
#' 
#' As a first step, you should make sure you do better than this.
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}
#' manualDM
#'
#' If you have the urge to play the game manually (giving moves 2, 4, 5, 6, or 8 using the keyboard) you
#' can pass this control function to runDeliveryMan
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
manualDM=function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

#' testDM
#'
#' Use this to debug under multiple circumstances and to see how your function compares with the par function
#' The mean for the par function (with n=500) on this is 172.734, and the sd is approximately 39.065.
#'
#' Your final result will be based on how your function performs on a similar run of 500 games, though with
#' a different seed used to select them.
#'
#' This set of seeds is chosen so as to include a tricky game that has pick ups and deliveries on the same
#' spot. This will occur in the actual games you are evaluated on too.
#'
#' While this is dependent on the machine used, we expect your function to be able to run the 500 evaluation games on
#' the evaluation machine in under 4 minutes (250 seconds). If the evaluation machine is slower than expected,
#' this will be altered so that the required time is 25% slower than the par function.
#'
#' The par function takes approximately 96 seconds on my laptop (with n=500 and verbose=0).
#'
#' @param myFunction The function you have created to control the Delivery Man game.
#' @param verbose Set to 0 for no output, 1 for a summary of the results of the games played (mean,
#' standard deviation and time taken), and 2 for the above plus written output detailing seeds used and the
#' runDeliveryMan output of the result of each game.
#' @param returnVec Set to TRUE if you want the results of the games played returned as a vector.
#' @param n The number of games played. You will be evaluated on a set of 500 games, which is also the default here.
#' @param timeLimit The time limit. If this is breached, a NA is returned.
#' @return If returnVec is false, a scalar giving the mean of the results of the games played. If returnVec is TRUE
#' a vector giving the result of each game played. If the time limit is breached, a NA is returned.
#' @export
testDM=function(myFunction,verbose=1,returnVec=FALSE,n=500,seed=21,timeLimit=250){
  if (!is.na(seed))
    set.seed(seed)
  seeds=sample(1:25000,n)
  startTime=Sys.time()
  aStar=sapply(seeds,function(s){
    midTime=Sys.time()
    if (as.numeric(midTime)-as.numeric(startTime)>timeLimit) {
      cat("\nRun terminated due to slowness.")
      return (NA)
    }
    set.seed(s)
    if (verbose==2)
      cat("\nNew game, seed",s)
    runDeliveryMan(myFunction,doPlot=F,pause=0,verbose=verbose==2)
  })
  endTime=Sys.time()
  if (verbose>=1){
    cat("\nMean:",mean(aStar))
    cat("\nStd Dev:",sd(aStar))
    cat("\nTime taken:",as.numeric(endTime)-as.numeric(startTime),"seconds.")
  }
  if (returnVec)
    return(aStar)
  else
    return (mean(aStar))
}

#' Run Delivery Man
#'
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic
#' conditional on the vertical roads. <1,1> is the bottom left, and <dim,dim> is the top right.
#'(2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not
#' delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10. Note that
#' this means you will have to remove duplicated nodes from your frontier to keep your AStar
#' computationally reasonable! There is a time limit for how long an average game can be run in, and
#' if your program takes too long, you will penalized or even fail.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param doPlot Specifies if you want the game state to be plotted each turn.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=myFunction,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5,verbose=T) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list())
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    roads=updateRoads(roads$hroads,roads$vroads)
    if (doPlot) {
      makeDotGrid(dim,i)
      plotRoads(roads$hroads,roads$vroads)
      points(car$x,car$y,pch=16,col="blue",cex=3)
      plotPackages(packages)
    }
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          if (verbose)
            cat("\nCongratulations! You suceeded in",i,"turns!")
          return (i)
        }
      }
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    if (pause>0) Sys.sleep(pause)
  }
  cat("\nYou failed to complete the task. Try again.")
  return (NA)
}
#' @keywords internal
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  }
  return (0)
}
#' @keywords internal
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$x,car$y]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$x,car$y]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$x,car$y]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$x,car$y]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")
  }
  car$nextMove=NA
  return (car)
}

#' @keywords internal
plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0)
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

#' @keywords internal
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @keywords internal
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n-1)
  vroads=matrix(rep(1,(n-1)*n),nrow=n)
  list(hroads=hroads,vroads=vroads)
}

#' @keywords internal
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(row,row+1),c(col,col),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(row,row),c(col,col+1),col=vroads[row,col])
    }
  }
}
#' @keywords internal
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }
  }
  list (hroads=hroads,vroads=vroads)
}


