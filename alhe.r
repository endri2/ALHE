#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must 
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs 
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################

#creates new set of IWDs spread out randomly over graph
initializeIWD <- function(model) {
    IWDs <- list()
    #initialize IWDs
    for (i in 1:length(model$nodes)) {
        #set initVel, set soil to 0, set IWD's starting points ("spread out nodes")
        IWDs[[length(IWDs) + 1]] <- list(
            id = i, 
            v = initVel, 
            soil = 0, 
            nodes = list(sample(nodes, 1)[[1]]),
            edges = list())
    }
    
    return (IWDs)
}

#prepare model for inner iteration
initInnerLoopModel <- function(model, history) {
    #set active IWDs running
    model$activeIWDs <- length(history)
    return (model)
}

solutionQuality <- function(solution, model) {
    quality <- 0
    nodeNum <- length(solution$nodes)
    for(j in 1:nodeNum) {
        #calculate next index in node list
        #remember that we need to add edge which goes
        #from last to first node!
        nextNodeIdx <- j + 1 #((j + 1) %% nodeNum)
        if(nextNodeIdx > nodeNum) {
            nextNodeIdx <- 1
        }
        
        cat(c(solution$nodes[[j]], ",", solution$nodes[[nextNodeIdx]], " | "))
        
        edgeIndex <- getEdgeIndexFromNodes(solution$nodes[[j]], solution$nodes[[nextNodeIdx]] , model$edges)
        
        cat(c(model$edges[[edgeIndex]]$begin, " -> ", model$edges[[edgeIndex]]$end,
              " len = ", model$edges[[edgeIndex]]$length, "\n"))
        quality <- quality + model$edges[[edgeIndex]]$length
    }
    return (quality)
}

#finds iteration-best solution in history
#and returns it with it's quality
getIterationBestSolution <- function(history, model) {
    bestSolutionQuality <- Inf
    bestSolution <- NULL
    for(i in 1:length(history)) {
        #do not take partial solutions
        #(number of visited nodes is not equal
        #to number of nodes in graph)
        if(length(history[[i]]$nodes) != length(model$nodes)) {
            next;
        }
        
        #calculate solution quality, which is sum of edge's length
        #across solution path
        quality <- solutionQuality(history[[i]], model)
        
        cat("Solution: [")
        for(k in 1:length(history[[i]]$nodes)) {
            cat(c(history[[i]]$nodes[[k]], ", "))
        }
        cat(c("\b\b\b], quality = ", quality,"\n"))

        if(quality < bestSolutionQuality) {
            bestSolutionQuality <- quality
            bestSolution <- history[[i]]
        }
    }
    bestSolution$quality <- bestSolutionQuality
    return (bestSolution)
}

#termination condition for inner loop
#all IWDs must have completed their solution
#and this is indicated by 0 activeIWDs
innerTermination <- function(history, model) {
    if(model$activeIWDs == 0) {
        return (TRUE)
    }
    
    return (FALSE)
}

updatePaths <- function(solution, model) {
    nodeNum <- length(solution$nodes)
    
    for(i in 1:nodeNum) {
        N_IB <- nodeNum 
        nextNode <- i + 1
        if(nextNode > nodeNum) {
            nextNode <- 1
        }
        
        edgeIndex <- getEdgeIndexFromNodes(solution$nodes[[i]], solution$nodes[[nextNode]], model$edges)
        
        model$edges[[edgeIndex]]$soil <- (1 + p_IWD) * model$edges[[edgeIndex]]$soil
        - (p_IWD) * ((1)/(N_IB - 1)) * solution$soil
        
    }
    
    return (model)
}

#finds edge index in edgeList, returns 0 if not found
getEdgeIndex <- function(edge, edgeList) {
    for(i in 1:length(edgeList)) { 
        listEdge <- edgeList[[i]]
        if(listEdge$begin == edge$begin && listEdge$end == edge$end) {
            return (i)
        }
    }
    
    return (0)
}

getEdgeIndexFromNodes <- function(nodeBegin, nodeEnd, edgeList) {
    for(i in 1:length(edgeList)) { 
        listEdge <- edgeList[[i]]
        if(listEdge$begin == nodeBegin && listEdge$end == nodeEnd) {
            return (i)
        } else if(listEdge$begin == nodeEnd && listEdge$end == nodeBegin) {
            return (i)
        }
    }
}

#creates IWDs and finds iteration-best solution
#returns updated edges by IWDs and iteration-best solution
#model contains nodes and edges of graph
innerLoop <- function(model) {
    history <- initializeIWD(model)
    model <- initInnerLoopModel(model, history)
    
    while(!innerTermination(history, model)) {
        aa<-aggregatedOperator(history, model)
        history<-historyPush(history,aa$newPoints)
        model<-aa$newModel
    }

    iterationBestSolution <- getIterationBestSolution(history, model)
    #point 7 of algorithm: update paths on iteration best solution
    model <- updatePaths(iterationBestSolution, model)
    
    #returns iteration-best solution
    return(list(solution = iterationBestSolution, model = model))
}

#### TO BE DEFINED BY THE USER

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model)
{
    #select a number of points from the history using the 
    #method's parameters and the current state of the model
    selectedIWDs <- historyPop(history, model$activeIWDs)
    
    return(selectedIWDs)
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(selectedPoints, oldModel, updatedEdges)
{
    newModel <- oldModel
    #update number of edges to take for next iteration
    newModel$activeIWDs <- length(selectedPoints)
    if(newModel$activeIWDs == 0) {
        return (newModel)
    }
    #update soil on edges
    for(i in 1:length(updatedEdges)) {
        idx <- getEdgeIndex(updatedEdges[[i]], newModel$edges)
        newModel$edges[[idx]]$soil <- newModel$edges[[idx]]$soil + updatedEdges[[i]]$auxSoil
    }
    
    
   #take a look at the list of selectedPoints and 
   #on the current state of the model, update it 
   #and then return
   return (newModel)
}

#generation of a LIST of new points
#to be defined
variation<-function(selectedPoints, model)
{
   #generate the list of newPoints and then  
   return (expandIWDs(selectedPoints, model))
}

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel)
{

   selectedPoints <- selection(history, oldModel)
   varResult <- variation(selectedPoints, oldModel)
   updatedEdges <- varResult$edges
   newPoints <- varResult$IWDs
   
   newModel <- modelUpdate(newPoints, oldModel, updatedEdges)
   
   return (list(newPoints=newPoints,newModel=newModel))
}

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun<-function(initialization, startPoints, termination, evaluation)
{
   history<-initialization(startPoints)
   history<-evaluateList(history)
   model<-initModel(history)
   while (!termination(history,model))
   {
      aa<-aggregatedOperator(history, model)
      aa$newPoints<-evaluateList(aa$newPoints, evaluation)
      history<-historyPush(history,aa$newPoints)
      model<-aa$newModel
   }
   return(history)
}

#push a LIST of points into the history
historyPush<-function(oldHistory, newPoints)
{
    newHistory<-c(oldHistory,newPoints)
    return (newHistory)
}
#read a LIST of points pushed recently into the history
historyPop<-function(history, number)
{
    stop=length(history)
    start=max(stop-number+1,1)
    return(history[start:stop])
}

#evaluate a LIST of points
evaluateList<-function(points,evaluation)
{
  for (i in 1:length(points))
     points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  return (points) 
}

last <- function(x) { return( x[length(x)] ) }

nodes <- list('A', 'B', 'C', 'D');
edges <- list( #access: edges[[i]]$field
    list(begin = nodes[[1]], end = nodes[[2]], length = 100, soil = 0, auxSoil = 0),
    list(begin = nodes[[1]], end = nodes[[3]], length = 120, soil = 0, auxSoil = 0),
    list(begin = nodes[[1]], end = nodes[[4]], length = 90, soil = 0, auxSoil = 0),
    list(begin = nodes[[2]], end = nodes[[3]], length = 60, soil = 0, auxSoil = 0),
    list(begin = nodes[[2]], end = nodes[[4]], length = 130, soil = 0, auxSoil = 0),
    list(begin = nodes[[3]], end = nodes[[4]], length = 70, soil = 0, auxSoil = 0)
    );

model = list(nodes = nodes, edges = edges)
#print(length(model$nodes))
#innerLoop(model)


IWDs <- list()
bestSolution <- NULL
iterMax = 10
iterCount = 0

#global algorithm constants
a_v = 1
b_v = 0.01
c_v = 1

a_s = 1
b_s = 0.01
c_s = 1

p_n = 0.9
p_IWD = 0.9
initSoil = 10000
initVel = 200

############################
#INITIZALIZATION PART
############################

#initialize soils
for (i in 1:length(model$edges)) {
    model$edges[[i]]$soil <- initSoil
}

#initialize IWDs
for (i in 1:length(nodes)) {
    #set initVel, set soil to 0, set IWD's starting points ("spread out nodes")
    IWDs[[length(IWDs) + 1]] <- list(
        id = i, 
        v = initVel, 
        soil = 0, 
        nodes = list(sample(nodes, 1)[[1]]),
        edges = list())
}

#test append to nodes list
#for (i in 1:length(nodes)) {
#    IWDs[[i]]$nodes[[length(IWDs[[i]]$nodes) + 1]] <- sample(nodes, 1)[[1]]
#}


############################
#EXAMPLE FIRST ITERATION
############################
#checks if specified node exists in specified path
f <- function(edge, possibleEdges) {
    epsilon <- 0.001
    return (1/(epsilon + g(edge, possibleEdges)))
}

g <- function(edge, possibleEdges) {
    minVal <- Inf
    for(i in 1:length(possibleEdges)) {
        minVal <- min(minVal, possibleEdges[[i]]$soil)
    }
    
    if(minVal >= 0) {
        return (edge$soil) 
    }
    
    return (edge$soil - minVal)
}

time <- function(edge, velocity) {
    #HUD(i, j) is defined as length of egde(i, j)
    return (edge$length/velocity)
}

existsInPath <- function(node, path)
{
    for(i in 1:length(path)) {
        if(path[[i]] == node) {
            return (TRUE)
        }
    }
    
    return (FALSE)
}

expandIWDs <- function(IWDs, model) {
    newIWDs <- list(IWDs = list(), edges = list())
    for (i in 1:length(IWDs)) {
        newIWD <- IWDs[[i]]
        #find nodes that were not visited by IWD yet
        lastNode <- newIWD$nodes[[length(newIWD$nodes)]]
        possibleEdges <- list()
        possibleNodes <- list()
        
        cat(c(i, ": possibleEdges\n"))
        edges <- model$edges
        for(j in 1:length(edges)) {
            #this edges starts from our last node (remember edges are bidirectional)
            if(edges[[j]]$begin == lastNode) {
                #only requirement is not to have the same node
                #twice or more times in generated path for
                #travelling salesman problem, which is also 
                #default requirement for the IWD
                
                if(existsInPath(edges[[j]]$end, newIWD$nodes) == FALSE) {
                    possibleEdges[[length(possibleEdges) + 1]] <- edges[[j]]
                    possibleNodes[[length(possibleNodes) + 1]] <- edges[[j]]$end
                }
                
            }else if(edges[[j]]$end == lastNode) {
                if(existsInPath(edges[[j]]$begin, newIWD$nodes) == FALSE) {
                    possibleEdges[[length(possibleEdges) + 1]] <- edges[[j]]
                    possibleNodes[[length(possibleNodes) + 1]] <- edges[[j]]$begin
                }
            }
        }
        
        #if possibleNodes is empty then this IWD has finished it's processing
        #(found a solution or became unable to move)
        if(length(possibleNodes) == 0) {
            next
        }
        
        #calculate probability vector
        possibleEdgesFSum <- 0
        probabilityList <- list()
        for(k in 1:length(possibleEdges)) {
            possibleEdgesFSum <- possibleEdgesFSum + f(possibleEdges[[k]], possibleEdges)
        }
        
        for(k in 1:length(possibleEdges)) {
            probabilityList[[length(probabilityList) + 1]] <- f(possibleEdges[[k]], possibleEdges)/possibleEdgesFSum
        }
        
        #print available edges with their probability of selection
        for(k in 1:length(possibleEdges)) {
            cat(c(lastNode, "->", possibleNodes[[k]], "| prob = ", probabilityList[[k]], "\n"))
        }
        
        #select index of next node
        selIndex <- sample(1:length(possibleEdges), 1, prob = probabilityList)
        cat(c("Selected edge: ", possibleEdges[[selIndex]]$begin, "->", possibleEdges[[selIndex]]$end, "\n"))
        
        #add node to IWD visited node list (remember that edge from A to B or from B to A
        #is represented once, so there are 2 cases here to consider depending where start is)
        if(possibleEdges[[selIndex]]$begin == lastNode) {
            newIWD$nodes[[length(newIWD$nodes) + 1]] <- possibleEdges[[selIndex]]$end
        } else {
            newIWD$nodes[[length(newIWD$nodes) + 1]] <- possibleEdges[[selIndex]]$begin
        }
        newIWD$edges[[length(newIWD$edges) + 1]] <- possibleEdges[[selIndex]]
        
        #print visited node list after adding new one
        cat("Visited nodes: [")
        for(k in 1:length(newIWD$nodes)) {
            cat(newIWD$nodes[[k]], ", ")
        }
        cat("\b\b\b]\n")
        
        #update velocity
        newIWD$v = newIWD$v + ((a_v)/(b_v + c_v * possibleEdges[[selIndex]]$soil^2))
        cat(c("Updated velocity: ", newIWD$v, "\n"))
        
        #calculate delta soil for selected edge
        dSoil <- ((a_s)/(b_s + c_s*time(possibleEdges[[selIndex]], newIWD$v)^2))
        cat(c("dSoil: ", dSoil, "\n"))
        
        #update soil for IWD and graph edge
        newIWD$soil <- newIWD$soil + dSoil
        edgeSoilChange <- (1 - p_n) * possibleEdges[[selIndex]]$soil - p_n * dSoil
        possibleEdges[[selIndex]]$auxSoil <- possibleEdges[[selIndex]]$auxSoil + edgeSoilChange
        
        newIWDs$IWDs[[length(newIWDs$IWDs) + 1]] <- newIWD
        newIWDs$edges[[length(newIWDs$edges) + 1]] <- possibleEdges[[selIndex]]
        
        cat(c("IWD soil: ", newIWD$soil , "\n"))
        cat(c("edge soil change: ", possibleEdges[[selIndex]]$auxSoil , "\n"))
        
    }
    
    return (newIWDs)
}
innerLoop(model)
stop("tak")
for (i in 1:length(IWDs)) {
    #find nodes that were not visited by IWD yet
    lastNode <- IWDs[[i]]$nodes[[length(IWDs[[i]]$nodes)]]
    possibleEdges <- list()
    possibleNodes <- list()
    cat(c(i, ": possibleEdges\n"))
    for(j in 1:length(edges)) {
        #this edges starts from our last node (remember edges are bidirectional)
        if(edges[[j]]$begin == lastNode) {
            #only requirement is not to have the same node
            #twice or more times in generated path for
            #travelling salesman problem, which is also 
            #default requirement for the IWD

            if(existsInPath(edges[[j]]$end, IWDs[[i]]$nodes) == FALSE) {
                possibleEdges[[length(possibleEdges) + 1]] <- edges[[j]]
                possibleNodes[[length(possibleNodes) + 1]] <- edges[[j]]$end
            }
            
        }else if(edges[[j]]$end == lastNode) {
            if(existsInPath(edges[[j]]$begin, IWDs[[i]]$nodes) == FALSE) {
                possibleEdges[[length(possibleEdges) + 1]] <- edges[[j]]
                possibleNodes[[length(possibleNodes) + 1]] <- edges[[j]]$begin
            }
        }
    }
    
    #if possibleNodes is empty then this IWD has finished it's processing
    #(found a solution or became unable to move)
    #TODO this part!
    
    possibleEdgesFSum <- 0
    probabilityList <- list()
    for(k in 1:length(possibleEdges)) {
        possibleEdgesFSum <- possibleEdgesFSum + f(possibleEdges[[k]], possibleEdges)
    }
    
    for(k in 1:length(possibleEdges)) {
        probabilityList[[length(probabilityList) + 1]] <- f(possibleEdges[[k]], possibleEdges)/possibleEdgesFSum
    }
    
    #print available edges with their probability of selection (DEBUG)
    for(k in 1:length(possibleEdges)) {
        cat(c(lastNode, "->", possibleNodes[[k]], "| prob = ", probabilityList[[k]], "\n"))
    }
    
    #select index of next node
    selIndex <- sample(1:length(possibleEdges), 1, prob = probabilityList)
    cat(c("Selected edge: ", possibleEdges[[selIndex]]$begin, "->", possibleEdges[[selIndex]]$end, "\n"))
    
    #add node to IWD visited node list (remember that edge from A to B or from B to A
    #is represented once, so there are 2 cases here to consider depending where start is)
    if(possibleEdges[[selIndex]]$begin == lastNode) {
        IWDs[[i]]$nodes[[length(IWDs[[i]]$nodes) + 1]] <- possibleEdges[[selIndex]]$end
    } else {
        IWDs[[i]]$nodes[[length(IWDs[[i]]$nodes) + 1]] <- possibleEdges[[selIndex]]$begin
    }
    IWDs[[i]]$edges[[length(IWDs[[i]]$edges) + 1]] <- possibleEdges[[selIndex]]
    
    #print visited node list after adding new one
    cat("Visited nodes: [")
    for(k in 1:length(IWDs[[i]]$nodes)) {
        cat(IWDs[[i]]$nodes[[k]], ", ")
    }
    cat("\b\b\b]\n")
    
    #update velocity
    IWDs[[i]]$v = IWDs[[i]]$v + ((a_v)/(b_v + c_v * possibleEdges[[selIndex]]$soil^2))
    cat(c("Updated velocity: ", IWDs[[i]]$v, "\n"))
    
    #calculate delta soil for selected edge
    dSoil <- ((a_s)/(b_s + c_s*time(possibleEdges[[selIndex]], IWDs[[i]]$v)^2))
    cat(c("dSoil: ", dSoil, "\n"))
    
    #update soil for IWD and graph edge
    IWDs[[i]]$soil <- IWDs[[i]]$soil + dSoil
    edgeSoilChange <- (1 - p_n) * possibleEdges[[selIndex]]$soil - p_n * dSoil
    possibleEdges[[selIndex]]$auxSoil <- possibleEdges[[selIndex]]$auxSoil + edgeSoilChange
    
    cat(c("IWD soil: ", IWDs[[i]]$soil , "\n"))
    cat(c("edge soil change: ", possibleEdges[[selIndex]]$auxSoil , "\n"))
    
}

#update soil on edges
for (i in 1:length(edges)) {
    edges[[i]]$soil <- edges[[i]]$soil + edges[[i]]$auxSoil
    edges[[i]]$auxSoil <- 0
}

#solution quality calculation
#TEMP: assumes that IWDs[[1]] is iteration-best solution
isEdgeInList <- function(edge, edgeList) {
    cat(c("testing for edge: ", edge$begin, "->", edge$end, "\n"))
    for(i in 1:length(edgeList)) { 
        listEdge <- edgeList[[i]]
        cat(c("testing with edge: ", listEdge$begin, "->", listEdge$end, "\n"))
        if(listEdge$begin == edge$begin && listEdge$end == edge$end) {
            return (TRUE)
        }
    }
    
    return (FALSE)
}
#sort
lengths <- sapply(edges,"[[","length")
sorted <- edges[order(lengths)]
#print sorted edges
print("Sorted all edges by length:")
for(i in 1:length(sorted)) {
    cat(c(sorted[[i]]$begin, "->", sorted[[i]]$end, " length = ", sorted[[i]]$length, "\n"))
}

quality <- 0
for(i in 1:(length(nodes) - length(IWDs[[1]]$edges))) {
    #for(j in 1:length(IWDs[[1]]$edges))
}

gatheredShortestEdges <- 0
neededShortestEdges <- (length(nodes) - length(IWDs[[1]]$edges))
iterNum <- 1

cat(c("neededShortestEdges", neededShortestEdges, "\n"))

while(gatheredShortestEdges < neededShortestEdges ) {
    shortestEdge <- sorted[[iterNum]]
    iterNum <- iterNum + 1
    
    
    if(isEdgeInList(shortestEdge, IWDs[[1]]$edges) == TRUE) {
        next;
    }
    
    quality <- quality + shortestEdge$length
    gatheredShortestEdges <- gatheredShortestEdges + 1
}
for(i in 1:length(IWDs[[1]]$edges)) {
    quality <- quality + IWDs[[1]]$edges[[i]]$length
}
cat(c("solution quality: ", quality, "\n"))

#update the soils on paths that form current iteration-best solution
#TEMP: assumes that IWDs[[1]] is iteration-best solution
for(i in 1:length(IWDs[[1]]$edges)) {
    #number of edges + 1 is number of nodes
    N_IB <- length(IWDs[[1]]$edges) + 1 
    IWDs[[1]]$edges[[i]] <- (1 + p_IWD) * IWDs[[1]]$edges[[i]]$soil
        - (p_IWD) * ((1)/(N_IB - 1)) * IWDs[[1]]$soil
}


