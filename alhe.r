#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must 
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs 
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################




#### TO BE DEFINED BY THE USER

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model)
{
   #select a number of points from the history using the 
   #method's parameters and the current state of the model
   return(selectedPoints)
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(selectedPoints, oldModel)
{
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
   return (newPoints)
}

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel)
{

   selectedPoints<-selection(history, oldModel)
   newModel<-modelUpdate(selectedPoints, oldModel)
   newPoints<-variation(selectedPoints, newModel)
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
    list(begin = nodes[[1]], end = nodes[[2]], length = 100, soil = 0),
    list(begin = nodes[[1]], end = nodes[[3]], length = 120, soil = 0),
    list(begin = nodes[[1]], end = nodes[[4]], length = 90, soil = 0),
    list(begin = nodes[[2]], end = nodes[[1]], length = 100, soil = 0),
    list(begin = nodes[[2]], end = nodes[[3]], length = 60, soil = 0),
    list(begin = nodes[[2]], end = nodes[[4]], length = 130, soil = 0),
    list(begin = nodes[[3]], end = nodes[[1]], length = 120, soil = 0),
    list(begin = nodes[[3]], end = nodes[[2]], length = 60, soil = 0),
    list(begin = nodes[[3]], end = nodes[[4]], length = 70, soil = 0),
    list(begin = nodes[[4]], end = nodes[[1]], length = 90, soil = 0),
    list(begin = nodes[[4]], end = nodes[[2]], length = 130, soil = 0),
    list(begin = nodes[[4]], end = nodes[[3]], length = 70, soil = 0)
    );

IWDs <- list()
bestSolution <- NULL
iterMax = 10
iterCount = 0

a_v = 1
b_v = 0.01
c_1 = 1

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
for (i in 1:length(edges)) {
    edges[[i]]$soil <- initSoil
}

#initialize IWDs
for (i in 1:length(nodes)) {
    #set initVel, set soil to 0, set IWD's starting points ("spread out nodes")
    IWDs[[length(IWDs) + 1]] <- list(id = i, v = initVel, soil = 0, nodes = list(sample(nodes, 1)[[1]]))
}

#test append to nodes list
#for (i in 1:length(nodes)) {
#    IWDs[[i]]$nodes[[length(IWDs[[i]]$nodes) + 1]] <- sample(nodes, 1)[[1]]
#}


############################
#EXAMPLE FIRST ITERATION
############################
for (i in 1:length(IWDs)) {
    #find nodes that were not visited by IWD yet
    lastNode <- IWDs[[i]]$nodes[[length(IWDs[[i]]$nodes)]]
    possibleEdges <- list()
    cat(c(i, ": possibleEdges\n"))
    for(j in 1:length(edges)) {
        #this edges starts from our last node
        if(edges[[j]]$begin == lastNode) {
            #only requirement is not to have the same node
            #twice or more times in generated path for
            #travelling salesman problem, which is also 
            #default requirement for the IWD
            possibleEdges[[length(possibleEdges) + 1]] <- edges[[j]]
        }
    }
    for(k in 1:length(possibleEdges)) {
        cat(c(possibleEdges[[k]]$begin, "->", possibleEdges[[k]]$end, "\n"))
        #print(j + ": " + possibleEdges[[j]]$begin + "->" + possibleEdges[[j]]$end)
    }
    
}

