
fastball <- function(bipartiteData, numRows, numCols, indexed=FALSE) {

  if (indexed=TRUE) {
    return (fastball(bipartiteData, c(numRows,numCols)))

  } else {
    #### Class Conversion ####
    convert <- tomatrix(B)
    class <- convert$summary$class
    B <- convert$G
    if (convert$summary$weighted==TRUE){stop("Graph must be unweighted.")}

    #### Mark Locations of One's ####
    oneLocations = List()
    for (row in 1:numRows) {oneLocations[[row]]=(which(bipartiteData[row,]==1))}

    #### Run Fastball Algorithm ####
    randomizedLocations = fastball(oneLocations, c(numRows,numCols))

    #### Convert One Locations to Matrix ####
    randomizedMatrix=matrix(0,numRows,numCols)
    for (row in 1:R){
      randomizedMatrix[row,randomizedLocations[[row]]]=1
    }

    #### Return Randomized Matrix ####
    return (randomizedMatrix)
  }


}

fastballAlternate <- function(bipartiteData, numRows, numCols, checkType=TRUE) {


  if (checkType=FALSE) {
    return (fastball(bipartiteData, c(numRows,numCols)))

  } else {
    ### Check If Input is Indexed ###

    #TODO

    ### Process Input #####

    #TODO

    #### Mark Locations of One's ####
    oneLocations = List()
    for (row in 1:numRows) {oneLocations[[row]]=(which(bipartiteData[row,]==1))}

    #### Run Fastball Algorithm ####
    randomizedLocations = fastball(oneLocations, c(numRows,numCols))

    #### Convert One Locations to Matrix ####
    randomizedMatrix=matrix(0,numRows,numCols)
    for (row in 1:R){
      randomizedMatrix[row,randomizedLocations[[row]]]=1
    }

    #### Return Randomized Matrix ####
    return (randomizedMatrix)
  }


}
