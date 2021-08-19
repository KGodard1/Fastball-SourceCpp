fastball <- function(bipartiteData, numRows, numCols, checkType=TRUE) {


  if (checkType=FALSE) {
    return (fastball(bipartiteData, c(numRows,numCols)))

  } else {

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
