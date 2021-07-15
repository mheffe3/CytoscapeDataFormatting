\name{CytoSort}
\alias{CytoSort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{CytoSort
%%  ~~function to do ... ~~
}
\description{Takes in a CSV file in the form of a table and outputs a CSV file that will work as a network file in Cytoscape where each column is sorted for the top 20 highest values 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CytoSort(importPath,exportPath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{importPath}{The path to the file being input}
  \item{exportPath}{The path to the directory where the new file will be saved}
%%     ~~Describe \code{x} here~~

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Has a null return type because it exports its output to the given exportPath
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Matt Heffernan, University of Illinois at Chicago
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{CytoSort("C:/Users/matth/OneDrive/Desktop/W.csv","C:/Users/matth/OneDrive/Desktop/Data.csv")
Takes in the path to the file W.csv and takes the top 20 highest values in each column and creates a file Data.csv which is formatted as a Cytoscape network file.
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
CytoSort<-function(importPath,exportPath)
{
  if(!(file.exists(importPath)))
  {
    return("Import Path Does Not Exist Try Again")
  }
  if(file.exists(exportPath))
  {
    return("Export Path Already Exists. Try A New Path")
  }
  cytoData <- read.csv(file = importPath)
  TF = names(cytoData[2])
  TF = rep(c(TF), times = 20)
  top20Data <- data.frame(TF)
  count = 1
  while(count < ncol(cytoData))
  {
    curcol = unlist(cytoData[count+1])
    TF = names(cytoData[count+1])
    TFData <- cytoData[with(cytoData,order(-curcol)),]
    TFData <- TFData[1:20,]
    TFData <-TFData[c("GENE", TF)]
    colnames(TFData)[2] <- "W.Values"
    
    if(count == 1)
    {
      top20Data <- cbind(top20Data,TFData)
      
    }
    else
    {
      TFData <- data.frame(TF, TFData)
      top20Data <- rbind(top20Data,TFData)
      
    }
    count=count+1
    
    
    
  }
  View(top20Data)
  write.csv(top20Data,exportPath,row.names=FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
