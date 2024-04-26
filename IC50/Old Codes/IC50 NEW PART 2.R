library(data.table)
library(dr4pl)

multiIC <- function(data, 
                    colDose, colResp, colID, 
                    inhib.percent, 
                    ...) {
  
  # Get curve IDs
  locID <- unique(data[[colID]])
  
  # Prepare a vector to store IC50s
  locIC <- rep(NA, length(locID))
  
  # Calculate IC50 separately for every curve
  for (ii in seq_along(locID)) {
    # Subset a single dose response
    locSub <- data[get(colID) == locID[[ii]], ]
    
    # Calculate IC50
    locIC[[ii]] <- dr4pl::IC(
      dr4pl::dr4pl(dose = locSub[[colDose]], 
                   response = locSub[[colResp]],
                   ...), 
      inhib.percent)
  }
  
  return(data.frame(id = locID,
                    x = locIC))
}

dfIC50 <- multiIC(data = example2, 
                  colDose = "dose", 
                  colResp = "POC", 
                  colID = "curve", 
                  inhib.percent = 50)

> dfIC50
id         x
1 C1 29.065593
2 C2  3.269516
3 C3  2.186479

geom_vline(data = dfIC50, 
           aes(xintercept = x, 
               color = id))