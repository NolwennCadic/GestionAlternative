setwd("C:\\Users\\Thomas\\Documents\\Cours\\3A\\Gestion Alternative");

datas <- read.csv(file='data_final_facteurs_fusionne_2018.csv',header=TRUE, sep=",");

datas_year_month <- datas[, c(0:3)];

datas_fm = data.frame(datas);

prev_stock_number = datas_fm[which(datas_fm$year == 2005), ];

prev_stock_number_unique = unique(prev_stock_number$stock_number);
stock_number = prev_stock_number;
stock_number_unique = prev_stock_number_unique;

#Determine les 100 titres à analyser 
#y = 2004;
#while (length(prev_stock_number_unique) >= 100 && y > 1966) {
#  stock_number = datas_fm[which(datas_fm$year == y), ];
"  stock_number_unique = unique(stock_number$stock_number);
  prev_stock_number_unique = intersect(prev_stock_number_unique, stock_number_unique);
  length(prev_stock_number_unique)
  y = y - 1;
}"

#datas_fm$return_rf[datas_fm$year == year & datas_fm$stock_number == stock_number]
y = 2004;
prev_stock_number_unique = unique(datas_fm$stock_number);
while (length(prev_stock_number_unique) > 100 && y > 1966) {
  stock_numbers = unique(datas_fm$stock_number[datas_fm$year == y]);
  stock_numbers = check_values(stock_numbers, y);
  prev_stock_number_unique = intersect(prev_stock_number_unique, stock_numbers);
  y = y - 1;
}
cat('Year = ')
cat(y + 1)
cat("\n")
cat("Stocks list : ")
cat(prev_stock_number_unique);
cat("\n")
cat("Longueur echantillon stocks : ")
cat(length(prev_stock_number_unique))

check_values <- function(list_stock, year) {
  for(stock in list_stock) {
    if(length(datas_fm$return_rf[datas_fm$stock_number == stock & datas_fm$year == year]) != 12) {
      #cat(list_stock);
      list_stock = list_stock[list_stock != stock]
    }
  }
  return(list_stock);
}

#Nouvelle data frame avec les données des 100 titres entre 1981 et 2005
datas_stock.df = datas_fm[which(datas_fm$stock_number %in% prev_stock_number_unique) ,];
datas_stock.df = datas_stock.df[which(datas_stock.df$year >= 1981), ];
#datas_stock.df

CalculeRenta <- function(bMonth, eMonth, bYear, eYear, stockNum) {
  value = 1;
  compteur = 0;
  for (month in bMonth:eMonth) {
    rentaMoinsRf = datas_stock.df$return_rf[datas_stock.df$month == month & datas_stock.df$year == bYear & datas_stock.df$stock_number == stockNum];
    rentaMoinsRf;
    rf = datas_stock.df$RiskFreeReturn[datas_stock.df$month == month & datas_stock.df$year == bYear & datas_stock.df$stock_number == stockNum];
    rf;
    if(length(rentaMoinsRf) != 0) {
      "coucou";
      value = value *(1 + rentaMoinsRf + rf);
      compteur = compteur + 1;
    }
  }
  return (value^(1/compteur) -1);
} 

#CalculeRenta(1, 12, 1982, 1985, 383);
colNames = c("stock_number", "year", "return_6_month");
renta6months.df = data.frame(matrix(nrow=0, ncol=3));
colnames(portfolio.df) <- colNames;
list_portefeuille = c();
populateDataFrame <- function(){
  port_df = data.frame(matrix(nrow=0, ncol=3));
  colnames(port_df) <- colNames;
  for (year in 1984:2005){
    for (stock_number in  prev_stock_number_unique){
      monthlyReturn = CalculeRentaOverYears(1, 6, year, year, stock_number);
      #cat(monthlyReturn);

        list_tmp <- c(stock_number, year, monthlyReturn);
        
        port_df[nrow(port_df) + 1,] <- list_tmp;
      
    }
  }
  return(port_df);
}


CalculeRentaOverYears <- function(bMonth, eMonth, bYear, eYear, stockNum) {
  value = 1;
  compteur = 0;
  if(eYear < bYear || (eYear == bYear && bMonth > eMonth)) { #Error period.
    cat("Error return period");
    return(0);
  }
  if(eYear > bYear) {#Good period and return over different years.
    year = bYear
    month = bMonth
    while(year<eYear || (year == eYear && month<=eMonth)) {
      rentaMoinsRf = datas_stock.df$return_rf[datas_stock.df$month == month & datas_stock.df$year == year & datas_stock.df$stock_number == stockNum];
      rf = datas_stock.df$RiskFreeReturn[datas_stock.df$month == month & datas_stock.df$year == year & datas_stock.df$stock_number == stockNum];
      if(length(rentaMoinsRf) != 0) {
        value = value *(1 + rentaMoinsRf + rf);
        compteur = compteur + 1;
      }else{
        cat("Missing value : ");
        cat("\n");
      }
      if(month == 12) {
        year = year + 1;
        month = 1;
      }else {
        month = month + 1;
      }
    }
    return (value**(1/compteur) -1);
  }else{#Good period and return over the same year.
    for (month in bMonth:eMonth) {
      rentaMoinsRf = datas_stock.df$return_rf[datas_stock.df$month == month & datas_stock.df$year == bYear & datas_stock.df$stock_number == stockNum];
      rentaMoinsRf;
      rf = datas_stock.df$RiskFreeReturn[datas_stock.df$month == month & datas_stock.df$year == bYear & datas_stock.df$stock_number == stockNum];
      rf;
      if(length(rentaMoinsRf) != 0) {
        value = value *(1 + rentaMoinsRf + rf);
        compteur = compteur + 1;
      }else{
        cat("Missing value")
        cat("\n")
      }
    }
    return (value**(1/compteur) -1);
  }
}


renta6months.df = populateDataFrame();
#Tri la dataframe
sortedPortfolio <- renta6months.df[with(renta6months.df, order(renta6months.df$return_6_month)), ];
sortedPortfolio1984 <- sortedPortfolio[sortedPortfolio$year == 1984, ];
#Data frame pour stocker tous les portefeuilles P1 et P10 entre 1984 et 2005
GetAllP = function() {
  port_df = data.frame(matrix(nrow=0, ncol=5));
  colNames = c("stock_number", "month", "year", "portfolio number", "return");
  colnames(port_df) <- colNames;
  for (year in 1984:1984) {
    sortedPortfolioyear <- sortedPortfolio[sortedPortfolio$year == year, ];
    lenPort = nrow(sortedPortfolioyear);
    P10 = sortedPortfolioyear[1:10,];
    deb = lenPort - 9;
    P1 = sortedPortfolioyear[deb:lenPort, ];
    for (row in 1:10) {
      for (month in 1:6) {
          list_tmp <- c(P10$stock_number[row], 6 + month, year, "P10", CalculeRentaOverYears(7, 6, year, year + 1, P10$stock_number[row]));
          port_df[nrow(port_df) + 1,] <- list_tmp;
          list_tmp_2 <- c(P1$stock_number[row], 6 + month, year, "P1", CalculeRentaOverYears(7, 6, year, year + 1, P1$stock_number[row]));
          port_df[nrow(port_df) + 1,] <- list_tmp_2;
      }
      for (month in 7:12) {
        list_tmp <- c(P10$stock_number[row], month - 6, year + 1, "P10", CalculeRentaOverYears(7, 6, year, year + 1, P10$stock_number[row]));
        port_df[nrow(port_df) + 1,] <- list_tmp;
        list_tmp_2 <- c(P1$stock_number[row], month - 6, year + 1, "P1", CalculeRentaOverYears(7, 6, year, year + 1, P1$stock_number[row]));
        port_df[nrow(port_df) + 1,] <- list_tmp_2;
      }
    }
  }
  return (port_df);
}

allP = GetAllP();
test = allP[allP$portefolio == "P1"];




#Calcule des portefeuilles P1  et P10 ou P1 est le portefeuille le plus performant et P10 le moins
#P10 = sortedPortfolio[1:10,];
#lenPort = nrow(sortedPortfolio);
#lenPort;
#deb = lenPort - 9;
#P1 = sortedPortfolio[deb:lenPort, ];
#P1;
