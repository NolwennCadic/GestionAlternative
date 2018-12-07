setwd("/user/2/.base/cadicn/home/Documents/3A/GA");

datas <- read.csv(file='data_final_facteurs_fusionne_2018.csv',header=TRUE, sep=",");

datas_year_month <- datas[, c(0:3)];

datas_fm = data.frame(datas);

prev_stock_number = datas_fm[which(datas_fm$year == 2005), ];

prev_stock_number_unique = unique(prev_stock_number$stock_number);
stock_number = prev_stock_number;
stock_number_unique = prev_stock_number_unique;

#Determine les 100 titres à analyser 
y = 2004;
while (length(prev_stock_number_unique) >= 100 && y > 1966) {
  stock_number = datas_fm[which(datas_fm$year == y), ];
  stock_number_unique = unique(stock_number$stock_number);
  prev_stock_number_unique = intersect(prev_stock_number_unique, stock_number_unique);
  length(prev_stock_number_unique)
  y = y - 1;
}

prev_stock_number_unique;

y;

#Nouvelle data frame avec les données des 100 titres entre 1981 et 2005
datas_stock.df = datas_fm[which(datas_fm$stock_number %in% prev_stock_number_unique) ,];
datas_stock.df = datas_stock.df[which(datas_stock.df$year >= 1981), ];
datas_stock.df

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
  return (value^1/compteur -1);
}

CalculeRenta(1, 12, 1982, 1985, 383);
colNames = c("stock_number", "year", "return_6_month");
renta6months.df = data.frame(matrix(nrow=0, ncol=3));
colnames(portfolio.df) <- colNames;
list_portefeuille = c();
populateDataFrame <- function(){
  port_df = data.frame(matrix(nrow=0, ncol=3));
  colnames(port_df) <- colNames;
  for (year in 1984:2005){
    for (stock_number in  prev_stock_number_unique){
      monthlyReturn = CalculeRenta(1, 6, year, year, stock_number);
      #cat(monthlyReturn);

        list_tmp <- c(stock_number, year, monthlyReturn);
        
        port_df[nrow(port_df) + 1,] <- list_tmp;
      
    }
  }
  return(port_df);
}

renta6months.df = populateDataFrame();
#Tri la dataframe
sortedPortfolio <- renta6months.df[with(renta6months.df, order(renta6months.df$return_6_month)), ];
sortedPortfolio1984 <- sortedPortfolio[sortedPortfolio$year == 1984, ];
#Data frame pour stocker tous les portefeuilles P1 et P10 entre 1984 et 2005
GetAllP = function() {
  port_df = data.frame(matrix(nrow=0, ncol=4));
  colNames = c("stock_number", "month", "year", "portefolio");
  colnames(port_df) <- colNames;
  for (year in 1984:1987) {
    sortedPortfolioyear <- sortedPortfolio[sortedPortfolio$year == year, ];
    lenPort = nrow(sortedPortfolioyear);
    P10 = sortedPortfolioyear[1:10,];
    deb = lenPort - 9;
    P1 = sortedPortfolioyear[deb:lenPort, ];
    for (row in 1:10) {
      for (month in 1:6) {
        list_tmp <- c(P10$stock_number[row], 6 + month, year, "P10");
        port_df[nrow(port_df) + 1,] <- list_tmp;
        list_tmp_2 <- c(P1$stock_number[row], 6 + month, year, "P1");
        port_df[nrow(port_df) + 1,] <- list_tmp_2;
      }
      for (month in 7:12) {
        list_tmp <- c(P10$stock_number[row], month - 6, year + 1, "P10");
        port_df[nrow(port_df) + 1,] <- list_tmp;
        list_tmp_2 <- c(P1$stock_number[row], month - 6, year + 1, "P1");
        port_df[nrow(port_df) + 1,] <- list_tmp_2;
      }
    }
  }
  return (port_df);
}

allP = GetAllP();
test = allP[allP$portefolio == "P1", ];

#Calcule des portefeuilles P1 et P10 ou P1 est le portefeuille le plus performant et P10 le moins
#P10 = sortedPortfolio[1:10,];
#lenPort = nrow(sortedPortfolio);
#lenPort;
#deb = lenPort - 9;
#P1 = sortedPortfolio[deb:lenPort, ];
#P1;
