setwd("C:\\Users\\Thomas\\Documents\\Cours\\3A\\Gestion Alternative");
#setwd("/user/2/.base/cadicn/home/Documents/3A/GA");

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


check_values <- function(list_stock, year) {
  for(stock in list_stock) {
    if(length(datas_fm$return_rf[datas_fm$stock_number == stock & datas_fm$year == year]) != 12) {
      #cat(list_stock);
      list_stock = list_stock[list_stock != stock]
    }
  }
  return(list_stock);
}

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


#Calcule de la rentabilité d'un titre entre les périodes données
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
#Data frame pour stocker tous les portefeuilles P1 et P10 entre 1984 et 2005, composition à chaque mois et renta chaque année
GetAllP = function() {
  port_df = data.frame(matrix(nrow=0, ncol=5));
  colNames = c("stock_number", "month", "year", "portfolio number", "return");
  colnames(port_df) <- colNames;
  for (year in 1984:2004) {
    sortedPortfolioyear <- sortedPortfolio[sortedPortfolio$year == year, ];
    lenPort = nrow(sortedPortfolioyear);
    P1 = sortedPortfolioyear[1:10,];
    deb = lenPort - 9;
    P10 = sortedPortfolioyear[deb:lenPort, ];
    for (row in 1:10) {
      for (month in 1:6) {
          data_row = datas_stock.df[datas_stock.df$stock_number == P1$stock_number[row] & datas_stock.df$year == year & datas_stock.df$month == month + 6, ]
          list_tmp <- c(P1$stock_number[row], 6 + month, year, "P1", data_row$return_rf + data_row$RiskFreeReturn);
          port_df[nrow(port_df) + 1,] <- list_tmp;
          data_row = datas_stock.df[datas_stock.df$stock_number == P10$stock_number[row] & datas_stock.df$year == year & datas_stock.df$month == month + 6, ]
          list_tmp_2 <- c(P10$stock_number[row], 6 + month, year, "P10", data_row$return_rf + data_row$RiskFreeReturn);
          port_df[nrow(port_df) + 1,] <- list_tmp_2;
      }
      for (month in 7:12) {
        data_row = datas_stock.df[datas_stock.df$stock_number == P1$stock_number[row] & datas_stock.df$year == year + 1 & datas_stock.df$month == month - 6, ]
        list_tmp <- c(P1$stock_number[row], month - 6, year + 1, "P1", data_row$return_rf + data_row$RiskFreeReturn);
        port_df[nrow(port_df) + 1,] <- list_tmp;
        data_row = datas_stock.df[datas_stock.df$stock_number == P10$stock_number[row] & datas_stock.df$year == year + 1 & datas_stock.df$month == month - 6, ]
        list_tmp_2 <- c(P10$stock_number[row], month - 6, year + 1, "P10", data_row$return_rf + data_row$RiskFreeReturn);
        port_df[nrow(port_df) + 1,] <- list_tmp_2;
      }
    }
  }
  return (port_df);
}

allP = GetAllP();
test = allP[allP$portefolio == "P1"];
colNames2 = c('Portfolio', 'Return', 'ReturnM', 'Returnf', 'stdevP', 'stdevM');
portfolios = data.frame(matrix(nrow=0, ncol=6));

#Calcule de la renta totale de P1, P10 et P10-P1
returnPeriodPort = function(YearRange) {
  for(year in YearRange){
    #calcule renta de rf sur la periode entre juillet de year et juin de year + 1
    dataSelectedPeriod = datas_stock.df[((datas_stock.df$year == year & datas_stock.df$month > 6) | (datas_stock.df$year == year + 1 & datas_stock.df$month < 7))  & datas_stock.df$stock_number == 559, ]
    returnf = mean(dataSelectedPeriod$RiskFreeReturn);
    returnm = mean(dataSelectedPeriod$Marketretrun);
    stdevRm = sd(dataSelectedPeriod$Marketretrun);
    pYearReturn = as.numeric(allP$return[allP$`portfolio number`=='P1' & allP$year==year & allP$month==7]);
    returnP = mean(pYearReturn);
    stdevP = sd(pYearReturn);
    list_tmp = c(year, returnP, returnm, returnf, stdevP, stdevRm);
    portfolios[nrow(portfolios) + 1,] <- list_tmp;
  }
}

returnPeriodPortByYear = returnPeriodPort(1984:1984);



returnPortByTime = function(byear, eYear) {
  colNames3 = c('year', 'month', 'returnP1', 'returnP10', 'returnP10MinusP1', 'RM', 'RF');
  returnByPort = data.frame(matrix(nrow=0, ncol=7));
  colnames(returnByPort) <- colNames3;
  for (month in 7:12) {
    Rm <- datas_stock.df$Marketretrun[datas_stock.df$year == byear & datas_stock.df$month==month & datas_stock.df$stock_number==559]
    Rf <- datas_stock.df$RiskFreeReturn[datas_stock.df$year == byear & datas_stock.df$month==month & datas_stock.df$stock_number==559]
    returnP1 = mean(as.numeric(allP$return[allP$`portfolio number`=='P1'& allP$year==byear & allP$month==month]));
    returnP10 = mean(as.numeric(allP$return[allP$`portfolio number`=='P10'& allP$year== byear & allP$month==month]));
    returnP10minusP1 = returnP10 - returnP1;
    list_tmp <- c(byear, month, returnP1, returnP10, returnP10minusP1, Rm, Rf);
    returnByPort[nrow(returnByPort) + 1, ] <- list_tmp;
  }
  endYear = eYear - 1
  beginYear = byear + 1;
  for (y in beginYear:endYear) {
    for (month in 1:12) {
      Rm <- datas_stock.df$Marketretrun[datas_stock.df$year == y & datas_stock.df$month==month & datas_stock.df$stock_number==559]
      Rf <- datas_stock.df$RiskFreeReturn[datas_stock.df$year == y & datas_stock.df$month==month & datas_stock.df$stock_number==559]
      returnP1 = mean(as.numeric(allP$return[allP$`portfolio number`=='P1'& allP$year==y & allP$month==month]));
      returnP10 = mean(as.numeric(allP$return[allP$`portfolio number`=='P10'& allP$year==y & allP$month==month]));
      returnP10minusP1 = returnP10 - returnP1;
      list_tmp <- c(y, month, returnP1, returnP10, returnP10minusP1, Rm, Rf);
      returnByPort[nrow(returnByPort) + 1, ] <- list_tmp;
    }
  }
  for (month in 1:6) {
    Rm <- datas_stock.df$Marketretrun[datas_stock.df$year == eYear & datas_stock.df$month==month & datas_stock.df$stock_number==559]
    Rf <- datas_stock.df$RiskFreeReturn[datas_stock.df$year == eYear & datas_stock.df$month==month & datas_stock.df$stock_number==559]
    returnP1 = mean(as.numeric(allP$return[allP$`portfolio number`=='P1'& allP$year==eYear & allP$month==month]));
    returnP10 = mean(as.numeric(allP$return[allP$`portfolio number`=='P10'& allP$year== eYear & allP$month==month]));
    returnP10minusP1 = returnP10 - returnP1;
    list_tmp <- c(eYear, month, returnP1, returnP10, returnP10minusP1, Rm, Rf);
    returnByPort[nrow(returnByPort) + 1, ] <- list_tmp;
  }
  return (returnByPort)
}

returnByPort = returnPortByTime(1984, 2005);

#-------------------------------------------------------------------------------------------------------------------
#----------------------------------------------Mesures de performance-----------------------------------------------

SharpeRatio <- function(bYear, bMonth, eYear, eMonth){
  dataSelectedPeriod = returnByPort[(returnByPort$year == bYear & returnByPort$month >= bMonth) | (returnByPort$year < eYear) |(returnByPort$year == eYear & returnByPort$month <= eMonth),]
  returnf = mean(dataSelectedPeriod$RF);
  returnm = mean(dataSelectedPeriod$RM);
  stdevRm = sd(dataSelectedPeriod$RM);
  P1Return = mean(dataSelectedPeriod$returnP1);
  P10Return = mean(dataSelectedPeriod$returnP10);
  P10MinusP1Return = mean(dataSelectedPeriod$returnP10MinusP1);
  stdevP1 = sd(dataSelectedPeriod$returnP1);
  stdevP10 = sd(dataSelectedPeriod$returnP10);
  stdevP10MinusP1 = sd(dataSelectedPeriod$returnP10MinusP1);
  
  SharpeRM <- (returnm - returnf)/stdevRm;
  SharpeP1 <- (P1Return - returnf)/stdevP1;
  SharpeP10 <- (P10Return - returnf)/stdevP10;
  SharpeP10MinusP1 <- (P10MinusP1Return - returnf)/stdevP10MinusP1;
  
  colNamesSharpe = c('Sharpe Market', 'Sharpe P1', 'Sharpe P10', 'Sharpe P10 Minus P1');
  Sharpe = data.frame(matrix(nrow=0, ncol=4));
  colnames(Sharpe) <- colNamesSharpe;
  list <- c(SharpeRM, SharpeP1, SharpeP10, SharpeP10MinusP1);
  Sharpe[nrow(Sharpe) + 1, ] <- list;
  
  return(Sharpe);
}


# La stratégie 6 mois 12 mois bne fonctionne pas très bien, titres de grandes tailles dans le portefeuille,
# info s'intègre lus rapidement.
MeasureSharpe <-SharpeRatio(1984, 7, 2005, 5)


#--------------------------------------------------------------------------------------------------------------------
#------------------------------------------------Alpha de Jensen et bet�---------------------------------------------


colNamesRegression = c('year', 'month', 'returnP1-Rf', 'returnP10-Rf', 'returnP10MinusP1-Rf', 'RM-Rf', "SMB", "HML", "MOM");
returnByPortRegression = data.frame(matrix(nrow=0, ncol=9));
colnames(returnByPortRegression) <- colNamesRegression;

for(i in 1:nrow(returnByPort))
{
  year <- returnByPort$year[i];
  month <- returnByPort$month[i];
  temp <- c(year, month,
            returnByPort$`returnP1`[i]-returnByPort$RF[i],
            returnByPort$`returnP10`[i]-returnByPort$RF[i],
            returnByPort$`returnP10MinusP1`[i]-returnByPort$RF[i],
            returnByPort$`RM`[i]-returnByPort$RF[i], 
            datas_fm$SmallMinusBig[datas_fm$year==year & datas_fm$month==month][1],
            datas_fm$HighMinusLow[datas_fm$year==year & datas_fm$month==month][1], 
            datas_fm$MomentumFactor[datas_fm$year==year & datas_fm$month==month][1]);
  returnByPortRegression[nrow(returnByPortRegression) + 1, ] <- temp;
}

epsilon <- 0.05

#Simple Measures ie Jensen Alpha & Treynor
SimpleRegressionP1 <- lm(returnByPortRegression$`returnP1-Rf`~returnByPortRegression$`RM-Rf`)
SimpleSummaryP1<-summary(SimpleRegressionP1)
AlphaP1NotNull <- SimpleSummaryP1$coefficient[2,4]  <= (epsilon/2)

SimpleRegressionP10 <- lm(returnByPortRegression$`returnP10-Rf`~returnByPortRegression$`RM-Rf`)
SimpleSummaryP10<-summary(SimpleRegressionP10)
AlphaP10NotNull <- SimpleSummaryP10$coefficient[2,4]  <= (epsilon/2)

SimpleRegressionP10MinusP1 <- lm(returnByPortRegression$`returnP10MinusP1-Rf`~returnByPortRegression$`RM-Rf`)
SimpleSummaryP10MinusP1<-summary(SimpleRegressionP10MinusP1)
AlphaP10MinusP1NotNull <- SimpleSummaryP10MinusP1$coefficient[2,4]  <= (epsilon/2)

ColNamesMeasures <- c("PortfolioName",  "Beta", "Jensen Alpha","Significativ Alpha", "Treynor");
SimpleMeasures <- data.frame(matrix(nrow=0, ncol=5));
colnames(SimpleMeasures) <- ColNamesMeasures;

list_tmp <- c("P1",
              SimpleRegressionP1$coefficients[2],
              SimpleRegressionP1$coefficients[1],
              AlphaP1NotNull,
              mean(returnByPortRegression$`returnP1-Rf`)/SimpleRegressionP1$coefficients[2])

SimpleMeasures[nrow(SimpleMeasures) + 1, ] <- list_tmp;

list_tmp <- c("P10",
              SimpleRegressionP10$coefficients[2],
              SimpleRegressionP10$coefficients[1],
              AlphaP10NotNull,
              mean(returnByPortRegression$`returnP10-Rf`)/SimpleRegressionP10$coefficients[2])

SimpleMeasures[nrow(SimpleMeasures) + 1, ] <- list_tmp;

list_tmp <- c("P10MinusP1",
              SimpleRegressionP10MinusP1$coefficients[2],
              SimpleRegressionP10MinusP1$coefficients[1],
              AlphaP10MinusP1NotNull,
              mean(returnByPortRegression$`returnP10MinusP1-Rf`)/SimpleRegressionP10MinusP1$coefficients[2])

SimpleMeasures[nrow(SimpleMeasures) + 1, ] <- list_tmp;

list_tmp <- c("Market",
              1,
              0,
              TRUE,
              mean(returnByPortRegression$`RM-Rf`))

SimpleMeasures[nrow(SimpleMeasures) + 1, ] <- list_tmp;

#Complex Measures ie Alpha Carhart-Fama&French model.

ComplexRegressionP1 <- lm(returnByPortRegression$`returnP1-Rf`~returnByPortRegression$`RM-Rf`+returnByPortRegression$SMB+returnByPortRegression$HML+returnByPortRegression$MOM)
ComplexSummaryP1<-summary(ComplexRegressionP1)
ComplexAlphaP1NotNull <- ComplexSummaryP1$coefficient[2,4]  <= (epsilon/2)

ComplexRegressionP10 <- lm(returnByPortRegression$`returnP10-Rf`~returnByPortRegression$`RM-Rf`+returnByPortRegression$SMB+returnByPortRegression$HML+returnByPortRegression$MOM)
ComplexSummaryP10<-summary(ComplexRegressionP10)
ComplexAlphaP10NotNull <- ComplexSummaryP10$coefficient[2,4]  <= (epsilon/2)

ComplexRegressionP10MinusP1 <- lm(returnByPortRegression$`returnP10MinusP1-Rf`~returnByPortRegression$`RM-Rf`+returnByPortRegression$SMB+returnByPortRegression$HML+returnByPortRegression$MOM)
ComplexSummaryP10MinusP1<-summary(ComplexRegressionP10MinusP1)
ComplexAlphaP10MinusP1NotNull <- ComplexSummaryP10MinusP1$coefficient[2,4]  <= (epsilon/2)

ColNamesComplexMeasures <- c("PortfolioName", "Alpha Jensen", "Significativ Alpha Jensen", "Alpha Carhart", "Significativ Alpha Carhart", "Beta Market" ,"Beta SMB", "Beta HML", "Beta MOM");
ComplexMeasures <- data.frame(matrix(nrow=0, ncol=9));
colnames(ComplexMeasures) <- ColNamesComplexMeasures;

list_tmp <- c("P1",
              SimpleRegressionP1$coefficients[1],
              AlphaP1NotNull,
              ComplexRegressionP1$coefficients[1],
              ComplexAlphaP1NotNull,
              ComplexRegressionP1$coefficients[2],
              ComplexRegressionP1$coefficients[3],
              ComplexRegressionP1$coefficients[4],
              ComplexRegressionP1$coefficients[5])

ComplexMeasures[nrow(ComplexMeasures) + 1, ] <- list_tmp;

list_tmp <- c("P10",
              SimpleRegressionP10$coefficients[1],
              ComplexAlphaP10NotNull,
              ComplexRegressionP10$coefficients[1],
              ComplexAlphaP10NotNull,
              ComplexRegressionP10$coefficients[2],
              ComplexRegressionP10$coefficients[3],
              ComplexRegressionP10$coefficients[4],
              ComplexRegressionP10$coefficients[5])

ComplexMeasures[nrow(ComplexMeasures) + 1, ] <- list_tmp;

list_tmp <- c("P10MinusP1",
              SimpleRegressionP10MinusP1$coefficients[1],
              AlphaP10MinusP1NotNull,
              ComplexRegressionP10MinusP1$coefficients[1],
              ComplexAlphaP10MinusP1NotNull,
              ComplexRegressionP10MinusP1$coefficients[2],
              ComplexRegressionP10MinusP1$coefficients[3],
              ComplexRegressionP10MinusP1$coefficients[4],
              ComplexRegressionP10MinusP1$coefficients[5])

ComplexMeasures[nrow(ComplexMeasures) + 1, ] <- list_tmp;
