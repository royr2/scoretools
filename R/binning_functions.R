library(Rcpp)

cppFunction('

NumericVector c_cut(NumericVector x, NumericVector cuts, double minx){

int n_x = x.size();
int n_cuts = cuts.size();

// cuts must be sorted
cuts = cuts.sort();

// Initialise the string vector holdng cut values
NumericVector groups(n_x);

// Loop through each cut value
for(int i = 0; i < n_x; i++){

  for(int j = 0; j < n_cuts; j++){

    if(x[i] >= minx && x[i] < cuts[j]){
    groups[i] = cuts[j];
    break;
    }

    groups[i] = 999;
  }

}

// Return
return(groups);

}')


