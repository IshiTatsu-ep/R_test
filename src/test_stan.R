library(inline)
library(Rcpp)
src <- 'std::vector<std::string> s; 
s.push_back("hello");
s.push_back("world");
return Rcpp::wrap(s);
'
hellofun <- cxxfunction(body = src, includes = '', plugin = 'Rcpp', verbose = FALSE)
cat(hellofun(), '\n') 
