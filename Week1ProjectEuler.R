#Find the sum of all the multiples of 3 or 5 below 1000
Euler1 = function(maxN=1000) {
  result = 0;
  for (number in 1:(maxN-1)) {
    if (((number %% 5) == 0) | ((number %% 3) == 0)) {
      result = result + number;
    }
  }
  return(result);
}

Euler1A = function(maxN=1000) {
  #List numbers from 1 - 1000
  numbers = seq(from=0, by =1, length=maxN);
  goodNumbers = ((numbers %% 5) == 0) | ((numbers %% 3) == 0);
  return(sum(numbers[goodNumbers]));
}

Euler1B = function(maxN=1000) {
  upperBound = maxN-1;
  return(5*sum(seq(0,upperBound/5)) + 3*sum(seq(0,upperBound/3)) - 3*5*sum(seq(0,upperBound/(5*3))));
}

seqSum = function(nStart=1, nEnd) {
  return( (nEnd - nStart + 1)*(nStart + nEnd)/2 );
}

Euler1C = function(maxN=1000) {
  upperBound = maxN-1;
  return(5*seqSum(1,upperBound %/% 5) + 3*seqSum(1,upperBound %/% 3) - 3*5*seqSum(1,upperBound %/% (3*5)));
}

Euler2 = function(maxVal = 4000000) {
  #Return the sum of all even-valued fibonacci numbers whose values are less than maxVal
  
}

findFactors = function(number) {
  return(which(number %% (1:sqrt(number)) == 0));
}

Euler3 = function(number = 600851475143) {
  #Return the largest prime factor of the number
  factors = findFactors(number);
  primeFactors = numeric();
  factors = factors[!(factors == 1)]
  #Find the factors of factors and keep removing them until we are only left with prime factors
  while(length(factors) > 1) {
    newFactors = findFactors(factors[1]);
    if (length(newFactors) == 1) {
      #Then we have a prime factor
      primeFactors = c(primeFactors, factors[1]);
    }
    #Remove the examined factor
    factors = factors[-1];    
    factors = unique(c(factors, newFactors ));
    factors = factors[!(factors == 1)];
  }
  return(max(primeFactors));
}

Euler4 = function(digits = 3) {
  #Return the largest palindrome made from the product of two "digits"-digit numbers
  
}

Euler5 = function(maxN = 20) {
  #Return the smallest positive number that is evenly divisible by all of the numbers from 1 to maxN
  require(gmp)
	counts = numeric(maxN); 
	for (i in 1:maxN) { 
		facs = rle(as.numeric(factorize(i))); 
		counts[facs$values] = ifelse(counts[facs$values] > facs$lengths, counts[facs$values], facs$length) 
	}
	return(prod((1:maxN)**counts))
}

is.divisor = function(a,number) {
  #Returns true if a is a divisor of number
  return(number %% a == 0);
}

is.prime = function(number, primeList) {
  #return(is.null(Find (function (a) { is.divisor(a,number)}, primeList)));
  return(Find (function (a) { is.divisor(a,number)}, primeList));
}

isPrime = function(number, primeList, sqrtPos) {
  return(all( (number %% primeList[1:sqrtPos]) != 0 ));
}

Euler10 = function(maxVal = 2000000) {
  #Return the sum of all primes less than maxVal
  #Seed with a primeList of {2,3}
  primeList = c(2,3);
  sqrtPos = 1;
  for (i in seq(5,maxVal,2)) {
    if ( primeList[sqrtPos+1] <= floor(sqrt(i)) ) {
      sqrtPos = sqrtPos+1;
    }
    
    if (is.prime(i, primeList[1:sqrtPos])) {
      primeList = c(primeList,i);
    }
  }
  
  #print(primeList);
  #print(sqrtPos);
  return(sum(primeList));
}
