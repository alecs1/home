// Generate a list of prime numbers using a vector

#include <vector>
#include <cstdio>
#include <cmath>

int main(int argc, char* argv[]) {

  int last;
  sscanf(argv[1], "%d", &last);

  std::vector<int> primes;

  primes.push_back(2);

  int crt = 3;

  while (crt <= last) {
    bool prime = true;
    int i = 0;
    while ( prime && (i < primes.size()) && (primes[i] * primes[i] <= crt) ) {
      if ( (crt % primes[i]) == 0)
        prime = false;
      i++;
    }
    if (prime)
      primes.push_back(crt);
    crt += 2;
  }

  printf("[");
  for (int i = 0; i < primes.size() - 1; i++)
    printf("%d,", primes[i]);
  printf("%d]", primes.back());

  printf("\n");


}
