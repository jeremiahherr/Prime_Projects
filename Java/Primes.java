import java.util.Arrays;

public class Primes
{
	// find every prime number from 2 to n
	public static int[] findPrimes(int n)
	{
		// there will be no more than n primes between 2 and n
		int[] primes = new int[n];
		// hard code in the first three primes
		primes[0] = 2;
		primes[1] = 3;
		primes[2] = 5;
		// keep track of the next empty spot in our array of primes
		int index = 3;

		// generate numbers and test their primality
		for (int i = 1; ((6*i)+1) <= n; i++)
		{
			// every prime greater than 5 is of the from (6*i)+1 or (6*i)+5, i >= 1
			int omod6 = (6*i) + 1;
			int fmod6 = (6*i) + 5;

			// assume each number we generate is prime
			// then, we'll attempt to prove that it is not prime
			boolean isPrime1 = true;
			boolean isPrime5 = true;

			// any factor of a number will be less than or equal to that number's square root
			int max = (int) Math.sqrt(fmod6);

			// for each number we generate, look through the primes we have already found
			// and check whether any of them are common factors of our generated numbers
			for (int j = 0; primes[j] <= max; j++)
			{
				int prime = primes[j];
				if (prime == 0) break;
				else
				{
					// if either of our numbers is divible by one of our primes,
					// then it is not prime
					if (omod6%prime == 0) isPrime1 = false;
					if (fmod6%prime == 0) isPrime5 = false;
				}
			}

			// both, one, or none of our generated numbers to our array of primes
			if (isPrime1 == true && isPrime5 == true)
			{
				primes[index] = omod6;
				primes[index+1] = fmod6;
				index += 2;
			}
			else if (isPrime1 == true && isPrime5 == false) 
			{
				primes[index] = omod6;
				index++;
			}
			else if (isPrime1 == false && isPrime5 == true)
			{
				primes[index] = fmod6;
				index++;
			} 
		}
		int[] prms = null;
		// our array of primes has unused space
		for (int i = 0; i < primes.length; i++)
		{
			if (primes[i] == 0)
			{
				prms = new int[i];
				for (int j = 0; j < prms.length; j++) prms[j] = primes[j];
				break;
			}
		}
		// return our array of true primes
		return prms;
	}

	// used to double check whether an input number is actually prime
	public static boolean isPrime(int prime)
	{
		// 1 is not prime
		if (prime == 1) return false;
		// 2 is prime
		if (prime == 2) return true;
		// any even number greater than 2 is not prime
		if (prime%2 == 0) return false;
		// any factor of a number will be less than or equal to that number's square root
		int max = (int) Math.sqrt(prime);
		// check whether our number has any factors greater than or equal to 3
		for (int i = 3; i <= max; i += 2)
		{
			if (prime%i == 0) return false;
		}
		// if we have not found any factors, assume our number is prime
		return true;
	}

	// test client for findPrimes method
	public static void main(String[] args)
	{
		int n = Integer.parseInt(args[0]);
		int[] primes = findPrimes(n);
		System.out.println(Arrays.toString(primes));
	}
}