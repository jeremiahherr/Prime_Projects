import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;

public class GenerateNums2
{
	public static void main(String[] args)
	{
		// round doubles to four decimal places
		final DecimalFormat df = new DecimalFormat("0.0000");
		// generate all primes less than or equal to n
		int n = Integer.parseInt(args[0]);
		String text = args[1];
		System.out.println("Generating Numbers");
		int[] nums = new int[n];
		for (int i = 1; i <= n; i++) nums[i-1] = i;
		// count the number of primes found so far
		int nth = 0;
		// start with the 0th power of ten
		int order = 0;
		// get the largest order of magnitude in our set of numbers
		int maxOrder = (int) Math.floor(Math.log10(nums[n-1]));
		//System.out.println(maxOrder);
		double[] orderProps = new double[maxOrder+1];
		// find the number of primes between each power of ten
		// and the proportion of numbers that are prime between each power of ten
		for (int i = 0; i < n; i++)
		{
			// check whether we have reached the next power of ten
			if (nums[i] > Math.pow(10,order+1) || i == n-1)
			{
				// calculate what proportion of numbers between
				// the current power of ten and the last power of ten are prime
				orderProps[order] = nth/(Math.pow(10,order+1)-Math.pow(10,order));
				// get the current order of magnitude
				order = (int) Math.floor(Math.log10(nums[i]));
				// start counting primes again from zero
				nth = 0;
			}
			if (Primes.isPrime(nums[i]))
			{
				// count the number of primes found since the last power of ten
				nth++;
			}
		}
		try
		{
			// reset tracking variables
			order = 0;
			nth = 0;
			// will record whether each number is prime
			String prime = "";
			// will record the distance between current number and last prime
			int lastPrime = 2;
			// prepare text file for storage
			File file = new File(text);
			FileWriter writer = new FileWriter(file);
			for (int i = 0; i < n; i++)
			{
				if (nums[i] == Math.pow(10,order+1)) nth = 0;
				order = (int) Math.floor(Math.log10(nums[i]));
				if (Primes.isPrime(nums[i])) 
				{
					nth++;
					prime = "Y";
				}
				else prime = "N";
				// in our text file store the following data:
				// current number
				// number of primes between this number and the last power of ten
				// current order of magnitude
				// proportion of prime numbers within current order of magnitude 
				// distance between this number and the last prime
				// current number mod 6
				// is current number even or odd? (0 means num is even, 1 means num is odd)
				// is current number prime?
				writer.write(nums[i] + " " + nth + " " + order + " " + df.format(orderProps[order]) 
					+ " " + (nums[i]-lastPrime) + " " + nums[i]%6 + " " + nums[i]%2 + " " + prime + "\n");
				if (Primes.isPrime(nums[i])) lastPrime = nums[i];
			}
			writer.flush();
			writer.close();
			System.out.println("Numbers have been generated");
		}
		catch (IOException e)
		{
			System.out.println("Error");
		}
	}
}