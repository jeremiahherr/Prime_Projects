import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;

// generate and then store a list of prime numbers
public class GeneratePrimes
{
	public static void main(String[] args)
	{
		// round doubles to four decimal places
		final DecimalFormat df = new DecimalFormat("0.0000");
		// generate all primes less than or equal to n
		int n = Integer.parseInt(args[0]);
		String text = args[1];
		System.out.println("Generating Primes");
		int[] primes = Primes.findPrimes(n);
		// count the number of primes found so far
		int nth = 1;
		// start with the 0th power of ten
		int order = 0;
		// get the largest order of magnitude in our set of numbers
		int maxOrder = (int) Math.floor(Math.log10(primes[primes.length-1]));
		double[] orderProps = new double[maxOrder+1];
		// find the number of primes between each power of ten
		// and the proportion of numbers that are prime between each power of ten
		for (int i = 0; i < primes.length; i++)
		{
			// check whether we have reached the next power of ten
			if (primes[i] > Math.pow(10,order+1) || i == primes.length-1)
			{
				// calculate what proportion of numbers between
				// the current power of ten and the last power of ten are prime
				orderProps[order] = nth/(Math.pow(10,order+1)-Math.pow(10,order));
				// start counting primes again from one
				nth = 1;
			}
			// get the current order of magnitude
			order = (int) Math.floor(Math.log10(primes[i]));
			// count the number of primes found since the last power of ten
			nth++;
		}
		try
		{
			// reset tracking variables
			order = 0;
			nth = 1;
			// prepare text file for storage
			File file = new File(text);
			FileWriter writer = new FileWriter(file);
			for (int i = 0; i < primes.length; i++)
			{
				if (primes[i] > Math.pow(10,order+1)) nth = 1;
				order = (int) Math.floor(Math.log10(primes[i]));
				// in our text file store the following data:
				// current prime number
				// the number of primes between this prime and the last power of ten 
				// current order of magnitude
				// proportion of prime numbers within current order of magnitude 
				writer.write(primes[i] + " " + nth + " " + order + " " + df.format(orderProps[order]) + "\n");
				nth++;
			}
			writer.flush();
			writer.close();
			System.out.println("Primes have been generated");
		}
		catch (IOException e)
		{
			System.out.println("Error");
		}
	}
}