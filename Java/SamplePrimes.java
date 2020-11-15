import java.util.Scanner;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedList;
import java.util.ArrayList;

// from a list of prime numbers, generate a random sample of primes
public class SamplePrimes
{
	public static void main(String[] args)
	{
		try
		{
			// get sample size, n
			int n = Integer.parseInt(args[0]);
			// get text file name of a list of primes
			File file1 = new File(args[1]);
			// get name of file to be generated
			String sample = args[2];

			// extract data from file
			Scanner sc = new Scanner(file1);
			LinkedList<String> dataList = new LinkedList<String>();
			System.out.println("Extract prime data from input file");
			while (sc.hasNextLine())
			{
				String row = sc.nextLine();
				dataList.add(row);
			}
			String[] data = new String[dataList.size()];
			data = dataList.toArray(data);

			// select random data indices
			int random;
			int uniqueNums = 0;
			int max = data.length; 
			ArrayList<Integer> randList = new ArrayList<Integer>();
			// randomly generate n unique numbers
			System.out.println("Select random sample set from data");
			while (uniqueNums < n)
			{
				//System.out.println(uniqueNums);
				// generate a random number between 0 (inclusive) and max (exclusive)
				random = (int) (Math.random()*max);
				//System.out.println(random + " " + uniqueNums);
				if (!randList.contains(random))
				{
					randList.add(random);
					uniqueNums++;
				}
			}
			Integer[] rands = new Integer[randList.size()];
			rands = randList.toArray(rands);

			// store list of randomly selected primes in a text file
			File file2 = new File(sample);
			FileWriter writer = new FileWriter(file2);
			for (int i = 0; i < n; i++) writer.write(data[rands[i]] + "\n");
			writer.flush();
			writer.close();
			System.out.println("Sample set of primes has been generated");
		}
		catch (IOException e)
		{
			System.out.println("Error: IOException");
		}
	}
}