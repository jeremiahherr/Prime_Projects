import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;

// double a check a list of primes to ensure each number is in fact prime
public class CheckPrimes
{
	public static void main(String[] args)
	{
		try
		{
			System.out.println("Checking input numbers to verify primality.");
			File file = new File(args[0]);
			Scanner sc = new Scanner(file);
			while (sc.hasNextLine())
			{
				String[] tokens = (sc.nextLine()).split(" ");
				int prime = Integer.parseInt(tokens[0]);
				boolean primality = Primes.isPrime(prime);
				if (primality == false) System.out.println("False positive detected: " + prime + " is not prime.");
			}
			System.out.println("Every input number has been checked.");
			sc.close();
		}
		catch(FileNotFoundException e)
		{
			System.out.println("Error: File not found");
		}
	}
}