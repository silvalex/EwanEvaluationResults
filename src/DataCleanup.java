import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

public class DataCleanup {
	public static void main(String[] args) throws IOException {
		List<RunResult> resultList = new ArrayList<RunResult>();
		String[] jackyFileNames = {"2008-dataset01.stat", "2008-dataset02.stat", "2008-dataset04.stat"};
		String[] ewanFileNames = {"ewan01.stat", "ewan02.stat", "ewan04.stat"};
		String[] alexFileNames = {"2008-graph-evol1/out%s.stat", "2008-graph-evol2/out%s.stat", "2008-graph-evol4/out%s.stat"};
		String[] datasetNumbers = {"1", "2", "4"};

		Scanner scan;

		// Read in Jacky's result, storing each run as an object
		for (int i = 0; i < jackyFileNames.length; i++) {
			int runNo = 1;
			scan = new Scanner(new File(jackyFileNames[i]));
			while (scan.hasNextLine()) {
				RunResult result = new RunResult();
				// Read execution time
				result.executionTime = scan.next();
				// Read QoS attributes
				result.availability = scan.next();
				result.reliability  = scan.next();
				result.time         = scan.next();
				result.cost         = scan.next();
				// Throw away representation line
				scan.nextLine();
				scan.nextLine();
				// Add run number
				result.runNo = runNo++;
				// Add dataset info
				result.dataset = datasetNumbers[i];
				// Add approach name
				result.approachName = "jacky";
				resultList.add(result);
			}
			scan.close();
		}

		// Read in Jacky's result, storing each run as an object
		for (int i = 0; i < ewanFileNames.length; i++) {
			scan = new Scanner(new File(ewanFileNames[i]));
			while (scan.hasNextLine()) {
				RunResult result = new RunResult();
				// Read execution time
				result.runNo = scan.nextInt();
				// Throw away fitness data
				scan.next();
				result.executionTime = scan.next();
				// Read QoS attributes
				result.availability = scan.next();
				result.reliability  = scan.next();
				result.cost         = scan.next();
				result.time         = scan.next();

				// Add dataset info
				result.dataset = datasetNumbers[i];
				// Add approach name
				result.approachName = "ewan";
				resultList.add(result);
			}
			scan.close();
		}

		// Read in Alex's result, storing each run as an object
		for (int i = 0; i < alexFileNames.length; i++) {
			for (int runNo = 1; runNo < 31; runNo++) {
				scan = new Scanner(new File(String.format(alexFileNames[i], runNo)));
				String availability = null;
				String reliability = null;
				String time = null;
				String cost = null;
				double totalExecutionTime = 0.0;

				for (int j = 0; j < 51; j++) {
					// Throw away gen number
					scan.next();
					// Get initialisation time
					totalExecutionTime += scan.nextDouble();
					// Get execution time
					totalExecutionTime += scan.nextDouble();
					// Throw the next 11 tokens away
					for (int x = 0; x < 11; x++) {
						scan.next();
					}
					// Get the latest QoS
					availability = scan.next();
					reliability = scan.next();
					time = scan.next();
					cost = scan.next();
				}

				RunResult result = new RunResult();

				result.executionTime = Double.toString(totalExecutionTime);
				result.availability = availability;
				result.reliability  = reliability;
				result.time         = time;
				result.cost         = cost;
				result.dataset = datasetNumbers[i];
				result.approachName = "alex";
				result.runNo = runNo;
				resultList.add(result);

				scan.close();
			}
		}

		// Save all results as a file
		FileWriter writer = new FileWriter(new File("combinedResults.stat"));
		for (RunResult r : resultList) {
			writer.append(r.toString());
			writer.append("\n");
		}
		writer.close();

		//Runtime.getRuntime().exec("need R");
		//Runtime.getRuntime().exec("./multi_comparison_2008.R");

		System.out.println("Done!");
	}
}
