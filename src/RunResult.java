
public class RunResult {
	public String approachName;
	public int runNo;
	public String dataset;
	public String executionTime;
	public String availability;
	public String reliability;
	public String time;
	public String cost;

	public String toString() {
		return String.format("%s %d %s %s %s %s %s %s", approachName, runNo, dataset, executionTime, availability, reliability, time, cost);
	}
}
