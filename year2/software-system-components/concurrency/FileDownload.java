package sscDownloader;

/**
 * Custom class to store file download variables
 * @author Jacky
 *
 */
public class FileDownload {
	
	private String filename;
	private String url;
	private String status;;
	
	/**
	 * Construct a new file download
	 * @param filename The file name
	 * @param url The file url
	 * @param status The status of the file download
	 */
	public FileDownload(String filename, String url, String status) {
		this.filename = filename;
		this.url = url;
		this.status = status;
	}
	
	/**
	 * Get the file name
	 * @return The file name
	 */
	public String getFilename() {
		return filename;
	}
	
	/**
	 * Get the file url
	 * @return The file url
	 */
	public String getUrl() {
		return url;
	}
	
	/**
	 * Get the file download status
	 * @return The file download status
	 */
	public String getStatus() {
		return status;
	}
	
	/**
	 * Set the file name
	 * @param name The new file name
	 */
	public void setFilename(String name) {
		this.filename = name;
	}
	
	/**
	 * Set the file url
	 * @param url The new file url
	 */
	public void setUrl(String url) {
		this.url = url;
	}
	
	/**
	 * Set the file download status
	 * @param status The new file status
	 */
	public void setStatus(String status) {
		this.status = status;
	}
	
	/**
	 * Print the file download name and status
	 */
	public String toString() {
		return filename + " - Status: " + status;
	}
}
