package sscDownloader;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import javax.swing.SwingWorker;

/**
 * Extended SwingWorker thread to download file
 * @author Jacky
 *
 */
public class DownloadSwingWorker extends SwingWorker<FileDownload, FileDownload> {
	
	private String path;
	private String fileurl;
	private String fileName;
		
	/**
	 * Construct a new download swing worker
	 * @param path The path to save the file
	 * @param fileurl The url of the file
	 */
	public DownloadSwingWorker(String path, String fileurl) {
		this.path = path;
		this.fileurl = fileurl;
	}

	@Override
	/**
	 * Download the file in the background
	 */
	protected FileDownload doInBackground() throws Exception {
		try {			
			fileName = fileurl.substring( fileurl.lastIndexOf('/')+1, fileurl.length() );

			//Open a URL Stream
			URL url = new URL(fileurl);
			InputStream in = url.openStream();
			OutputStream out = new BufferedOutputStream(new FileOutputStream(path + "/" + fileName));
			
			for (int b; (b = in.read()) != -1;) {
				out.write(b);
			}
			
			out.close();
			in.close();
	} catch (IOException e) {
		e.printStackTrace();
	}
		return new FileDownload(fileName, fileurl, "Completed");
	}
	
	/**
	 * Message to confirm process is done
	 */
	protected void done() {
		if (isCancelled())
			System.out.println("Cancelled !");
		else
			System.out.println(fileName + "Done !");
	}
}
