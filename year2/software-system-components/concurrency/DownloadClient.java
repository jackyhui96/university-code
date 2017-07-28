package sscDownloader;

import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.SwingWorker.StateValue;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import net.miginfocom.swing.MigLayout;

/**
 * Download client GUI to let user download files from a webpage
 * @author Jacky
 *
 */
public class DownloadClient {

	private JFrame frmDownloaderClient;
	private JTextField txtFieldUrl;
	private JTextField txtFieldPath;
	private JTextField txtFieldThread;
	private JComboBox<String> cmbExt;
	private JList<FileDownload> listFiles;
	private DefaultListModel<FileDownload> fileModel = new DefaultListModel<FileDownload>();;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					DownloadClient window = new DownloadClient();
					window.frmDownloaderClient.setVisible(true);
				}
				catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public DownloadClient() {
		initialise();
	}

	/**
	 * Initialise the contents of the frame.
	 */
	private void initialise() {
		// Frame settings
		frmDownloaderClient = new JFrame();
		frmDownloaderClient.setTitle("Downloader Client");
		frmDownloaderClient.setBounds(100, 100, 599, 382);
		frmDownloaderClient.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frmDownloaderClient.getContentPane().setLayout(new MigLayout("", "[110px][19px][137px,grow][6px][77px][6px][79px]", "[20px][23px][20px][20px][98px,grow][23px]"));
		
		// Text box to enter URL
		JLabel lblUrl = new JLabel("URL: ");
		txtFieldUrl = new JTextField();
		txtFieldUrl.setColumns(10);
		
		// Text box to enter path
		JLabel lblPath = new JLabel("Download to folder: ");
		txtFieldPath = new JTextField();
		txtFieldPath.setEnabled(false);
		txtFieldPath.setColumns(10);
		
		// Text box to specify number of threads
		JLabel lblThread = new JLabel("Number of threads:");		
		txtFieldThread = new JTextField();
		txtFieldThread.setColumns(10);
		
		// Combo box to select extension to filter files
		JLabel lblExtension = new JLabel("Extension:");		
		cmbExt = new JComboBox<String>();
		cmbExt.setModel(new DefaultComboBoxModel<String>(new String[] {".pdf", ".zip", ".png"}));
		
		// Listbox to contain file links parsed from web page
		listFiles = new JList<FileDownload>();
		
		// Button to enter the download path
		JButton btnPath = new JButton("Browse");
		btnPath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				actionPath();
			}
		});
		
		// Button to clear the path text box
		JButton btnClear = new JButton("Clear");
		btnClear.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				txtFieldPath.setText("");
			}
		});
		
		// Button to download files
		JButton btnDownload = new JButton("Download");
		btnDownload.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (!txtFieldUrl.getText().isEmpty() && !txtFieldPath.getText().isEmpty() && !txtFieldThread.getText().isEmpty()) {
					downloadFiles();
				}
				else {
					JOptionPane.showMessageDialog(null, "URL, Path or number of Threads is missing. Please fill these in.");
				}
			}
		});
		
		// Button to find files
		JButton btnFindFiles = new JButton("Find Files");
		btnFindFiles.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				
				// Check if url is empty
				if (!txtFieldUrl.getText().isEmpty()) {
					// Initialise the fileModel 
					fileModel.clear();
					
					Elements files = findFiles();
					for (Element file : files) {
						String urlstr = file.absUrl("href"); 
						String fileName = urlstr.substring( urlstr.lastIndexOf('/')+1, urlstr.length() );
						
						// Only include files that have the user selected extension
						if (urlstr.endsWith((String)cmbExt.getSelectedItem())) {
							fileModel.addElement(new FileDownload(fileName, urlstr, "Ready"));
						}
					}
					// Add the file model to the JList in the GUI
					listFiles.setModel(fileModel);
				}
				else {
					JOptionPane.showMessageDialog(null, "URL is empty. Please give a URL.");
				}
			}
		});
		
		// Add components to form
		frmDownloaderClient.getContentPane().add(lblUrl, "cell 0 0,alignx center,aligny center");
		frmDownloaderClient.getContentPane().add(txtFieldUrl, "cell 2 0 5 1,growx,aligny top");
		frmDownloaderClient.getContentPane().add(lblPath, "cell 0 1,alignx center,aligny center");
		frmDownloaderClient.getContentPane().add(txtFieldPath, "cell 2 1,growx,aligny center");
		frmDownloaderClient.getContentPane().add(lblThread, "cell 0 2,alignx center,aligny center");
		frmDownloaderClient.getContentPane().add(txtFieldThread, "cell 2 2,growx,aligny top");
		frmDownloaderClient.getContentPane().add(lblExtension, "cell 0 3,alignx center,aligny center");
		frmDownloaderClient.getContentPane().add(cmbExt, "cell 2 3,growx,aligny top");
		frmDownloaderClient.getContentPane().add(btnPath, "cell 4 1,growx,aligny top");
		frmDownloaderClient.getContentPane().add(btnClear, "cell 6 1,growx,aligny top");
		frmDownloaderClient.getContentPane().add(btnFindFiles, "cell 4 5,alignx left,aligny top");
		frmDownloaderClient.getContentPane().add(btnDownload, "cell 6 5,alignx left,aligny top");
		frmDownloaderClient.getContentPane().add(new JScrollPane(listFiles), "cell 0 4 7 1,grow");
	}
	
	
	/**
	 * Download the selected files
	 * @throws InterruptedException
	 * @throws ExecutionException
	 */
	private void downloadFiles() {
		String ext = (String)cmbExt.getSelectedItem();
		String folderPath = txtFieldPath.getText();
		int numThreads = Integer.valueOf(txtFieldThread.getText());
		DownloadSwingWorker runnableobj = null;
		// Create a thread pool with a number of threads
		ExecutorService pool = Executors.newFixedThreadPool(numThreads);
		// Add file links to a job queue
		List<FileDownload> jobqueue = listFiles.getSelectedValuesList();
		
		// For each job, create a SwingWorker and submit it to the Thread pool
		for (FileDownload job : jobqueue) {	
			if (job.getUrl().endsWith(ext)) {
				runnableobj = new DownloadSwingWorker(folderPath, job.getUrl());
				
				// Add a property listener to update the state of each job
				runnableobj.addPropertyChangeListener(new PropertyChangeListener() {
					public void propertyChange(PropertyChangeEvent evt) {		
						if ("state".equals(evt.getPropertyName())) {
							StateValue state = (StateValue) evt.getNewValue();
							String strState;
							if (state == StateValue.STARTED) {
								strState = "Downloading";
							}
							else if(state == StateValue.PENDING) {
								strState = "Queued";
							}
							else {
								strState = "Finished";
							}
							job.setStatus(strState);
						}
					}
				});
			}
			pool.submit(runnableobj);
		}
		pool.shutdown();
		JOptionPane.showMessageDialog(null, "File(s) downloaded.");
	}
	
	/**
	 * Parse the webpage and find all file links
	 * @return List of all files
	 */
	private Elements findFiles() {
		Document doc;
		String url = txtFieldUrl.getText();
		Elements files = null;
				
		try {			
			// Connect to the URL
			doc = Jsoup.connect(url).userAgent("Mozilla").get();
			// Parse all links in the webpage
			files = doc.select("a[href]");
		}
		catch(IOException e) {
			JOptionPane.showMessageDialog(null, "Couldn't connect to the URL");
		}
		return files;
	}
	
	/**
	 * Open a file chooser to choose a valid directory for path
	 */
	private void actionPath() {
    	final JFileChooser fc = new JFileChooser();
    	// Allow only directories to be selected
    	fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    	int returnVal = fc.showOpenDialog(frmDownloaderClient);
    	if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            txtFieldPath.setText(file.getPath());
    	}
    }

}
