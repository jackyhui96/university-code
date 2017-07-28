package javaMail;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.mail.*;
import javax.mail.internet.*;
import javax.swing.*;
import javax.swing.event.*;
 
/**
 * A class representing the email client
 * @author Jacky
 *
 */
public class EmailClient extends JFrame {
     
    // Message table's data model.
    private MessagesTableModel tableModel;
     
    // Table listing messages.
    private JTable table;
     
    // This the text area for displaying messages.
    private JTextArea messageTextArea;
     
    // This is the split panel that holds the messages
    // table and the message view panel.
    private JSplitPane splitPane;
     
    // These are the buttons for managing the selected message.
    private JButton replyButton, forwardButton, deleteButton, seenButton;
     
    // Currently selected message in table.
    private Message selectedMessage;
     
    // Flag for whether or not a message is being deleted.
    private boolean deleting;
     
    // This is the JavaMail session.
    private Session session; 
     
    /**
     * Construct a new email client
     */
    public EmailClient() {
        // Set application title.
        setTitle("E-mail Client");
         
        // Set window size.
        setSize(640, 480);
         
        // Handle window closing events.
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                actionExit();
            }
        });
         
        // Setup file menu.
        JMenuBar menuBar = new JMenuBar();
        JMenu fileMenu = new JMenu("File");
        fileMenu.setMnemonic(KeyEvent.VK_F);
        JMenuItem fileExitMenuItem = new JMenuItem("Exit",
                KeyEvent.VK_X);
        fileExitMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                actionExit();
            }
        });
        fileMenu.add(fileExitMenuItem);
        menuBar.add(fileMenu);
        setJMenuBar(menuBar);
         
        // Setup buttons panel.
        JPanel buttonPanel = new JPanel();
        JButton newButton = new JButton("New Message");
        newButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                actionNew();
            }
        });
        buttonPanel.add(newButton);
         
        // Setup messages table.
        tableModel = new MessagesTableModel();
        table = new JTable(tableModel);
        table.getSelectionModel().addListSelectionListener(new
                ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                tableSelectionChanged();
            }
        });
        // Allow only one row at a time to be selected.
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
         
        // Setup E-mails panel.
        JPanel emailsPanel = new JPanel();
        emailsPanel.setBorder(
                BorderFactory.createTitledBorder("E-mails"));
        messageTextArea = new JTextArea();
        messageTextArea.setEditable(false);
        splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
                new JScrollPane(table), new JScrollPane(messageTextArea));
        emailsPanel.setLayout(new BorderLayout());
        emailsPanel.add(splitPane, BorderLayout.CENTER);
         
        // Setup buttons panel 2.
        JPanel buttonPanel2 = new JPanel();
        replyButton = new JButton("Reply");
        replyButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                actionReply();
            }
        });
        replyButton.setEnabled(false);
        buttonPanel2.add(replyButton);
        
        forwardButton = new JButton("Forward");
        forwardButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                actionForward();
            }
        });
        forwardButton.setEnabled(false);
        buttonPanel2.add(forwardButton);
        
        deleteButton = new JButton("Delete");
        deleteButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                actionDelete();
            }
        });
        deleteButton.setEnabled(false);
        buttonPanel2.add(deleteButton);
        
        seenButton = new JButton("Read/Unread");
        seenButton.setEnabled(false);
        seenButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                actionSeen();
            }
        });
        buttonPanel2.add(seenButton);
         
        // Add panels to display.
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(buttonPanel, BorderLayout.NORTH);
        getContentPane().add(emailsPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel2, BorderLayout.SOUTH);
    }
     
    /**
     * Close the program
     */
    private void actionExit() {
        System.exit(0);
    }
     
    /**
     * Open a new dialog for a new message
     */
    private void actionNew() {
        sendMessage(MessageDialog.NEW, null);
    }
     
    /**
     * Show message content when selected
     */
    private void tableSelectionChanged() {
    /* If not in the middle of deleting a message, set
       the selected message and display it. */
        if (!deleting) {
            selectedMessage = tableModel.getMessage(table.getSelectedRow());
            showSelectedMessage();
            updateButtons();
        }
    }
    
    /**
     * Set the seen flags for a selected message
     */
    private void actionSeen() {
    	boolean seen;
    	try {
    		seen = selectedMessage.getFlags().contains(Flags.Flag.SEEN);
			selectedMessage.setFlag(Flags.Flag.SEEN, !seen);
			Folder folder = selectedMessage.getFolder();
	        folder.close(true);
	        folder.open(Folder.READ_WRITE);
		}
		catch (MessagingException e) {
			showError("Unable to set read/unread flag.", false);
		}
    }
    
    /**
     * Open a dialog to reply to a selected message
     */
    private void actionReply() {
        sendMessage(MessageDialog.REPLY, selectedMessage);
    }
     
    /**
     * Open a dialog to forward a selected message
     */
    private void actionForward() {
        sendMessage(MessageDialog.FORWARD, selectedMessage);
    }
     
    /**
     * Delete a selected a email
     */
    private void actionDelete() {
        deleting = true;
         
        try {
            // Delete message from server.
            selectedMessage.setFlag(Flags.Flag.DELETED, true);
            Folder folder = selectedMessage.getFolder();
            folder.close(true);
            folder.open(Folder.READ_WRITE);
        } 
        catch (Exception e) {
            showError("Unable to delete message.", false);
        }
         
        // Delete message from table.
        tableModel.deleteMessage(table.getSelectedRow());
         
        // Update GUI.
        messageTextArea.setText("");
        deleting = false;
        selectedMessage = null;
        updateButtons();
    }
     
    /**
     * Send the message
     * @param type The type of message
     * @param message The current message
     */
    private void sendMessage(int type, Message message) {
        
        MessageDialog dialog;
		String smtphost = "smtp.gmail.com";
		Properties props = System.getProperties();
        String username = props.getProperty("mail.user");
		String password = props.getProperty("mail.password");
        
		// Display message dialog to get message values.
        try {
            dialog = new MessageDialog(this, type, message);
            if (!dialog.display()) {
                // Return if dialog was cancelled.
                return;
            }
            
            // Create a new message with values from dialog.
            Message newMessage = new MimeMessage(session);
            newMessage.setFrom(new InternetAddress(username));
            newMessage.setRecipients(Message.RecipientType.TO, InternetAddress.parse(dialog.getTo()));
            if (!dialog.getCC().isEmpty()) {
            	newMessage.setRecipients(Message.RecipientType.CC, InternetAddress.parse(dialog.getCC()));
            }
            newMessage.setSubject(dialog.getSubject());
            newMessage.setSentDate(new Date());
            
            // If there is an attachment create a Multipart message
            if (dialog.getAttach().isEmpty()) {
	            newMessage.setText(dialog.getContent());
            }
            else {
            	Multipart multipart = new MimeMultipart();
            	
            	MimeBodyPart messageBodyPart = new MimeBodyPart();
            	messageBodyPart.setContent(dialog.getContent(), "text/html");
            	
            	MimeBodyPart attachPart = new MimeBodyPart();
            	attachPart.attachFile(dialog.getAttach());
            	
            	multipart.addBodyPart(messageBodyPart);
            	multipart.addBodyPart(attachPart);
            	
            	newMessage.setContent(multipart);
            }
            
            newMessage.saveChanges();
            
            // Send new message.
            Transport tr = session.getTransport("smtp");
            tr.connect(smtphost, username, password);
            tr.sendMessage(newMessage, newMessage.getAllRecipients());
        } 	
        catch (Exception e) {
            showError("Unable to send message.", false);
        }
    }
     
    /**
     * Show the selected message in the text area
     */
    private void showSelectedMessage() {
        
    	try {
            messageTextArea.setText(getMessageContent(selectedMessage));
            messageTextArea.setCaretPosition(0);
        } 
    	catch (Exception e) {
            showError("Unabled to load message.", false);
    	}
    }
     
  /* Update each button's state based off of whether or not
     there is a message currently selected in the table. */
    private void updateButtons() {
        if (selectedMessage != null) {
            replyButton.setEnabled(true);
            forwardButton.setEnabled(true);
            deleteButton.setEnabled(true);
            seenButton.setEnabled(true);
            ;
        } else {
            replyButton.setEnabled(false);
            forwardButton.setEnabled(false);
            deleteButton.setEnabled(false);
            seenButton.setEnabled(false);
        }
    }
     
    // Show the application window on the screen.
    public void show() {
        super.show();
         
        // Update the split panel to be divided 50/50.
        splitPane.setDividerLocation(.5);
    }
     
    /**
     * Connect to the email sevrer using IMAPS
     */
    public void connect() {
         
        // Establish JavaMail session and connect to server.
        Store store = null;
        String username = "huitesting@gmail.com";
		String password = "";
		
        try {
            
        	// Set mail user properties using Properties object
    		Properties props = System.getProperties();
    		props.setProperty("mail.store.protocol", "imaps");
    		// Get user password using JPasswordField 
    		JPasswordField pwd = new JPasswordField(10);
    		int action = JOptionPane.showConfirmDialog(null, pwd, "Enter Password",
    				JOptionPane.OK_CANCEL_OPTION);
    		if (action < 0) {
    			JOptionPane.showMessageDialog(null,
    					"Cancel, X or escape key selected");
    			System.exit(0);
    		}
    		else
    			password = new String(pwd.getPassword());

    		// Set Property with username and password for authentication  
    		props.setProperty("mail.user", username);
    		props.setProperty("mail.password", password);

    		// Set system properties to allow smtp
    		props.put("mail.smtp.auth", "true");
    		props.put("mail.smtp.starttls.enable", "true");
    		props.put("mail.smtp.host", "smtp.gmail.com");
    		props.put("mail.smtp.port", "587");
    		
    		//Establish a mail session (java.mail.Session)
    		session = Session.getDefaultInstance(props);
             
            // Connect to e-mail server.
    		store = session.getStore("imaps");
			store.connect("imap.googlemail.com", username, password);
			
        } catch (Exception e) {
             
            // Show error dialog.
            showError("Unable to connect.", true);
        }
         
        // Download message headers from server.
        try {
            // Open main "INBOX" folder.
            Folder folder = store.getFolder("INBOX");
            folder.open(Folder.READ_WRITE);
             
            // Get folder's list of messages.
            Message[] messages = folder.getMessages();
             
            // Retrieve message headers for each message in folder.
            FetchProfile profile = new FetchProfile();
            profile.add(FetchProfile.Item.ENVELOPE);
            folder.fetch(messages, profile);
             
            // Put messages in table.
            tableModel.setMessages(messages);
        } catch (Exception e) {
             
            // Show error dialog.
            showError("Unable to download messages.", true);
        }
    }
     
    /**
     * Show error message
     * @param message The message in the error
     * @param exit Flag to terminate program
     */
    private void showError(String message, boolean exit) {
        JOptionPane.showMessageDialog(this, message, "Error",
                JOptionPane.ERROR_MESSAGE);
        if (exit)
            System.exit(0);
    }
     
    /**
     * Retrieves the message content from the given message
     * @param message The given message
     * @return The message as a string
     * @throws Exception
     */
    public static String getMessageContent(Message message) throws Exception {
        Object content = message.getContent();
        if (content instanceof Multipart) {
            StringBuffer messageContent = new StringBuffer();
            Multipart multipart = (Multipart) content;
            for (int i = 0; i < multipart.getCount(); i++) {
                Part part = (Part) multipart.getBodyPart(i);
                if (part.isMimeType("text/plain")) {
                    messageContent.append(part.getContent().toString());
                }
            }
            return messageContent.toString();
        } else {
            return content.toString();
        }
    }
     
    // Run the E-mail Client.
    public static void main(String[] args) {
        EmailClient client = new EmailClient();
        client.show();
         
        // Display connect dialog.
        client.connect();
    }
}

