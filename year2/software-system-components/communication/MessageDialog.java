package javaMail;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import javax.mail.Address;
import javax.mail.Message;
import javax.swing.*;
 
/**
 * The message dialog for composing a message
 * @author Jacky
 *
 */
public class MessageDialog extends JDialog {
     
    // Dialog message identifiers.
    public static final int NEW = 0;
    public static final int REPLY = 1;
    public static final int FORWARD = 2;
     
    // Message from, to and subject text fields.
    private JTextField toTextField;
    private JTextField subjectTextField;
     
    // Message content text area.
    private JTextArea contentTextArea;
     
    // Flag specifying whether or not dialog was cancelled.
    private boolean cancelled;
    private JTextField ccTextField;
    private JTextField attachTextField;
     
    /**
     * Construct a new dialog to create messages
     * @param parent The parent frame from which it was called
     * @param type The type of message
     * @param message The current message
     * @throws Exception
     */
    public MessageDialog(Frame parent, int type, Message message)
    throws Exception {
        // Call super constructor, specifying that dialog is modal.
        super(parent, true);
         
    /* Set dialog title and get message's "to", "subject"
       and "content" values based on message type. */
        String to = "", subject = "", content = "";
        switch (type) {
            // Reply message.
            case REPLY:
                setTitle("Reply To Message");
                 
                // Get message "to" value
                Address[] senders = message.getFrom();
                if (senders != null || senders.length > 0) {
                    to = senders[0].toString();
                }
                to = message.getFrom()[0].toString();
                 
                // Get message subject.
                subject = message.getSubject();
                if (subject != null && subject.length() > 0) {
                    subject = "RE: " + subject;
                } else {
                    subject = "RE:";
                }
                 
                // Get message content and add "REPLIED TO" notation.
                content = "\n----------------- " +
                        "REPLIED TO MESSAGE" +
                        " -----------------\n" +
                        EmailClient.getMessageContent(message);
                break;
                 
                // Forward message.
            case FORWARD:
                setTitle("Forward Message");
                 
                // Get message subject.
                subject = message.getSubject();
                if (subject != null && subject.length() > 0) {
                    subject = "FWD: " + subject;
                } else {
                    subject = "FWD:";
                }
                 
                // Get message content and add "FORWARDED" notation.
                content = "\n----------------- " +
                        "FORWARDED MESSAGE" +
                        " -----------------\n" +
                        EmailClient.getMessageContent(message);
                break;
                 
                // New message.
            default:
                setTitle("New Message");
        }
         
        // Handle closing events.
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                actionCancel();
            }
        });
         
        // Setup fields panel.
        JPanel fieldsPanel = new JPanel();
        GridBagConstraints constraints;
        GridBagLayout layout = new GridBagLayout();
        layout.rowHeights = new int[]{0, 0, 0, 0};
        layout.columnWidths = new int[]{76, 45, 0};
        layout.columnWeights = new double[]{0.0, 1.0, 0.0};
        fieldsPanel.setLayout(layout);
        constraints = new GridBagConstraints();
        constraints.anchor = GridBagConstraints.EAST;
        constraints.insets = new Insets(5, 5, 0, 0);
        constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridwidth = GridBagConstraints.REMAINDER;
        constraints.insets = new Insets(5, 5, 0, 0);
        constraints = new GridBagConstraints();
        constraints.anchor = GridBagConstraints.EAST;
        constraints.insets = new Insets(5, 5, 0, 0);
        constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridwidth = GridBagConstraints.REMAINDER;
        constraints.insets = new Insets(5, 5, 0, 0);
        constraints.weightx = 1.0D;
        constraints = new GridBagConstraints();
        constraints.insets = new Insets(5, 5, 5, 0);
        constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridwidth = GridBagConstraints.REMAINDER;
        constraints.insets = new Insets(5, 5, 5, 0);
         
        // Setup content panel.
        JScrollPane contentPanel = new JScrollPane();
        contentTextArea = new JTextArea(content, 10, 50);
        contentPanel.setViewportView(contentTextArea);
         
        // Setup buttons panel.
        JPanel buttonsPanel = new JPanel();
        JButton sendButton = new JButton("Send");
        sendButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                actionSend();
            }
        });
        buttonsPanel.add(sendButton);
        JButton cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                actionCancel();
            }
        });
        buttonsPanel.add(cancelButton);
        JButton attachButton = new JButton("Attach a file");
        attachButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                actionAttach();
            }
        });
        buttonsPanel.add(attachButton);
         
        // Add panels to display.
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(fieldsPanel, BorderLayout.NORTH);
        
        JLabel toLabel = new JLabel("To:");
        layout.setConstraints(toLabel, constraints);
        GridBagConstraints gbc_toLabel = new GridBagConstraints();
        gbc_toLabel.insets = new Insets(0, 0, 5, 5);
        gbc_toLabel.gridx = 0;
        gbc_toLabel.gridy = 0;
        fieldsPanel.add(toLabel, gbc_toLabel);
        toTextField = new JTextField(to);
        layout.setConstraints(toTextField, constraints);
        GridBagConstraints gbc_toTextField = new GridBagConstraints();
        gbc_toTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_toTextField.insets = new Insets(0, 0, 5, 5);
        gbc_toTextField.gridx = 1;
        gbc_toTextField.gridy = 0;
        fieldsPanel.add(toTextField, gbc_toTextField);
        
        JLabel cclabel = new JLabel("CC:");
        GridBagConstraints gbc_cclabel = new GridBagConstraints();
        gbc_cclabel.insets = new Insets(0, 0, 5, 5);
        gbc_cclabel.gridx = 0;
        gbc_cclabel.gridy = 1;
        fieldsPanel.add(cclabel, gbc_cclabel);
        ccTextField = new JTextField();
        GridBagConstraints gbc_ccTextField = new GridBagConstraints();
        gbc_ccTextField.insets = new Insets(0, 0, 5, 5);
        gbc_ccTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_ccTextField.gridx = 1;
        gbc_ccTextField.gridy = 1;
        fieldsPanel.add(ccTextField, gbc_ccTextField);
        
        JLabel subjectLabel = new JLabel("Subject:");
        layout.setConstraints(subjectLabel, constraints);
        GridBagConstraints gbc_subjectLabel = new GridBagConstraints();
        gbc_subjectLabel.insets = new Insets(0, 0, 5, 5);
        gbc_subjectLabel.gridx = 0;
        gbc_subjectLabel.gridy = 2;
        fieldsPanel.add(subjectLabel, gbc_subjectLabel);
        subjectTextField = new JTextField(subject);
        layout.setConstraints(subjectTextField, constraints);
        GridBagConstraints gbc_subjectTextField = new GridBagConstraints();
        gbc_subjectTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_subjectTextField.insets = new Insets(0, 0, 5, 5);
        gbc_subjectTextField.gridx = 1;
        gbc_subjectTextField.gridy = 2;
        fieldsPanel.add(subjectTextField, gbc_subjectTextField);
        
        JLabel attachLabel = new JLabel("Attachment:");
        GridBagConstraints gbc_attachLabel = new GridBagConstraints();
        gbc_attachLabel.anchor = GridBagConstraints.EAST;
        gbc_attachLabel.insets = new Insets(0, 0, 0, 5);
        gbc_attachLabel.gridx = 0;
        gbc_attachLabel.gridy = 3;
        fieldsPanel.add(attachLabel, gbc_attachLabel);
        attachTextField = new JTextField();
        attachTextField.setEnabled(false);
        GridBagConstraints gbc_attachTextField = new GridBagConstraints();
        gbc_attachTextField.insets = new Insets(0, 0, 0, 5);
        gbc_attachTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_attachTextField.gridx = 1;
        gbc_attachTextField.gridy = 3;
        fieldsPanel.add(attachTextField, gbc_attachTextField);
        attachTextField.setColumns(10);
        
        // Clear button to clear attachment
        JButton clearButton = new JButton("Clear");
        GridBagConstraints gbc_clearButton = new GridBagConstraints();
        gbc_clearButton.gridx = 2;
        gbc_clearButton.gridy = 3;
        fieldsPanel.add(clearButton, gbc_clearButton);
        clearButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	attachTextField.setText("");
            }
        });
        
        getContentPane().add(contentPanel, BorderLayout.CENTER);
        getContentPane().add(buttonsPanel, BorderLayout.SOUTH);
         
        // Size dialog to components.
        pack();
         
        // Center dialog over application.
        setLocationRelativeTo(parent);
    }
     
    /**
     * Validate the fields and send the message
     */
    private void actionSend() {
        if (toTextField.getText().trim().length() < 1
                || subjectTextField.getText().trim().length() < 1
                || contentTextArea.getText().trim().length() < 1) {
            JOptionPane.showMessageDialog(this,
                    "One or more fields is missing.",
                    "Missing Field(s)", JOptionPane.ERROR_MESSAGE);
            return;
        }
         
        // Close dialog.
        dispose();
    }
    
    /**
     * Add an attachment to the message
     */
    private void actionAttach() {
    	final JFileChooser fc = new JFileChooser();
    	int returnVal = fc.showOpenDialog(this);
    	if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            attachTextField.setText(file.getPath());
    	}
    }
    
    /**
     * Close the dialog if user cancels
     */
    private void actionCancel() {
        cancelled = true;
         
        // Close dialog.
        dispose();
    }
     
    /**
     * Display the dialog
     * @return Flag to show if display was succesful
     */
    @SuppressWarnings("deprecation")
	public boolean display() {
        show();
         
        // Return whether or not display was successful.
        return !cancelled;
    }
     
    /**
     * Get the text from the 'To' field
     * @return The text in the field
     */
    public String getTo() {
        return toTextField.getText();
    }
    
    /**
     * Get the text from the 'CC' field
     * @return The text in the field
     */
    public String getCC() {
        return ccTextField.getText();
    }
     
    /**
     * Get the text from the 'Subject' field
     * @return The text in the field
     */
    public String getSubject() {
        return subjectTextField.getText();
    }
     
    /**
     * Get the text from the 'Content' area
     * @return The text in the area
     */
    public String getContent() {
        return contentTextArea.getText();
    }
    
    /**
    * Get the text from the 'Attach' field
    * @return The text in the field
    */
    public String getAttach() {
        return attachTextField.getText();
    }
}

