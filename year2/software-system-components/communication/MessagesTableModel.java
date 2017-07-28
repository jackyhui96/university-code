package javaMail;

import java.util.*;
import javax.mail.*;
import javax.swing.table.*;


/**
 * Table model to store message
 * @author Jacky
 *
 */
public class MessagesTableModel extends AbstractTableModel {
	
	// These are the names for the table's columns.
	private static final String[] columnNames = {"Sender", "Subject", "Date", "Seen"};
	
	// The table's list of messages.
    private ArrayList<Message> messageList = new ArrayList<Message>();
    
    /**
     * Set the messages in the table
     * @param messages The messages to insert into the table 
     */
    public void setMessages(Message[] messages) {
        for (int i = messages.length - 1; i >= 0; i--) {
            messageList.add(messages[i]);
        }
         
        // Fire table data change notification to table.
        fireTableDataChanged();
    }
    
    /**
     * Get the message at the given row
     * @param row The row
     * @return The message
     */
    public Message getMessage(int row) {
        return (Message) messageList.get(row);
    }
    
    /**
     * Delete the message at the given row
     * @param row The row
     */
    public void deleteMessage(int row) {
        messageList.remove(row);
         
        // Fire table row deletion notification to table.
        fireTableRowsDeleted(row, row);
    }
    
    /**
     * Get the column name
     * @return The column name
     */
    public String getColumnName(int col) {
        return columnNames[col];
    }

	@Override
	 // Get table's row count.
	public int getRowCount() {
		return messageList.size();
	}

	@Override
	// Get table's column count.
	public int getColumnCount() {
		return columnNames.length;
	}

	/**
	 * Get the value at a specific row and column combination
	 */
    public Object getValueAt(int row, int col) {
        try {
        	Message message = (Message) messageList.get(row);
            switch (col) {
                case 0: // Sender
                    Address[] senders = message.getFrom();
                    if (senders != null || senders.length > 0) {
                        return senders[0].toString();
                    } else {
                        return "[none]";
                    }
                case 1: // Subject
                    String subject = message.getSubject();
                    if (subject != null && subject.length() > 0) {
                        return subject;
                    } else {
                        return "[none]";
                    }
                case 2: // Date
                    Date date = message.getSentDate();
                    if (date != null) {
                        return date.toString();
                    } else {
                        return "[none]";
                    }
                case 3: // Seen flag
                    boolean seen = message.getFlags().contains(Flags.Flag.SEEN);
                    if (seen == true) {
                        return "Read";
                    } else {
                        return "Unread";
                    }
            }
        } catch (Exception e) {
            // Fail silently.
            return "";
        }   
        return "";
    }
}
