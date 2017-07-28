import java.util.*;
import java.io.*;
import java.sql.*;

public class setupDB
{
	public static void main(String[] args)
	{
		PreparedStatement pstmt;
		Statement stmt;
		ResultSet rs;
		Connection conn = null;
		
		try {
			Class.forName("org.postgresql.Driver");
		} 
		catch (ClassNotFoundException ex) 
		{
			System.out.println("Driver not found");
		}
		
		System.out.println("PostgreSQL driver registered.");

		try 
		{
			// Get a database connection
			conn = DriverManager.getConnection("jdbc:postgresql://dbteach2/jxh592", "jxh592", "rebomosl");
			
			// Get a Statement object
	   		stmt = conn.createStatement();
			
			// Delete all tables in database
			pstmt = conn.prepareStatement("DROP TABLE IF EXISTS "
							   + "Titles,"
							   + "Student,"
							   + "StudentContact,"
							   + "NextOfKinContact,"
							   + "Lecturer,"
							   + "LecturerContact,"							
							   + "Tutor,"
							   + "RegistrationType,"
							   + "StudentRegistration");
			pstmt.executeUpdate();
			pstmt.close();
		
			// Create all the tables for the database
			// Create table for titles
	   		pstmt = conn.prepareStatement("CREATE TABLE Titles("
							   + "titleID INTEGER NOT NULL,"
							   + "titleString CHAR(20) NOT NULL,"
							   + "PRIMARY KEY (titleID))");
			pstmt.executeUpdate();
			pstmt.close();

			// Create table for students
	   		pstmt = conn.prepareStatement("CREATE TABLE Student("
							   + "studentID INTEGER NOT NULL,"
							   + "titleID INTEGER NOT NULL,"
							   + "foreName CHAR(20) NOT NULL,"
							   + "familyName CHAR(20) NOT NULL,"
							   + "dateOfBirth DATE NOT NULL,"
							   + "PRIMARY KEY (studentID),"
							   	+ "FOREIGN KEY (titleID) REFERENCES Titles(titleID))");
			pstmt.executeUpdate();
			pstmt.close();
			
			// Create table for student contacts
	   		pstmt = conn.prepareStatement("CREATE TABLE StudentContact("
							   + "studentID INTEGER NOT NULL,"
							   + "eMailAddress CHAR(40),"
							   + "postalAddress CHAR(60),"
							   + "FOREIGN KEY (studentID) REFERENCES Student(studentID)"
							   	+ " ON DELETE CASCADE ON UPDATE CASCADE)");
			pstmt.executeUpdate();
			pstmt.close();
			
			// Create table for next of kin contacts
	   		pstmt = conn.prepareStatement("CREATE TABLE NextOfKinContact("
							   + "studentID INTEGER NOT NULL,"
							   + "name CHAR(20),"
							   + "eMailAddress CHAR(40),"
							   + "postalAddress CHAR(60),"
							   + "FOREIGN KEY (studentID) REFERENCES Student(studentID)"
							   	+ " ON DELETE CASCADE ON UPDATE CASCADE)");
			pstmt.executeUpdate();
			pstmt.close();			
			
			// Create table for lecturers
	   		pstmt = conn.prepareStatement("CREATE TABLE Lecturer("
							   + "lecturerID INTEGER NOT NULL,"
							   + "titleID INTEGER NOT NULL,"
							   + "foreName CHAR(20) NOT NULL,"
							   + "familyName CHAR(20) NOT NULL,"
							   + "PRIMARY KEY (lecturerID),"
							   + "FOREIGN KEY (titleID) REFERENCES Titles(titleID))");
			pstmt.executeUpdate();
			pstmt.close();

			// Create table for lecturer contacts
	   		pstmt = conn.prepareStatement("CREATE TABLE LecturerContact("
							   + "lecturerID INTEGER NOT NULL,"
							   + "Office INTEGER,"
							   + "eMailAddress CHAR(40),"
							   + "FOREIGN KEY (lecturerID) REFERENCES Lecturer(lecturerID)"
							   	+ " ON DELETE CASCADE ON UPDATE CASCADE)");
			pstmt.executeUpdate();
			pstmt.close();			
			
			// Create table for tutors
	   		pstmt = conn.prepareStatement("CREATE TABLE Tutor("
							   + "studentID INTEGER NOT NULL,"
							   + "lecturerID INTEGER NOT NULL,"
							   + "FOREIGN KEY (studentID) REFERENCES Student(studentID) "
							   	+ "ON DELETE CASCADE ON UPDATE CASCADE, "
							   + "FOREIGN KEY (lecturerID) REFERENCES Lecturer(lecturerID) "
							   	+ "ON DELETE CASCADE ON UPDATE CASCADE)");
			pstmt.executeUpdate();
			pstmt.close();	
			
			// Create table for registration types
	   		pstmt = conn.prepareStatement("CREATE TABLE RegistrationType("
							   + "registrationTypeID INTEGER NOT NULL,"
							   + "description CHAR(60) NOT NULL,"
							   + "PRIMARY KEY (registrationTypeID))");
			pstmt.executeUpdate();
			pstmt.close();	
			
			// Create table for student registrations
	   		pstmt = conn.prepareStatement("CREATE TABLE StudentRegistration("
							   + "studentID INTEGER NOT NULL,"
							   + "yearOfStudy INTEGER NOT NULL,"
							   + "registrationTypeID INTEGER NOT NULL,"
							   + "FOREIGN KEY (registrationTypeID) REFERENCES RegistrationType(registrationTypeID) "
							   	+ "ON DELETE CASCADE ON UPDATE CASCADE, "
							   + "FOREIGN KEY (studentID) REFERENCES Student(studentID) "
							   	+ "ON DELETE CASCADE ON UPDATE CASCADE, "
							   + "PRIMARY KEY (studentID, registrationTypeID))");
			pstmt.executeUpdate();
			pstmt.close();
			
			// Insert records for titles using a PreparedStatement
			pstmt = conn.prepareStatement("INSERT INTO Titles " +
						 "(titleID, titleString) " +
						 "VALUES (?, ?)") ;
			pstmt.clearParameters();
			pstmt.setString(1, "1");
			pstmt.setString(2, "Mr");
			pstmt.executeUpdate();

			pstmt.clearParameters();			
			pstmt.setString(1, "2");
			pstmt.setString(2, "Miss");
			pstmt.executeUpdate();
		
			pstmt.clearParameters();
			pstmt.setString(1, "3");
			pstmt.setString(2, "Mrs");
			pstmt.executeUpdate();

			pstmt.clearParameters();
			pstmt.setString(1, "4");
			pstmt.setString(2, "Ms");
			pstmt.executeUpdate();
			
			pstmt.clearParameters();
			pstmt.setString(1, "5");
			pstmt.setString(2, "Dr");
			pstmt.executeUpdate();
			
			// Show the results
			System.out.println("Titles");
	    		rs = stmt.executeQuery("SELECT * FROM Titles");
	    		while (rs.next()) {
				System.out.print(rs.getInt(1));
				System.out.print(": ");
				System.out.print(rs.getString(2));
				System.out.println("");				
			}
	    		rs.close();
			System.out.println("");
			
			// Insert records for titles using a PreparedStatement
			pstmt = conn.prepareStatement("INSERT INTO Lecturer " +
						 "(lecturerID, titleID, foreName, familyName) " +
						 "VALUES (?, ?, ?, ?)") ;
			pstmt.clearParameters();
			pstmt.setString(1, "100");
			pstmt.setString(2, "5");
			pstmt.setString(3, "Mark");
			pstmt.setString(4, "Lee");
			pstmt.executeUpdate();

			pstmt.clearParameters();			
			pstmt.setString(1, "101");
			pstmt.setString(2, "5");
			pstmt.setString(3, "Volker");
			pstmt.setString(4, "Sorge");
			pstmt.executeUpdate();
		
			pstmt.clearParameters();
			pstmt.setString(1, "102");
			pstmt.setString(2, "1");
			pstmt.setString(3, "Bob");
			pstmt.setString(4, "Hendley");
			pstmt.executeUpdate();

			pstmt.clearParameters();
			pstmt.setString(1, "103");
			pstmt.setString(2, "1");
			pstmt.setString(3, "Jon");
			pstmt.setString(4, "Rowe");
			pstmt.executeUpdate();
			
			pstmt.clearParameters();
			pstmt.setString(1, "104");
			pstmt.setString(2, "1");
			pstmt.setString(3, "Martin");
			pstmt.setString(4, "Escardo");
			pstmt.executeUpdate();
			
			// Show the results
			System.out.println("Lecturers");
	    		rs = stmt.executeQuery("SELECT * FROM Lecturer");
	    		while (rs.next()) 
			{
				System.out.print(rs.getInt(1));
				System.out.print(": ");
				System.out.print(rs.getString(2));
				System.out.print(": ");
				System.out.print(rs.getString(3));
				System.out.print(": ");
				System.out.print(rs.getString(4));
				System.out.println("");
			}
	    		rs.close();	
			System.out.println("");
			
			// Insert synthetic records for students using a PreparedStatement
			pstmt = conn.prepareStatement("INSERT INTO Student " +
						 "(studentID, titleID, foreName, familyName, dateOfBirth) " +
						 "VALUES (?, ?, ?, ?, ?)") ;
			int idCounter = 200;
			int nameCounter = 1;
			int recordCounter = 0;

			while (recordCounter < 100)
			{
				pstmt.clearParameters();
				pstmt.setString(1, Integer.toString(idCounter));
				pstmt.setString(2, "1");
				pstmt.setString(3, "FirstName" + Integer.toString(nameCounter));
				pstmt.setString(4, "LastName" + Integer.toString(nameCounter));
				pstmt.setString(5, "2000-11-11");
				pstmt.executeUpdate();
				idCounter++;
				nameCounter++;
				recordCounter++;
			}
			
			// Insert some actual data for students
			pstmt.clearParameters();
			pstmt.setString(1, Integer.toString(idCounter));
			pstmt.setString(2, "1");
			pstmt.setString(3, "John");
			pstmt.setString(4, "Smith");
			pstmt.setString(5, "1996-12-01");
			pstmt.executeUpdate();
			idCounter++;
			
			pstmt.clearParameters();
			pstmt.setString(1, Integer.toString(idCounter));
			pstmt.setString(2, "2");
			pstmt.setString(3, "Olivia");
			pstmt.setString(4, "Birks");
			pstmt.setString(5, "1996-08-20");
			pstmt.executeUpdate();
			idCounter++;
			
			pstmt.clearParameters();
			pstmt.setString(1, Integer.toString(idCounter));
			pstmt.setString(2, "1");
			pstmt.setString(3, "Jacky");
			pstmt.setString(4, "Hui");
			pstmt.setString(5, "1996-04-26");
			pstmt.executeUpdate();
			idCounter++;
			
			// Show the results
			System.out.println("Students");
	    		rs = stmt.executeQuery("SELECT COUNT(*) FROM Student");
	    		while (rs.next()) 
			{
				System.out.print(rs.getInt(1));
				System.out.println("");
			}
	    		rs.close();	
			System.out.println("");
			
			// Insert records for student contacts using a PreparedStatement
			pstmt = conn.prepareStatement("INSERT INTO StudentContact " +
						 "(studentID, eMailAddress, postalAddress) " +
						 "VALUES (?, ?, ?)") ;
			pstmt.clearParameters();
			pstmt.setString(1, "300");
			pstmt.setString(2, "js101@studentemail.com");
			pstmt.setString(3, "10 Oxford Street");
			pstmt.executeUpdate();

			pstmt.clearParameters();
			pstmt.setString(1, "301");
			pstmt.setString(2, "ob102@studentemail.com");
			pstmt.setString(3, "18 Victoria Road");
			pstmt.executeUpdate();
		
			pstmt.clearParameters();
			pstmt.setString(1, "302");
			pstmt.setString(2, "jh103@studentemail.com");
			pstmt.setString(3, "83 High Street");
			pstmt.executeUpdate();
			
			// Show the results
			System.out.println("Student Contacts");
	    		rs = stmt.executeQuery("SELECT * FROM StudentContact");
	    		while (rs.next()) {
				System.out.print(rs.getInt(1));
				System.out.print(": ");
				System.out.print(rs.getString(2));
				System.out.print(": ");
				System.out.print(rs.getString(3));
				System.out.println("");				
			}
	    		rs.close();
			System.out.println("");
			
			// Insert records for next of kin contacts using a PreparedStatement
			pstmt = conn.prepareStatement("INSERT INTO NextOfKinContact " +
						 "(studentID, name, eMailAddress, postalAddress) " +
						 "VALUES (?, ?, ?, ?)") ;
			pstmt.clearParameters();
			pstmt.setString(1, "300");
			pstmt.setString(2, "James");
			pstmt.setString(3, "james@email.com");
			pstmt.setString(4, "5 Baker Street");
			pstmt.executeUpdate();

			pstmt.clearParameters();
			pstmt.setString(1, "301");
			pstmt.setString(2, "Deleine");
			pstmt.setString(3, "deliene@email.com");
			pstmt.setString(4, "7 Friars Road");
			pstmt.executeUpdate();

			pstmt.clearParameters();
			pstmt.setString(1, "302");
			pstmt.setString(2, "Jenny");
			pstmt.setString(3, "jenny@email.com");
			pstmt.setString(4, "88 Pandora Street");
			pstmt.executeUpdate();

			
			// Show the results
			System.out.println("Next Of Kin Contacts");
	    		rs = stmt.executeQuery("SELECT * FROM NextOfKinContact");
	    		while (rs.next()) {
				System.out.print(rs.getInt(1));
				System.out.print(": ");
				System.out.print(rs.getString(2));
				System.out.print(": ");
				System.out.print(rs.getString(3));
				System.out.print(": ");
				System.out.print(rs.getString(4));
				System.out.println("");				
			}
	    		rs.close();
			System.out.println("");
			
			// Insert records for registration type using a PreparedStatement
			pstmt = conn.prepareStatement("INSERT INTO RegistrationType " +
						 "(registrationTypeID, description) " +
						 "VALUES (?, ?)") ;
			pstmt.clearParameters();
			pstmt.setString(1, "1");
			pstmt.setString(2, "Normal");
			pstmt.executeUpdate();

			pstmt.clearParameters();
			pstmt.setString(1, "2");
			pstmt.setString(2, "Repeat");
			pstmt.executeUpdate();
		
			pstmt.clearParameters();
			pstmt.setString(1, "3");
			pstmt.setString(2, "External");
			pstmt.executeUpdate();
			
			// Show the results
			System.out.println("Registration Types");
	    		rs = stmt.executeQuery("SELECT * FROM RegistrationType");
	    		while (rs.next()) 
			{
				System.out.print(rs.getInt(1));
				System.out.print(": ");
				System.out.print(rs.getString(2));
				System.out.println("");
			}
	    		rs.close();	
			System.out.println("");
			
			// Insert records for next of kin contacts using a PreparedStatement
			pstmt = conn.prepareStatement("INSERT INTO NextOfKinContact " +
						 "(studentID, name, eMailAddress, postalAddress) " +
						 "VALUES (?, ?, ?, ?)") ;
			pstmt.clearParameters();
			pstmt.setString(1, "300");
			pstmt.setString(2, "James");
			pstmt.setString(3, "james@email.com");
			pstmt.setString(4, "5 Baker Street");
			pstmt.executeUpdate();

			pstmt.clearParameters();
			pstmt.setString(1, "301");
			pstmt.setString(2, "Deleine");
			pstmt.setString(3, "deliene@email.com");
			pstmt.setString(4, "7 Friars Road");
			pstmt.executeUpdate();

			pstmt.clearParameters();
			pstmt.setString(1, "302");
			pstmt.setString(2, "Jenny");
			pstmt.setString(3, "jenny@email.com");
			pstmt.setString(4, "88 Pandora Street");
			pstmt.executeUpdate();

			
			// Show the results
			System.out.println("Next Of Kin Contacts");
	    		rs = stmt.executeQuery("SELECT * FROM NextOfKinContact");
	    		while (rs.next()) {
				System.out.print(rs.getInt(1));
				System.out.print(": ");
				System.out.print(rs.getString(2));
				System.out.print(": ");
				System.out.print(rs.getString(3));
				System.out.print(": ");
				System.out.print(rs.getString(4));
				System.out.println("");				
			}
	    		rs.close();
			System.out.println("");
			
			// Insert records for lecturer contacts using a PreparedStatement
			pstmt = conn.prepareStatement("INSERT INTO LecturerContact " +
						 "(LecturerID, Office, eMailAddress) " +
						 "VALUES (?, ?, ?)") ;
			pstmt.clearParameters();
			pstmt.setString(1, "100");
			pstmt.setString(2, "200");
			pstmt.setString(3, "ml@email.com");
			pstmt.executeUpdate();

			pstmt.clearParameters();
			pstmt.setString(1, "101");
			pstmt.setString(2, "201");
			pstmt.setString(3, "vs@email.com");
			pstmt.executeUpdate();
		
			pstmt.clearParameters();
			pstmt.setString(1, "102");
			pstmt.setString(2, "202");
			pstmt.setString(3, "bh@email.com");
			pstmt.executeUpdate();
			
			// Show the results
			System.out.println("Lecturer Contacts");
	    		rs = stmt.executeQuery("SELECT * FROM LecturerContact");
	    		while (rs.next()) 
			{
				System.out.print(rs.getInt(1));
				System.out.print(": ");
				System.out.print(rs.getString(2));
				System.out.print(": ");
				System.out.print(rs.getString(3));
				System.out.println("");
			}
	    		rs.close();	
			System.out.println("");
			
		} 
		catch (SQLException ex) 
		{
			ex.printStackTrace();
		}
		if (conn != null) 
		{
			System.out.println("Database accessed");
			System.out.println("Database initialised");
		} 
		else 
		{
			System.out.println("Failed to make connection");
		}		
	}
}
