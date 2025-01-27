       IDENTIFICATION DIVISION.
       PROGRAM-ID. EmployeeManagement.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employees.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.
           SELECT TEMP-FILE ASSIGN TO "temp.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS TEMP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMPLOYEE-ID       PIC 9(5).
           05 EMPLOYEE-NAME     PIC X(30).
           05 EMPLOYEE-AGE      PIC 9(2).

       FD TEMP-FILE.
       01 TEMP-RECORD.
           05 TEMP-ID          PIC 9(5).
           05 TEMP-NAME        PIC X(30).
           05 TEMP-AGE         PIC 9(2).

       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
           05 USER-CHOICE       PIC 9.
           05 SEARCH-ID         PIC 9(5).
           05 FOUND-FLAG        PIC X VALUE "N".
           05 END-OF-FILE       PIC X VALUE "N".
           05 FILE-STATUS       PIC XX.
           05 TEMP-STATUS       PIC XX.
           05 WS-ERROR-MSG      PIC X(50).
           05 CONTINUE-FLAG     PIC X.
           05 CONFIRM-DELETE    PIC X.
           05 WS-COPY-STATUS    PIC S9(9) USAGE BINARY.
           05 WS-DELETE-STATUS  PIC S9(9) USAGE BINARY.
           05 WS-OLD-FILENAME   PIC X(255) VALUE "temp.dat".
           05 WS-NEW-FILENAME   PIC X(255) VALUE "employees.dat".

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM MAIN-MENU
           STOP RUN.

       MAIN-MENU.
           PERFORM UNTIL USER-CHOICE = 6
               PERFORM CLEAR-SCREEN
               DISPLAY "-------------------------------------------"
               DISPLAY "       Employee Management System"
               DISPLAY "-------------------------------------------"
               DISPLAY "1. Add Employee"
               DISPLAY "2. View Employees"
               DISPLAY "3. Search Employee by ID"
               DISPLAY "4. Edit Employee"
               DISPLAY "5. Delete Employee"
               DISPLAY "6. Exit"
               DISPLAY "-------------------------------------------"
               DISPLAY "Enter your choice: "
               ACCEPT USER-CHOICE
               
               EVALUATE USER-CHOICE
                   WHEN 1
                       PERFORM ADD-EMPLOYEE
                   WHEN 2
                       PERFORM VIEW-EMPLOYEES
                   WHEN 3
                       PERFORM SEARCH-EMPLOYEE
                   WHEN 4
                       PERFORM EDIT-EMPLOYEE
                   WHEN 5
                       PERFORM DELETE-EMPLOYEE
                   WHEN 6
                       DISPLAY "Exiting the system. Goodbye!"
                   WHEN OTHER
                       DISPLAY "Invalid choice. Please try again."
                       PERFORM PRESS-ENTER
               END-EVALUATE
           END-PERFORM.

       ADD-EMPLOYEE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Employee ID (5 digits): "
           ACCEPT EMPLOYEE-ID
           IF EMPLOYEE-ID IS NOT NUMERIC OR EMPLOYEE-ID = ZEROS
               DISPLAY "Invalid ID format. Must be 5 digits."
               PERFORM PRESS-ENTER
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Enter Employee Name: "
           ACCEPT EMPLOYEE-NAME
           IF EMPLOYEE-NAME = SPACES
               DISPLAY "Name cannot be empty."
               PERFORM PRESS-ENTER
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Enter Employee Age: "
           ACCEPT EMPLOYEE-AGE
           IF EMPLOYEE-AGE IS NOT NUMERIC OR 
              EMPLOYEE-AGE < 18 OR EMPLOYEE-AGE > 99
               DISPLAY "Invalid age. Must be between 18 and 99."
               PERFORM PRESS-ENTER
               EXIT PARAGRAPH
           END-IF.

           OPEN EXTEND EMPLOYEE-FILE
           IF FILE-STATUS NOT = "00"
               OPEN OUTPUT EMPLOYEE-FILE
           END-IF.
           
           IF FILE-STATUS = "00"
               WRITE EMPLOYEE-RECORD
               IF FILE-STATUS = "00"
                   DISPLAY "Employee record added successfully!"
               ELSE
                   MOVE "Error writing record. Status: " TO WS-ERROR-MSG
                   MOVE FILE-STATUS TO WS-ERROR-MSG(27:2)
                   DISPLAY WS-ERROR-MSG
               END-IF
           ELSE
               MOVE "Error opening file. Status: " TO WS-ERROR-MSG
               MOVE FILE-STATUS TO WS-ERROR-MSG(25:2)
               DISPLAY WS-ERROR-MSG
           END-IF.
           
           CLOSE EMPLOYEE-FILE
           PERFORM PRESS-ENTER.

       VIEW-EMPLOYEES.
           PERFORM CLEAR-SCREEN
           MOVE "N" TO END-OF-FILE
           
           OPEN INPUT EMPLOYEE-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file. No records exist."
               PERFORM PRESS-ENTER
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "-------------------------------------------"
           DISPLAY "ID      Name                           Age"
           DISPLAY "-------------------------------------------"
           
           PERFORM UNTIL END-OF-FILE = "Y"
               READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       DISPLAY EMPLOYEE-ID SPACE 
                               EMPLOYEE-NAME SPACE 
                               EMPLOYEE-AGE
               END-READ
           END-PERFORM.
           
           CLOSE EMPLOYEE-FILE
           DISPLAY "-------------------------------------------"
           PERFORM PRESS-ENTER.

       SEARCH-EMPLOYEE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Employee ID to search: "
           ACCEPT SEARCH-ID
           
           IF SEARCH-ID IS NOT NUMERIC OR SEARCH-ID = ZEROS
               DISPLAY "Invalid ID format. Must be 5 digits."
               PERFORM PRESS-ENTER
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT EMPLOYEE-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file. No records exist."
               PERFORM PRESS-ENTER
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO FOUND-FLAG
           MOVE "N" TO END-OF-FILE
           
           PERFORM UNTIL FOUND-FLAG = "Y" OR END-OF-FILE = "Y"
               READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       IF EMPLOYEE-ID = SEARCH-ID
                           DISPLAY "Employee Found!"
                           DISPLAY "ID: " EMPLOYEE-ID
                           DISPLAY "Name: " EMPLOYEE-NAME
                           DISPLAY "Age: " EMPLOYEE-AGE
                           MOVE "Y" TO FOUND-FLAG
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE EMPLOYEE-FILE
           
           IF FOUND-FLAG = "N"
               DISPLAY "Employee not found!"
           END-IF.
           
           PERFORM PRESS-ENTER.

       EDIT-EMPLOYEE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Employee ID to edit: "
           ACCEPT SEARCH-ID
           
           IF SEARCH-ID IS NOT NUMERIC OR SEARCH-ID = ZEROS
               DISPLAY "Invalid ID format. Must be 5 digits."
               PERFORM PRESS-ENTER
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT TEMP-FILE
           
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file. No records exist."
               PERFORM PRESS-ENTER
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO FOUND-FLAG
           MOVE "N" TO END-OF-FILE
           
           PERFORM UNTIL END-OF-FILE = "Y"
               READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       IF EMPLOYEE-ID = SEARCH-ID
                           MOVE "Y" TO FOUND-FLAG
                           DISPLAY "Current Details:"
                           DISPLAY "Name: " EMPLOYEE-NAME
                           DISPLAY "Age: " EMPLOYEE-AGE
                           DISPLAY "Enter new details:"
                           
                           DISPLAY "Enter new Name: "
                           ACCEPT TEMP-NAME
                           IF TEMP-NAME = SPACES
                               MOVE EMPLOYEE-NAME TO TEMP-NAME
                           END-IF
                           
                           DISPLAY "Enter new Age: "
                           ACCEPT TEMP-AGE
                           IF TEMP-AGE = SPACES
                               MOVE EMPLOYEE-AGE TO TEMP-AGE
                           END-IF
                           
                           MOVE SEARCH-ID TO TEMP-ID
                           WRITE TEMP-RECORD
                       ELSE
                           MOVE EMPLOYEE-RECORD TO TEMP-RECORD
                           WRITE TEMP-RECORD
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE EMPLOYEE-FILE
           CLOSE TEMP-FILE
           
                      IF FOUND-FLAG = "N"
               DISPLAY "Employee not found!"
           ELSE
               CLOSE EMPLOYEE-FILE
               CLOSE TEMP-FILE
               CALL "CBL_DELETE_FILE" USING WS-NEW-FILENAME
                   RETURNING WS-DELETE-STATUS
               CALL "CBL_COPY_FILE" USING 
                   WS-OLD-FILENAME 
                   WS-NEW-FILENAME
                   RETURNING WS-COPY-STATUS
               IF WS-COPY-STATUS = 0
                   CALL "CBL_DELETE_FILE" USING WS-OLD-FILENAME
                   DISPLAY "Employee record updated successfully!"
               ELSE
                   DISPLAY "Error updating record!"
               END-IF
           END-IF.
           
           PERFORM PRESS-ENTER.

       DELETE-EMPLOYEE.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Employee ID to delete: "
           ACCEPT SEARCH-ID
           
           IF SEARCH-ID IS NOT NUMERIC OR SEARCH-ID = ZEROS
               DISPLAY "Invalid ID format. Must be 5 digits."
               PERFORM PRESS-ENTER
               EXIT PARAGRAPH
           END-IF.

           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT TEMP-FILE
           
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file. No records exist."
               PERFORM PRESS-ENTER
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO FOUND-FLAG
           MOVE "N" TO END-OF-FILE
           
           PERFORM UNTIL END-OF-FILE = "Y"
               READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       IF EMPLOYEE-ID = SEARCH-ID
                           MOVE "Y" TO FOUND-FLAG
                           DISPLAY "Employee Found:"
                           DISPLAY "Name: " EMPLOYEE-NAME
                           DISPLAY "Age: " EMPLOYEE-AGE
                           DISPLAY "Are you sure you want to delete? (Y/N): "
                           ACCEPT CONFIRM-DELETE
                           IF CONFIRM-DELETE NOT = "Y" AND 
                              CONFIRM-DELETE NOT = "y"
                               MOVE EMPLOYEE-RECORD TO TEMP-RECORD
                               WRITE TEMP-RECORD
                           END-IF
                       ELSE
                           MOVE EMPLOYEE-RECORD TO TEMP-RECORD
                           WRITE TEMP-RECORD
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE EMPLOYEE-FILE
           CLOSE TEMP-FILE
           
           IF FOUND-FLAG = "N"
               DISPLAY "Employee not found!"
           ELSE
               CLOSE EMPLOYEE-FILE
               CLOSE TEMP-FILE
               IF CONFIRM-DELETE = "Y" OR CONFIRM-DELETE = "y"
                   CALL "CBL_DELETE_FILE" USING WS-NEW-FILENAME
                       RETURNING WS-DELETE-STATUS
                   CALL "CBL_COPY_FILE" USING 
                       WS-OLD-FILENAME 
                       WS-NEW-FILENAME
                       RETURNING WS-COPY-STATUS
                   IF WS-COPY-STATUS = 0
                       CALL "CBL_DELETE_FILE" USING WS-OLD-FILENAME
                       DISPLAY "Employee record deleted successfully!"
                   ELSE
                       DISPLAY "Error deleting record!"
                   END-IF
               ELSE
                   CALL "CBL_DELETE_FILE" USING WS-NEW-FILENAME
                       RETURNING WS-DELETE-STATUS
                   CALL "CBL_COPY_FILE" USING 
                       WS-OLD-FILENAME 
                       WS-NEW-FILENAME
                       RETURNING WS-COPY-STATUS
                   IF WS-COPY-STATUS = 0
                       CALL "CBL_DELETE_FILE" USING WS-OLD-FILENAME
                       DISPLAY "Delete operation cancelled."
                   ELSE
                       DISPLAY "Error cancelling operation!"
                   END-IF
               END-IF
           END-IF.

       PRESS-ENTER.
           DISPLAY "Press Enter to continue..."
           ACCEPT CONTINUE-FLAG.

       CLEAR-SCREEN.
           CALL 'SYSTEM' USING 'clear'
           IF RETURN-CODE NOT = 0
               CALL 'SYSTEM' USING 'cls'
           END-IF.
