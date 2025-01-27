       IDENTIFICATION DIVISION.
       PROGRAM-ID. EditEmployee.

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
       01 FILE-STATUS          PIC XX.
       01 TEMP-STATUS         PIC XX.
       01 SEARCH-ID           PIC 9(5).
       01 FOUND-FLAG          PIC X VALUE "N".
       01 END-OF-FILE         PIC X VALUE "N".
       01 CONTINUE-FLAG       PIC X.
       01 WS-COPY-STATUS      PIC S9(9) USAGE BINARY.
       01 WS-DELETE-STATUS    PIC S9(9) USAGE BINARY.
       01 WS-OLD-FILENAME     PIC X(255) VALUE "temp.dat".
       01 WS-NEW-FILENAME     PIC X(255) VALUE "employees.dat".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Employee ID to edit: "
           ACCEPT SEARCH-ID
           
           IF SEARCH-ID IS NOT NUMERIC OR SEARCH-ID = ZEROS
               DISPLAY "Invalid ID format. Must be 5 digits."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
           END-IF.

           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT TEMP-FILE
           
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file. No records exist."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
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
           
           PERFORM PRESS-ENTER
           EXIT PROGRAM.

       PRESS-ENTER.
           DISPLAY "Press Enter to continue..."
           ACCEPT CONTINUE-FLAG.

       CLEAR-SCREEN.
           CALL 'SYSTEM' USING 'clear'
           IF RETURN-CODE NOT = 0
               CALL 'SYSTEM' USING 'cls'
           END-IF.
           