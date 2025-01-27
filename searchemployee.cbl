       IDENTIFICATION DIVISION.
       PROGRAM-ID. SearchEmployee.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employees.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMPLOYEE-ID       PIC 9(5).
           05 EMPLOYEE-NAME     PIC X(30).
           05 EMPLOYEE-AGE      PIC 9(2).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS          PIC XX.
       01 SEARCH-ID           PIC 9(5).
       01 FOUND-FLAG          PIC X VALUE "N".
       01 END-OF-FILE         PIC X VALUE "N".
       01 CONTINUE-FLAG       PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Employee ID to search: "
           ACCEPT SEARCH-ID
           
           IF SEARCH-ID IS NOT NUMERIC OR SEARCH-ID = ZEROS
               DISPLAY "Invalid ID format. Must be 5 digits."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
           END-IF.

           OPEN INPUT EMPLOYEE-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file. No records exist."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
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
           