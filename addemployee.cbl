       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddEmployee.

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
       01 WS-ERROR-MSG        PIC X(50).
       01 CONTINUE-FLAG       PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Employee ID (5 digits): "
           ACCEPT EMPLOYEE-ID
           IF EMPLOYEE-ID IS NOT NUMERIC OR EMPLOYEE-ID = ZEROS
               DISPLAY "Invalid ID format. Must be 5 digits."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
           END-IF.

           DISPLAY "Enter Employee Name: "
           ACCEPT EMPLOYEE-NAME
           IF EMPLOYEE-NAME = SPACES
               DISPLAY "Name cannot be empty."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
           END-IF.

           DISPLAY "Enter Employee Age: "
           ACCEPT EMPLOYEE-AGE
           IF EMPLOYEE-AGE IS NOT NUMERIC OR 
              EMPLOYEE-AGE < 18 OR EMPLOYEE-AGE > 99
               DISPLAY "Invalid age. Must be between 18 and 99."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
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
           