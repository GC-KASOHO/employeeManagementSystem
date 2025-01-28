       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddEmployee.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employees.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 FILE-LINE        PIC X(80).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS          PIC XX.
       01 WS-ERROR-MSG         PIC X(50).
       01 CONTINUE-FLAG        PIC X.
       01 WS-EMPLOYEE.
           05 WS-ID            PIC 9(5).
           05 WS-NAME          PIC X(20).
           05 WS-AGE           PIC 9(2).
       01 WS-HEADER1.
           05 FILLER           PIC X(47) VALUE 
              "+-------+------------------------+-----+".
       01 WS-HEADER2.
           05 FILLER           PIC X(47) VALUE
              "| ID    | Name                   | Age |".
       01 WS-HEADER3.
           05 FILLER           PIC X(47) VALUE
              "+-------+------------------------+-----+".
       01 WS-OUTPUT-LINE.
           05 FILLER           PIC X(2) VALUE "| ".
           05 WS-OUT-ID        PIC 9(5).
           05 FILLER           PIC X(4) VALUE " | ".
           05 WS-OUT-NAME      PIC X(20).
           05 FILLER           PIC X(4) VALUE " | ".
           05 WS-OUT-AGE       PIC Z9.
           05 FILLER           PIC X(3) VALUE " |".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter Employee ID (5 digits): "
           ACCEPT WS-ID
           IF WS-ID IS NOT NUMERIC OR WS-ID = ZEROS
               DISPLAY "Invalid ID format. Must be 5 digits."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
           END-IF.

           DISPLAY "Enter Employee Name: "
           ACCEPT WS-NAME
           IF WS-NAME = SPACES
               DISPLAY "Name cannot be empty."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
           END-IF.

           DISPLAY "Enter Employee Age: "
           ACCEPT WS-AGE
           IF WS-AGE IS NOT NUMERIC OR 
              WS-AGE < 18 OR WS-AGE > 99
               DISPLAY "Invalid age. Must be between 18 and 99."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
           END-IF.

           MOVE WS-ID TO WS-OUT-ID
           MOVE WS-NAME TO WS-OUT-NAME
           MOVE WS-AGE TO WS-OUT-AGE

           OPEN EXTEND EMPLOYEE-FILE
           IF FILE-STATUS NOT = "00"
               OPEN OUTPUT EMPLOYEE-FILE
               WRITE EMPLOYEE-RECORD FROM WS-HEADER1
               WRITE EMPLOYEE-RECORD FROM WS-HEADER2
               WRITE EMPLOYEE-RECORD FROM WS-HEADER3
           END-IF.
           
           IF FILE-STATUS = "00"
               MOVE WS-OUTPUT-LINE TO EMPLOYEE-RECORD
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
           CALL 'SYSTEM' USING 'cls'.


