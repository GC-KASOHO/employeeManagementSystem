       IDENTIFICATION DIVISION.
       PROGRAM-ID. ViewEmployees.

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
           05 EMPLOYEE-ID       PIC 9(5).
           05 FILLER           PIC X VALUE "|".
           05 EMPLOYEE-NAME     PIC X(20).
           05 FILLER           PIC X VALUE "|".
           05 EMPLOYEE-AGE      PIC 9(2).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS          PIC XX.
       01 END-OF-FILE          PIC X VALUE "N".
       01 CONTINUE-FLAG        PIC X.
       01 TABLE-HEADER.
           05 FILLER PIC X(47) VALUE 
              "+-------+------------------------+-----+".
       01 COLUMN-HEADERS.
           05 FILLER PIC X(47) VALUE 
              "| ID    | Name                   | Age |".
       01 DASHED-LINE.
           05 FILLER PIC X(47) VALUE
              "+-------+------------------------+-----+".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM CLEAR-SCREEN
           MOVE "N" TO END-OF-FILE
           
           OPEN INPUT EMPLOYEE-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file. No records exist."
               PERFORM PRESS-ENTER
               EXIT PROGRAM
           END-IF.

           DISPLAY DASHED-LINE
           DISPLAY COLUMN-HEADERS
           DISPLAY DASHED-LINE
           
           PERFORM UNTIL END-OF-FILE = "Y"
               READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       DISPLAY "| " EMPLOYEE-ID 
                               " | " EMPLOYEE-NAME 
                               " | " EMPLOYEE-AGE 
                               " |"
               END-READ
           END-PERFORM.
           
           CLOSE EMPLOYEE-FILE
           DISPLAY DASHED-LINE
           PERFORM PRESS-ENTER
           EXIT PROGRAM.

       PRESS-ENTER.
           DISPLAY "Press Enter to continue..."
           ACCEPT CONTINUE-FLAG.

       CLEAR-SCREEN.
           CALL 'SYSTEM' USING 'cls'.

           