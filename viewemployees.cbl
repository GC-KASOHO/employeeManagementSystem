       IDENTIFICATION DIVISION.
       PROGRAM-ID. ViewEmployees.

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
       01 END-OF-FILE          PIC X VALUE "N".
       01 CONTINUE-FLAG        PIC X.
       01 TABLE-LINE           PIC X(50) VALUE ALL "-".

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

           DISPLAY "+-------+----------------------+-----+"
           DISPLAY "|   ID  | Name                 | Age |"
           DISPLAY "+-------+----------------------+-----+"
           
           PERFORM UNTIL END-OF-FILE = "Y"
               READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       DISPLAY "| " EMPLOYEE-ID
                               " | " EMPLOYEE-NAME(1:20)
                               " | " EMPLOYEE-AGE
                               "  |"
               END-READ
           END-PERFORM.
           
           CLOSE EMPLOYEE-FILE
           DISPLAY "+-------+----------------------+-----+"
           PERFORM PRESS-ENTER
           EXIT PROGRAM.

       PRESS-ENTER.
           DISPLAY "Press Enter to continue..."
           ACCEPT CONTINUE-FLAG.

       CLEAR-SCREEN.
           CALL 'SYSTEM' USING 'cls'.






           