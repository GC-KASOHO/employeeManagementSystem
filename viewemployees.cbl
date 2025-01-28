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
           05 FILE-LINE        PIC X(80).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS          PIC XX.
       01 END-OF-FILE          PIC X VALUE "N".
       01 CONTINUE-FLAG        PIC X.
       01 WS-HEADER1.
           05 FILLER           PIC X(47) VALUE 
              "+-------+------------------------+-----+".
       01 WS-HEADER2.
           05 FILLER           PIC X(47) VALUE
              "| ID    | Name                   | Age |".
       01 WS-HEADER3.
           05 FILLER           PIC X(47) VALUE
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

          DISPLAY WS-HEADER1
          DISPLAY WS-HEADER2
          DISPLAY WS-HEADER3
          
          PERFORM UNTIL END-OF-FILE = "Y"
              READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                  AT END
                      MOVE "Y" TO END-OF-FILE
                  NOT AT END
                      DISPLAY FILE-LINE
              END-READ
          END-PERFORM.
          
          DISPLAY WS-HEADER1
          CLOSE EMPLOYEE-FILE
          PERFORM PRESS-ENTER
          EXIT PROGRAM.

       PRESS-ENTER.
          DISPLAY "Press Enter to continue..."
          ACCEPT CONTINUE-FLAG.

       CLEAR-SCREEN.
          CALL 'SYSTEM' USING 'cls'.



          