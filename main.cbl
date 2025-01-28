       IDENTIFICATION DIVISION.
       PROGRAM-ID. EmployeeManagement.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
           05 USER-CHOICE       PIC 9.
           05 CONTINUE-FLAG     PIC X.

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
               DISPLAY "Enter your choice: " WITH NO ADVANCING
               ACCEPT USER-CHOICE
               
               EVALUATE USER-CHOICE
                   WHEN 1
                       CALL "AddEmployee"
                   WHEN 2
                       CALL "ViewEmployees"
                   WHEN 3
                       CALL "SearchEmployee"
                   WHEN 4
                       CALL "EditEmployee"
                   WHEN 5
                       CALL "DeleteEmployee"
                   WHEN 6
                       DISPLAY "Exiting the system. Goodbye!"
                   WHEN OTHER
                       DISPLAY "Invalid choice. Please try again."
                       PERFORM PRESS-ENTER
               END-EVALUATE
           END-PERFORM.

       PRESS-ENTER.
           DISPLAY "Press Enter to continue..."
           ACCEPT CONTINUE-FLAG.

       CLEAR-SCREEN.
           CALL 'SYSTEM' USING 'CLS'.