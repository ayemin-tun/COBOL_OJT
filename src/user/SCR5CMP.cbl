       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCR5CMP.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PREMIUM-DISP            PIC ZZZ,ZZ9.

       LINKAGE SECTION.
       01  LS-APP-DATA.
           *> --- UNIFIED EXACT MATCHING STRUCTURE ---
           05 LS-APP-ID               PIC 9(4).
           05 LS-DEVICE-TYPE          PIC X(20).
           05 LS-DEVICE-MODEL         PIC X(30).
           05 LS-PURCHASE-PRICE       PIC 9(6).
           05 LS-PURCHASE-DATE        PIC X(10).
           05 LS-COVERAGE-PERIOD      PIC 9(2).
           05 LS-PREMIUM              PIC 9(6).
           05 LS-PLAN-NAME            PIC X(10).
           05 LS-USER-NAME            PIC X(30).
           05 LS-USER-EMAIL           PIC X(20).
           05 LS-USER-PHONE           PIC X(15).
           05 LS-USER-POSTAL          PIC X(10).
           05 LS-USER-ADDRESS         PIC X(50).
           05 LS-USER-DOB             PIC X(10).
           *> Dynamic Answers Array (Matches Main Program)
           05 LS-DEC-ANSWERS.
              10 LS-DEC-ANS           OCCURS 20 TIMES PIC X.
           05 LS-STATUS               PIC X(10).
           05 FILLER                  PIC X(20).

       PROCEDURE DIVISION USING LS-APP-DATA.
           MOVE LS-PREMIUM TO WS-PREMIUM-DISP.
           DISPLAY " ".
           DISPLAY "--- SCREEN 5: APPLICATION COMPLETED ---".
           DISPLAY "Application ID: " LS-APP-ID.
           DISPLAY "Applicant Name: " FUNCTION TRIM(LS-USER-NAME).
           DISPLAY "Plan Name: " FUNCTION TRIM(LS-PLAN-NAME).
           DISPLAY "Estimated Premium: " WS-PREMIUM-DISP " JPY".
           DISPLAY "Message: Your application has been submitted.".
           
           DISPLAY "Status:" LS-STATUS .
           EXIT PROGRAM.