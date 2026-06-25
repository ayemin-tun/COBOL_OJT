       IDENTIFICATION DIVISION.
       PROGRAM-ID. SHOWCOV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL PLAN-DEF-FILE 
           ASSIGN TO "data/plan_master.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PLAN-DEF-FILE.
       01  PLAN-DEF-REC               PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC XX.
       01  WS-EOF                     PIC X VALUE "N".
       01  WS-P-NAME                  PIC X(15).
       01  WS-P-DESC                  PIC X(60).
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
           05 LS-DEC-ANSWERS.
              10 LS-DEC-ANS           OCCURS 20 TIMES PIC X.
           05 LS-STATUS               PIC X(10).
           05 LS-TOTAL-SCORE          PIC 9(3). 
           05 FILLER                  PIC X(17).

       PROCEDURE DIVISION USING LS-APP-DATA.
           OPEN INPUT PLAN-DEF-FILE.
           IF WS-FILE-STATUS = "35"
               DISPLAY "[Error] data/plan_master.csv not found!"
               EXIT PROGRAM
           END-IF.

           MOVE "N" TO WS-EOF.
           PERFORM UNTIL WS-EOF = "Y"
               READ PLAN-DEF-FILE INTO PLAN-DEF-REC
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       UNSTRING PLAN-DEF-REC DELIMITED BY ","
                           INTO WS-P-NAME WS-P-DESC
                       
                       *> Match the Plan Name with User's Selection
                       IF FUNCTION UPPER-CASE
                           (FUNCTION TRIM(WS-P-NAME)) 
                        = FUNCTION UPPER-CASE
                           (FUNCTION TRIM(LS-PLAN-NAME))
                           
                           MOVE LS-PREMIUM TO WS-PREMIUM-DISP
                           DISPLAY " "
                           DISPLAY "==================================="
                           DISPLAY "      YOUR SELECTED PLAN SUMMARY   "
                           DISPLAY "==================================="
                           DISPLAY "Plan Name        : " 
                               FUNCTION TRIM(LS-PLAN-NAME)
                           DISPLAY "Coverage Period  : " 
                               LS-COVERAGE-PERIOD " Months"
                           DISPLAY "Coverage Details : " 
                               FUNCTION TRIM(WS-P-DESC)
                           DISPLAY "Total Premium    : " 
                               WS-PREMIUM-DISP " JPY"
                           DISPLAY "==================================="
                           DISPLAY " "
                           MOVE "Y" TO WS-EOF  *> Found it, so exit loop
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE PLAN-DEF-FILE.
           EXIT PROGRAM.
           