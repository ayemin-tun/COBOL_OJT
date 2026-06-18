       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRTPLAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PLAN-FILE ASSIGN TO "plan.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PLAN-FILE.
       01  PLAN-REC                   PIC X(120).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC XX.
       
       01  WS-HEADER-CSV.
           05 FILLER PIC X(5)  VALUE "AppID".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(8)  VALUE "PlanName".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(6)  VALUE "Period".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(7)  VALUE "Premium".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(2)  VALUE "D1".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(2)  VALUE "D2".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(2)  VALUE "D3".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(2)  VALUE "D4".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(6)  VALUE "Status".

       01  WS-FORMATTED-REC.
           05 WS-F-APP-ID             PIC 9(4).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-PLAN               PIC X(10).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-PERIOD             PIC 9(2).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-PREMIUM            PIC 9(6).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-D1                 PIC X.
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-D2                 PIC X.
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-D3                 PIC X.
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-D4                 PIC X.
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-STATUS             PIC X(10).

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
           05 LS-USER-EMAIL           PIC X(30).
           05 LS-USER-PHONE           PIC X(15).
           05 LS-USER-POSTAL          PIC X(10).
           05 LS-USER-ADDRESS         PIC X(50).
           05 LS-USER-DOB             PIC X(10).
           05 LS-DEC-1                PIC X.
           05 LS-DEC-2                PIC X.
           05 LS-DEC-3                PIC X.
           05 LS-DEC-4                PIC X.
           05 LS-STATUS               PIC X(10).
           05 FILLER                  PIC X(20).

       PROCEDURE DIVISION USING LS-APP-DATA.
           MOVE LS-APP-ID TO WS-F-APP-ID.
           MOVE LS-PLAN-NAME TO WS-F-PLAN.
           MOVE LS-COVERAGE-PERIOD TO WS-F-PERIOD.
           MOVE LS-PREMIUM TO WS-F-PREMIUM.
           MOVE LS-DEC-1 TO WS-F-D1.
           MOVE LS-DEC-2 TO WS-F-D2.
           MOVE LS-DEC-3 TO WS-F-D3.
           MOVE LS-DEC-4 TO WS-F-D4.
           
           *> Check if all declarations are 'Y'
           IF FUNCTION UPPER-CASE(LS-DEC-1) = "Y" AND 
              FUNCTION UPPER-CASE(LS-DEC-2) = "Y" AND 
              FUNCTION UPPER-CASE(LS-DEC-3) = "Y" AND 
              FUNCTION UPPER-CASE(LS-DEC-4) = "Y"
               MOVE "REJECT" TO WS-F-STATUS
           ELSE
               MOVE "PENDING" TO WS-F-STATUS
           END-IF.
           
           MOVE WS-F-STATUS TO LS-STATUS.

           OPEN INPUT PLAN-FILE.
           IF WS-FILE-STATUS = "35"
               OPEN OUTPUT PLAN-FILE
               WRITE PLAN-REC FROM WS-HEADER-CSV
               WRITE PLAN-REC FROM WS-FORMATTED-REC
               CLOSE PLAN-FILE
           ELSE
               CLOSE PLAN-FILE
               OPEN EXTEND PLAN-FILE
               WRITE PLAN-REC FROM WS-FORMATTED-REC
               CLOSE PLAN-FILE
           END-IF.
           
           EXIT PROGRAM.