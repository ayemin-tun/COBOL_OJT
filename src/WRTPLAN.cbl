       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRTPLAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PLAN-FILE ASSIGN TO "result/plan.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PLAN-FILE.
       01  PLAN-REC                   PIC X(150).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC XX.
       01  WS-IDX                     PIC 9(2).
       01  WS-ANS-COUNT               PIC 9(2).
       
       01  WS-HEADER-CSV.
           05 FILLER PIC X(5)  VALUE "AppID".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(8)  VALUE "PlanName".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(6)  VALUE "Period".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(7)  VALUE "Premium".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(20) VALUE "Answers".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(6)  VALUE "Status".
           05 FILLER PIC X     VALUE ",".
           05 FILLER PIC X(5)  VALUE "Score".

       01  WS-FORMATTED-REC.
           05 WS-F-APP-ID             PIC 9(4).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-PLAN               PIC X(10).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-PERIOD             PIC 9(2).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-PREMIUM            PIC 9(6).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-ANSWERS            PIC X(20).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-STATUS             PIC X(10).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-TOTAL-SCORE        PIC 9(3).

       LINKAGE SECTION.
       01  LS-APP-DATA.
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
           
           MOVE LS-APP-ID TO WS-F-APP-ID.
           MOVE LS-PLAN-NAME TO WS-F-PLAN.
           MOVE LS-COVERAGE-PERIOD TO WS-F-PERIOD.
           MOVE LS-PREMIUM TO WS-F-PREMIUM.
           
           MOVE SPACES TO WS-F-ANSWERS.
           MOVE 0 TO WS-ANS-COUNT.
           MOVE "REJECT" TO WS-F-STATUS.

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 20
               IF LS-DEC-ANS(WS-IDX) NOT = SPACE
                   ADD 1 TO WS-ANS-COUNT
                   
                   MOVE FUNCTION UPPER-CASE(LS-DEC-ANS(WS-IDX)) 
                        TO WS-F-ANSWERS(WS-IDX:1)

                   IF FUNCTION UPPER-CASE(LS-DEC-ANS(WS-IDX)) = "N"
                       MOVE "PENDING" TO WS-F-STATUS
                   END-IF
               END-IF
           END-PERFORM.

           IF WS-ANS-COUNT = 0
               MOVE "PENDING" TO WS-F-STATUS
           END-IF.
           
           MOVE WS-F-STATUS TO LS-STATUS.
           MOVE LS-TOTAL-SCORE TO WS-F-TOTAL-SCORE.
        
           OPEN EXTEND PLAN-FILE.
           
           IF WS-FILE-STATUS = "35"
               OPEN OUTPUT PLAN-FILE
               WRITE PLAN-REC FROM WS-HEADER-CSV
               WRITE PLAN-REC FROM WS-FORMATTED-REC
               CLOSE PLAN-FILE
           ELSE
               WRITE PLAN-REC FROM WS-FORMATTED-REC
               CLOSE PLAN-FILE
           END-IF.
           
           EXIT PROGRAM.