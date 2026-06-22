       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCR2PLN.

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
       01  WS-VALID                   PIC X.
       01  WS-CONFIRM-PLAN            PIC X VALUE "N".
       01  WS-USER-CONFIRM            PIC X.
       01  WS-TEMP-PLAN               PIC X(20).
       01  WS-FOUND                   PIC X.
    
       01  WS-BASE-RATE               PIC V99 VALUE .02.
       01  WS-MULTIPLIER              PIC 9V9.
       01  WS-PLAN-MULT               PIC 9V9.
       01  WS-CALC-PREMIUM            PIC 9(6).
       01  WS-PREMIUM-DISP            PIC ZZZ,ZZ9.
       
      
       01  WS-PLAN-TABLE.
           05 WS-PLAN-ENTRY OCCURS 10 TIMES.
              10 WS-P-NAME            PIC X(15).
              10 WS-P-DESC            PIC X(60).
              
       01  WS-PLAN-COUNT              PIC 9(2) VALUE 0.
       01  WS-IDX                     PIC 9(2).

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
           05 FILLER                  PIC X(20).

       PROCEDURE DIVISION USING LS-APP-DATA.
           DISPLAY " ".
           DISPLAY "--- SCREEN 2: PLAN SELECTION ---".
           
          
           OPEN INPUT PLAN-DEF-FILE.
           IF WS-FILE-STATUS = "35"
               DISPLAY "[Error] plan_master.csv file not found!"
               EXIT PROGRAM
           END-IF.

           MOVE "N" TO WS-EOF.
           MOVE 0 TO WS-PLAN-COUNT.
           PERFORM UNTIL WS-EOF = "Y" OR WS-PLAN-COUNT >= 10
               READ PLAN-DEF-FILE INTO PLAN-DEF-REC
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-PLAN-COUNT
                       UNSTRING PLAN-DEF-REC DELIMITED BY ","
                           INTO WS-P-NAME(WS-PLAN-COUNT)
                                WS-P-DESC(WS-PLAN-COUNT)
               END-READ
           END-PERFORM.
           CLOSE PLAN-DEF-FILE.
           
           
           MOVE "N" TO WS-CONFIRM-PLAN.
           PERFORM UNTIL WS-CONFIRM-PLAN = "Y"
           
               DISPLAY " "
               DISPLAY "==============================================="
               DISPLAY "        ALL AVAILABLE PLANS REVIEW             "
               DISPLAY "==============================================="
               
               
               PERFORM VARYING WS-IDX FROM 1 BY 1 
                 UNTIL WS-IDX > WS-PLAN-COUNT
                   
                   
                   MOVE FUNCTION UPPER-CASE
                   (FUNCTION TRIM(WS-P-NAME(WS-IDX)))
                        TO WS-TEMP-PLAN
                   EVALUATE FUNCTION TRIM(WS-TEMP-PLAN)
                       WHEN "LIGHT"
                           MOVE 1.0 TO WS-PLAN-MULT
                       WHEN "STANDARD"
                           MOVE 1.5 TO WS-PLAN-MULT
                       WHEN "PREMIUM"
                           MOVE 2.0 TO WS-PLAN-MULT
                       WHEN OTHER
                           MOVE 1.0 TO WS-PLAN-MULT
                   END-EVALUATE
                   
                
                   EVALUATE LS-COVERAGE-PERIOD
                       WHEN 12
                           MOVE 1.0 TO WS-MULTIPLIER
                       WHEN 24
                           MOVE 1.8 TO WS-MULTIPLIER
                       WHEN 36
                           MOVE 2.5 TO WS-MULTIPLIER
                       WHEN OTHER
                           MOVE 1.0 TO WS-MULTIPLIER
                   END-EVALUATE
                   
                 
                   COMPUTE WS-CALC-PREMIUM = LS-PURCHASE-PRICE 
                                           * WS-BASE-RATE 
                                           * WS-MULTIPLIER
                                           * WS-PLAN-MULT
                   MOVE WS-CALC-PREMIUM TO WS-PREMIUM-DISP
                   
                
                   DISPLAY "Plan Name        : [" 
                           FUNCTION TRIM(WS-P-NAME(WS-IDX)) "]"
                   DISPLAY "Coverage Period  : " 
                           LS-COVERAGE-PERIOD " Months"
                   DISPLAY "Estimated Premium: " 
                           WS-PREMIUM-DISP " JPY"
                   DISPLAY "Coverage Details : " 
                           FUNCTION TRIM(WS-P-DESC(WS-IDX))
                   DISPLAY "-------------------------------------------"
               END-PERFORM
               
              
               MOVE "N" TO WS-VALID
               PERFORM UNTIL WS-VALID = "Y"
                   DISPLAY "Enter Selected Plan Name: "
                   ACCEPT WS-TEMP-PLAN
                   
                   MOVE "N" TO WS-FOUND
                   PERFORM VARYING WS-IDX FROM 1 BY 1 
                     UNTIL WS-IDX > WS-PLAN-COUNT
                       
                     IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-TEMP-PLAN))
                          = FUNCTION UPPER-CASE(
                              FUNCTION TRIM(WS-P-NAME(WS-IDX)))
                          
                          MOVE WS-P-NAME(WS-IDX) TO LS-PLAN-NAME
                          MOVE "Y" TO WS-FOUND
                       END-IF
                   END-PERFORM
                   
                   IF WS-FOUND = "Y"
                       MOVE "Y" TO WS-VALID
                   ELSE
                       DISPLAY "[Error] Invalid Plan Name. Try again."
                   END-IF
               END-PERFORM
               
           
               MOVE "N" TO WS-VALID
               PERFORM UNTIL WS-VALID = "Y"
                   DISPLAY "Confirm your selected plan? (Y/N): "
                   ACCEPT WS-USER-CONFIRM
                   
                  IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-USER-CONFIRM)) 
                      = "Y"
                       MOVE "Y" TO WS-CONFIRM-PLAN
                       MOVE "Y" TO WS-VALID
                   ELSE IF FUNCTION UPPER-CASE(
                           FUNCTION TRIM(WS-USER-CONFIRM)) = "N"
                       DISPLAY " "
                       DISPLAY"[Info] Restarting Plan Selection Menu..."
                       MOVE "N" TO WS-CONFIRM-PLAN
                       MOVE "Y" TO WS-VALID
                   ELSE
                       DISPLAY "[Error] Invalid input! Enter Y or N"
                   END-IF
               END-PERFORM
               
           END-PERFORM.
           
           EXIT PROGRAM.
           