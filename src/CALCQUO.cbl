       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCQUO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PREMIUM-DISP            PIC ZZZ,ZZ9.
       01  WS-BASE-RATE               PIC V99 VALUE .02.
       01  WS-MULTIPLIER              PIC 9V9.
       01  WS-PLAN-MULT               PIC 9V9.

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
           
           *> 1. Determine Period Multiplier
           EVALUATE LS-COVERAGE-PERIOD
               WHEN 12
                   MOVE 1.0 TO WS-MULTIPLIER
               WHEN 24
                   MOVE 1.8 TO WS-MULTIPLIER
               WHEN 36
                   MOVE 2.5 TO WS-MULTIPLIER
               WHEN OTHER
                   MOVE 1.0 TO WS-MULTIPLIER
           END-EVALUATE.

          
           EVALUATE FUNCTION UPPER-CASE(FUNCTION TRIM(LS-PLAN-NAME))
               WHEN "LIGHT"
                   MOVE 1.0 TO WS-PLAN-MULT
               WHEN "STANDARD"
                   MOVE 1.5 TO WS-PLAN-MULT
               WHEN "PREMIUM"
                   MOVE 2.0 TO WS-PLAN-MULT
               WHEN OTHER
                   MOVE 1.0 TO WS-PLAN-MULT
           END-EVALUATE.

           *> 3. Premium Calculation
           COMPUTE LS-PREMIUM = LS-PURCHASE-PRICE 
                              * WS-BASE-RATE 
                              * WS-MULTIPLIER
                              * WS-PLAN-MULT.

           MOVE LS-PREMIUM TO WS-PREMIUM-DISP.
           
           DISPLAY " ".
           DISPLAY "--- QUOTATION CALCULATION COMPLETE ---".
           DISPLAY"Calculated Estimated Premium: "WS-PREMIUM-DISP" JPY".

           EXIT PROGRAM.