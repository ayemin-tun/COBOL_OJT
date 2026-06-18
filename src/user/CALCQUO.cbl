       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCQUO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PREMIUM-DISP            PIC ZZZ,ZZ9.
       01  WS-BASE-RATE               PIC V99 VALUE .02.
       01  WS-MULTIPLIER              PIC 9V9.

       LINKAGE SECTION.
       01  LS-APP-DATA.
           *> Exact matching structure to avoid Offset errors
           05 LS-APP-ID               PIC 9(4).
           05 LS-DEVICE-TYPE          PIC X(20).
           05 LS-DEVICE-MODEL         PIC X(30).
           05 LS-PURCHASE-PRICE       PIC 9(6).
           05 LS-PURCHASE-DATE        PIC X(10).
           05 LS-COVERAGE-PERIOD      PIC 9(2).
           05 LS-PREMIUM              PIC 9(6).
           05 LS-PLAN-NAME            PIC X(10).
           05 FILLER                  PIC X(146).

       PROCEDURE DIVISION USING LS-APP-DATA.

           *> Determine Multiplier based on Coverage Period
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

           *> Premium Calculation
           COMPUTE LS-PREMIUM = LS-PURCHASE-PRICE 
                              * WS-BASE-RATE * WS-MULTIPLIER.

           MOVE LS-PREMIUM TO WS-PREMIUM-DISP.
           DISPLAY " ".
           DISPLAY "--- QUOTATION RESULT ---".
           DISPLAY "Calculated Estimated Premium: " 
                   WS-PREMIUM-DISP " JPY".

           EXIT PROGRAM.