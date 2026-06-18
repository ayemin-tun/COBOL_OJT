       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCR2PLN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VALID                   PIC X.
       01  WS-CONFIRM-PLAN            PIC X VALUE "N".
       01  WS-USER-CONFIRM            PIC X.

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
           DISPLAY " ".
           DISPLAY "--- SCREEN 2: PLAN SELECTION ---".
           DISPLAY "Available Plans & Coverage Details:".
           DISPLAY "--------------------------------------------------".
           DISPLAY " [Light]    - Screen Damage only.".
           DISPLAY "[Standard]-Screen, Water Damage & Natural Failure.".
           DISPLAY " [Premium] Screen,Water Damage & Natural Failure " &
                   "+ Theft.".
           DISPLAY "--------------------------------------------------".
           
           *> Outer Loop for Plan Selection and Confirmation
           MOVE "N" TO WS-CONFIRM-PLAN.
           PERFORM UNTIL WS-CONFIRM-PLAN = "Y"
           
               *> Plan Selection Validation
               MOVE "N" TO WS-VALID
               PERFORM UNTIL WS-VALID = "Y"
                   DISPLAY"Enter Selected Plan(Light/Standard/Premium) "
                   ACCEPT LS-PLAN-NAME
                   
                   IF FUNCTION UPPER-CASE
                   (FUNCTION TRIM(LS-PLAN-NAME)) = "LIGHT"
                       MOVE "Light" TO LS-PLAN-NAME
                       MOVE "Y" TO WS-VALID
                   ELSE IF FUNCTION UPPER-CASE
                   (FUNCTION TRIM(LS-PLAN-NAME)) = "STANDARD"
                       MOVE "Standard" TO LS-PLAN-NAME
                       MOVE "Y" TO WS-VALID
                   ELSE IF FUNCTION UPPER-CASE
                   (FUNCTION TRIM(LS-PLAN-NAME)) = "PREMIUM"
                       MOVE "Premium" TO LS-PLAN-NAME
                       MOVE "Y" TO WS-VALID
                   ELSE
                       DISPLAY"[Error]Invalid Plan Nametry again."
                   END-IF
               END-PERFORM
               
               *> Confirmation Step (Y/N)
               MOVE "N" TO WS-VALID
               PERFORM UNTIL WS-VALID = "Y"
                   DISPLAY "Confirm selected plan? (Y/N): "
                   ACCEPT WS-USER-CONFIRM
                   
                   IF FUNCTION UPPER-CASE
                   (FUNCTION TRIM(WS-USER-CONFIRM)) = "Y"
                       MOVE "Y" TO WS-CONFIRM-PLAN
                       MOVE "Y" TO WS-VALID
                   ELSE IF FUNCTION UPPER-CASE
                   (FUNCTION TRIM(WS-USER-CONFIRM)) = "N"
                       DISPLAY " "
                       DISPLAY "[Info] Please select your plan again."
                       MOVE "N" TO WS-CONFIRM-PLAN
                       MOVE "Y" TO WS-VALID
                   ELSE
                       DISPLAY"[Error]Invalid input!Please enter Y or N"
                   END-IF
               END-PERFORM
               
           END-PERFORM.
           
           EXIT PROGRAM.