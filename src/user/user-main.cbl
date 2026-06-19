       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-BUY-CHOICE              PIC X.
       01  WS-VALID                   PIC X.
       *> user-main.cbl ထဲက APP-DATA အပိုင်းကို ပြင်ဆင်ရန်
       01  APP-DATA.
           *> --- UNIFIED EXACT MATCHING STRUCTURE ---
           05 APP-ID                  PIC 9(4) VALUE 1001.
           05 DEVICE-TYPE             PIC X(20).
           05 DEVICE-MODEL            PIC X(30).
           05 PURCHASE-PRICE          PIC 9(6).
           05 PURCHASE-DATE           PIC X(10).
           05 COVERAGE-PERIOD         PIC 9(2).
           05 PREMIUM                 PIC 9(6).
           05 PLAN-NAME               PIC X(10).
           05 USER-NAME               PIC X(30).
           05 USER-EMAIL              PIC X(20).
           05 USER-PHONE              PIC X(15).
           05 USER-POSTAL             PIC X(10).
           05 USER-ADDRESS            PIC X(50).
           05 USER-DOB                PIC X(10).
           *> မေးခွန်း အခု ၂၀ အထိ Dynamic လက်ခံနိုင်ရန် ပြင်ဆင်ခြင်း
           05 DEC-ANSWERS.
              10 DEC-ANS              OCCURS 20 TIMES PIC X.
           05 APP-STATUS              PIC X(10).
           05 FILLER                  PIC X(20).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           CALL "SCR1DEV" USING APP-DATA.
           CALL "SCR2PLN" USING APP-DATA.
           CALL "CALCQUO" USING APP-DATA.

           DISPLAY " ".
         
           MOVE "N" TO WS-VALID
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "Do you want to proceed and buy? (Y/N): "
               ACCEPT WS-BUY-CHOICE
               IF WS-BUY-CHOICE = 'Y' OR WS-BUY-CHOICE = 'y' OR
                  WS-BUY-CHOICE = 'N' OR WS-BUY-CHOICE = 'n'
                   MOVE "Y" TO WS-VALID
               ELSE
                   DISPLAY "[Error] Please enter Y or N only."
               END-IF
           END-PERFORM.

           IF WS-BUY-CHOICE = 'Y' OR WS-BUY-CHOICE = 'y'
               CALL "SCR3USR" USING APP-DATA
               CALL "SCR4DEC" USING APP-DATA
               CALL "WRTPLAN" USING APP-DATA
               CALL "SCR5CMP" USING APP-DATA
           ELSE
               DISPLAY "Application cancelled. Removing ID: " APP-ID
               CALL "DELDEV" USING APP-ID
               DISPLAY "Record successfully removed."
           END-IF.

           STOP RUN.