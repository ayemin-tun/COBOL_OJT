       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCR4DEC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VALID                   PIC X.

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
           DISPLAY " ".
           DISPLAY "--- SCREEN 4: DECLARATION ---".
           
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "Is the device already damaged? (Y/N): "
               ACCEPT LS-DEC-1
               IF LS-DEC-1 = 'Y' OR LS-DEC-1 = 'y' OR 
                  LS-DEC-1 = 'N' OR LS-DEC-1 = 'n'
                   MOVE "Y" TO WS-VALID
               ELSE
                   DISPLAY "[Error] Please enter Y or N."
               END-IF
           END-PERFORM.

           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "Is the screen cracked? (Y/N): "
               ACCEPT LS-DEC-2
               IF LS-DEC-2 = 'Y' OR LS-DEC-2 = 'y' OR 
                  LS-DEC-2 = 'N' OR LS-DEC-2 = 'n'
                   MOVE "Y" TO WS-VALID
               ELSE
                   DISPLAY "[Error] Please enter Y or N."
               END-IF
           END-PERFORM.

           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "Has the device been water-damaged? (Y/N): "
               ACCEPT LS-DEC-3
               IF LS-DEC-3 = 'Y' OR LS-DEC-3 = 'y' OR 
                  LS-DEC-3 = 'N' OR LS-DEC-3 = 'n'
                   MOVE "Y" TO WS-VALID
               ELSE
                   DISPLAY "[Error] Please enter Y or N."
               END-IF
           END-PERFORM.

           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "Was the device purchased > 1 year ago? (Y/N): "
               ACCEPT LS-DEC-4
               IF LS-DEC-4 = 'Y' OR LS-DEC-4 = 'y' OR 
                  LS-DEC-4 = 'N' OR LS-DEC-4 = 'n'
                   MOVE "Y" TO WS-VALID
               ELSE
                   DISPLAY "[Error] Please enter Y or N."
               END-IF
           END-PERFORM.

           EXIT PROGRAM.