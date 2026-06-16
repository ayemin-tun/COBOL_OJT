       IDENTIFICATION DIVISION.
       PROGRAM-ID. APP-REGISTER-SCORING.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Application Table File
           SELECT OPTIONAL APP-FILE ASSIGN TO 'app-data.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
      * Declaration Table File
           SELECT OPTIONAL DECL-FILE ASSIGN TO 'decl-data.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  APP-FILE.
       01  APP-RECORD.
           05 T-APP-ID           PIC X(10).
           05 T-USER-NAME        PIC X(50).
           05 T-EMAIL            PIC X(100).
           05 T-ADDRESS          PIC X(200).
           05 T-DEVICE-TYPE      PIC X(10).
           05 T-PLAN-CODE        PIC X(5).
           05 T-STATUS           PIC X(10).
           05 T-CREATED-AT       PIC X(14).

       FD  DECL-FILE.
       01  DECL-RECORD.
           05 D-APP-ID           PIC X(10).
           05 D-DAMAGE-FLG       PIC X(1).
           05 D-SCREEN-FLG       PIC X(1).
           05 D-WATER-FLG        PIC X(1).
           05 D-OLD-DEVICE-FLG   PIC X(1).

       WORKING-STORAGE SECTION.
       01  WS-INPUT-DATA.
           05 WS-NAME            PIC X(50) VALUE SPACES.
           05 WS-EMAIL           PIC X(100) VALUE SPACES.
           05 WS-ADDRESS         PIC X(200) VALUE SPACES.
           05 WS-PHONE           PIC X(20) VALUE SPACES.
           05 WS-APP-DATE        PIC X(8) VALUE SPACES.
           05 WS-DEVICE-TYPE     PIC X(10) VALUE SPACES.

       01  WS-DECLARATION-ANSWERS.
           05 WS-Q1-DAMAGE       PIC X VALUE SPACES.
           05 WS-Q2-SCREEN       PIC X VALUE SPACES.
           05 WS-Q3-WATER        PIC X VALUE SPACES.
           05 WS-Q4-OLD          PIC X VALUE SPACES.

       01  WS-SCORING-DATA.
           05 WS-RISK-SCORE      PIC 9(3) VALUE ZEROS.
           05 WS-FINAL-RESULT    PIC X(15) VALUE SPACES.

       01  WS-SYSTEM-VARS.
           05 WS-VALID-FLAG      PIC X VALUE 'N'.
           05 WS-AT-COUNT        PIC 9(2) VALUE ZEROS.
           05 WS-SYS-DATE        PIC X(8).
           05 WS-SYS-TIME        PIC X(8).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "--- Application Registration ---"
           PERFORM GET-APP-INPUTS
           PERFORM SAVE-TO-APP-TABLE
           DISPLAY " "
           DISPLAY ">>> Application successfully saved! <<<"
           DISPLAY " "
           
           DISPLAY "--- Device Declaration ---"
           PERFORM GET-DECLARATION-INPUTS
           PERFORM CALCULATE-RISK-SCORE
           PERFORM SAVE-TO-DECL-TABLE
           
           DISPLAY " "
           DISPLAY "--- Final Result ---"
           DISPLAY "Total Risk Score : " WS-RISK-SCORE
           DISPLAY "Decision         : " WS-FINAL-RESULT
           DISPLAY "--------------------"
           
           STOP RUN.

       GET-APP-INPUTS.
      * 1 to 3: Name, Email, Address
           DISPLAY "Enter Name (Required): " WITH NO ADVANCING
           ACCEPT WS-NAME
           
           DISPLAY "Enter Email (Required): " WITH NO ADVANCING
           ACCEPT WS-EMAIL
           
           DISPLAY "Enter Address (Required): " WITH NO ADVANCING
           ACCEPT WS-ADDRESS
           
      * 4 & 5: Phone, App Date
           DISPLAY "Enter Phone: " WITH NO ADVANCING
           ACCEPT WS-PHONE
           
           DISPLAY "Enter App Date (YYYYMMDD): " WITH NO ADVANCING
           ACCEPT WS-APP-DATE
           
      * 6. Ask for Device Type
           DISPLAY "Device Type (Android/IOS): " WITH NO ADVANCING
           ACCEPT WS-DEVICE-TYPE.

       GET-DECLARATION-INPUTS.
      * Q1
           DISPLAY "Q1: Existing damage? (Y/N): " WITH NO ADVANCING
           ACCEPT WS-Q1-DAMAGE
           MOVE FUNCTION UPPER-CASE(WS-Q1-DAMAGE) TO WS-Q1-DAMAGE
      * Q2
           DISPLAY "Q2: Screen crack? (Y/N): " WITH NO ADVANCING
           ACCEPT WS-Q2-SCREEN
           MOVE FUNCTION UPPER-CASE(WS-Q2-SCREEN) TO WS-Q2-SCREEN
      * Q3
           DISPLAY "Q3: Water damage history? (Y/N): " WITH NO ADVANCING
           ACCEPT WS-Q3-WATER
           MOVE FUNCTION UPPER-CASE(WS-Q3-WATER) TO WS-Q3-WATER
      * Q4
           DISPLAY "Q4: Purchased over 1 year ago? (Y/N): " 
               WITH NO ADVANCING
           ACCEPT WS-Q4-OLD
           MOVE FUNCTION UPPER-CASE(WS-Q4-OLD) TO WS-Q4-OLD.

       CALCULATE-RISK-SCORE.
           MOVE 0 TO WS-RISK-SCORE
           
      * Add scores based on Logic Image
           IF WS-Q1-DAMAGE = 'Y' 
               ADD 50 TO WS-RISK-SCORE
           END-IF
           
           IF WS-Q2-SCREEN = 'Y' 
               ADD 30 TO WS-RISK-SCORE
           END-IF
           
           IF WS-Q3-WATER = 'Y' 
               ADD 40 TO WS-RISK-SCORE
           END-IF
           
           IF WS-Q4-OLD = 'Y' 
               ADD 20 TO WS-RISK-SCORE
           END-IF
           
      * Determine Result based on Threshold Image
           EVALUATE WS-RISK-SCORE
               WHEN 0 THRU 30
                   MOVE "APPROVED" TO WS-FINAL-RESULT
               WHEN 31 THRU 70
                   MOVE "CONDITIONAL" TO WS-FINAL-RESULT
               WHEN OTHER
                   MOVE "REJECTED" TO WS-FINAL-RESULT
           END-EVALUATE.

       SAVE-TO-APP-TABLE.
           OPEN EXTEND APP-FILE
           MOVE SPACES TO APP-RECORD
           
           ACCEPT WS-SYS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-SYS-TIME FROM TIME
           STRING "AP" WS-SYS-TIME(1:6) DELIMITED BY SIZE INTO T-APP-ID
           
           MOVE WS-NAME TO T-USER-NAME
           MOVE WS-EMAIL TO T-EMAIL
           MOVE WS-ADDRESS TO T-ADDRESS
           MOVE WS-DEVICE-TYPE TO T-DEVICE-TYPE
           MOVE "BASIC" TO T-PLAN-CODE
           MOVE WS-FINAL-RESULT TO T-STATUS
           STRING WS-SYS-DATE WS-SYS-TIME(1:6) DELIMITED BY SIZE 
                  INTO T-CREATED-AT
                  
           WRITE APP-RECORD
           CLOSE APP-FILE.

       SAVE-TO-DECL-TABLE.
           OPEN EXTEND DECL-FILE
           MOVE SPACES TO DECL-RECORD
           
      * Use the same APP-ID generated earlier to link the two tables
           MOVE T-APP-ID TO D-APP-ID
           MOVE WS-Q1-DAMAGE TO D-DAMAGE-FLG
           MOVE WS-Q2-SCREEN TO D-SCREEN-FLG
           MOVE WS-Q3-WATER TO D-WATER-FLG
           MOVE WS-Q4-OLD TO D-OLD-DEVICE-FLG
           
           WRITE DECL-RECORD
           CLOSE DECL-FILE.
           