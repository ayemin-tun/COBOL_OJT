       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCR4DEC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Connect to the CSV file
           SELECT OPTIONAL QUESTION-FILE ASSIGN TO "data/questions.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  QUESTION-FILE.
       01  QUESTION-REC               PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC XX.
       01  WS-EOF                     PIC X VALUE "N".
       01  WS-VALID                   PIC X.
       01  WS-TEMP-ANS                PIC X.
       
       *> Variables for unstringing the CSV
       01  WS-Q-ID                    PIC X(2).
       01  WS-Q-TEXT                  PIC X(80).
       01  WS-Q-SCORE                 PIC 9(3).
       
       *> Array to store the questions dynamically (Up to 20 questions)
       01  WS-QUESTIONS-TABLE.
           05 WS-Q-ENTRY OCCURS 20 TIMES.
               10 WS-QUESTION PIC X(80).
               10 WS-SCORE PIC 9(3).
           
       01  WS-Q-COUNT                 PIC 9(2) VALUE 0.
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
           *> Dynamic Answers Array (Matches Main Program)
           05 LS-DEC-ANSWERS.
              10 LS-DEC-ANS           OCCURS 20 TIMES PIC X.
           05 LS-STATUS               PIC X(10).
           05 LS-TOTAL-SCORE          PIC 9(3) VALUE 0. 
           05 FILLER                  PIC X(17). 

       PROCEDURE DIVISION USING LS-APP-DATA.
           DISPLAY " ".
           DISPLAY "--- SCREEN 4: DECLARATION ---".
           
           *> (1) Read the CSV and load questions dynamically
           OPEN INPUT QUESTION-FILE.
           IF WS-FILE-STATUS = "35"
               DISPLAY "[Error] questions.csv file not found!"
               EXIT PROGRAM
           END-IF.

           MOVE "N" TO WS-EOF.
           MOVE 0 TO WS-Q-COUNT.
           PERFORM UNTIL WS-EOF = "Y" OR WS-Q-COUNT >= 20
               READ QUESTION-FILE INTO QUESTION-REC
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       *> Extract only the question text
                       UNSTRING QUESTION-REC DELIMITED BY ","
                           INTO WS-Q-ID WS-Q-TEXT WS-Q-SCORE
                           
                       ADD 1 TO WS-Q-COUNT
                       MOVE WS-Q-TEXT TO WS-QUESTION OF
                        WS-Q-ENTRY(WS-Q-COUNT)
                       MOVE FUNCTION NUMVAL(WS-Q-SCORE) 
                            TO WS-SCORE OF WS-Q-ENTRY(WS-Q-COUNT)
               END-READ
           END-PERFORM.
           CLOSE QUESTION-FILE.
           
           MOVE 0 TO LS-TOTAL-SCORE.
           *> (2) Dynamic Loop based on actual number of questions (WS-Q-COUNT)
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-Q-COUNT
               MOVE "N" TO WS-VALID
               PERFORM UNTIL WS-VALID = "Y"
                   
                   *> Display the question dynamically
                   DISPLAY FUNCTION TRIM(WS-QUESTION 
                   OF WS-Q-ENTRY(WS-IDX))
                   ACCEPT WS-TEMP-ANS
                   
                   *> Convert to Uppercase
                   MOVE FUNCTION UPPER-CASE(WS-TEMP-ANS) TO WS-TEMP-ANS
                   
                   IF WS-TEMP-ANS = "Y" OR WS-TEMP-ANS = "N"
                       MOVE "Y" TO WS-VALID
                       *> Save answer to the correct index in Linkage Array
                       MOVE WS-TEMP-ANS TO LS-DEC-ANS(WS-IDX)
                       
                       *> Add score if answer is "Y"
                       IF WS-TEMP-ANS = "Y"
                           ADD WS-SCORE OF WS-Q-ENTRY(WS-IDX) 
                           TO LS-TOTAL-SCORE
                       END-IF

                   ELSE
                       DISPLAY "[Error] Please enter Y or N."
                   END-IF
                   
               END-PERFORM
           END-PERFORM.

           EXIT PROGRAM.