       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCR3USR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL USER-FILE ASSIGN TO "user.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-REC                   PIC X(180).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC XX.
       01  WS-VALID                   PIC X.
       
       *> Added for length validations
       01  WS-TEMP-ADDRESS            PIC X(100).
       01  WS-PHONE-LEN               PIC 9(2).
       01  WS-POSTAL-LEN              PIC 9(2).
       01  WS-I                       PIC 9(3).

       *> Added for Email Validation
       01  WS-AT-COUNT                PIC 9(2).
       01  WS-AT-POS                  PIC 9(2).

       *> Added for DOB Validation
       01  WS-DOB-VARS.
           05 WS-DOB-YY               PIC 9(4).
           05 WS-DOB-MM               PIC 9(2).
           05 WS-DOB-DD               PIC 9(2).
       01  WS-MAX-DAYS                PIC 9(2).
       01  WS-REM-4                   PIC 9(4).
       01  WS-REM-100                 PIC 9(4).
       01  WS-REM-400                 PIC 9(4).
       01  WS-QUOT                    PIC 9(4).

       01  WS-FORMATTED-REC.
           05 WS-F-APP-ID             PIC 9(4).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-NAME               PIC X(30).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-EMAIL              PIC X(30).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-PHONE              PIC X(15).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-POSTAL             PIC X(10).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-ADDRESS            PIC X(50).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-DOB                PIC X(10).

      
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
           DISPLAY "--- SCREEN 3: APPLICANT INFORMATION ---".
           
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "Enter Applicant Name: "
               ACCEPT LS-USER-NAME
               IF LS-USER-NAME = SPACES
                   DISPLAY "[Error] Name cannot be empty."
               ELSE
                   MOVE "Y" TO WS-VALID
               END-IF
           END-PERFORM.

           *> Email Validation
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               
               MOVE SPACES TO LS-USER-EMAIL  
               
               DISPLAY "Enter Email Address: "
               ACCEPT LS-USER-EMAIL
               IF LS-USER-EMAIL = SPACES
                   DISPLAY "[Error] Email cannot be empty."
               ELSE
                   MOVE 0 TO WS-AT-COUNT
                   MOVE 0 TO WS-AT-POS
                   
                   *> Check if '@' exists
                   INSPECT LS-USER-EMAIL TALLYING WS-AT-COUNT 
                           FOR ALL "@"
                   
                   IF WS-AT-COUNT = 0
                       DISPLAY "[Error] Email must contain '@' sign."
                   ELSE
                       *> Count characters before '@'
                       INSPECT LS-USER-EMAIL TALLYING WS-AT-POS 
                               FOR CHARACTERS BEFORE INITIAL "@"
                       
                       IF WS-AT-POS < 2
                           DISPLAY "[Error] At least 2 characters " &
                                   "needed before '@'."
                       ELSE
                           MOVE "Y" TO WS-VALID
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

           *> Phone Number Validation (10-11 characters)
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "Enter Phone Number (10 to 11 digits): "
               ACCEPT LS-USER-PHONE
               IF LS-USER-PHONE = SPACES
                   DISPLAY "[Error] Phone cannot be empty."
               ELSE
                   MOVE 15 TO WS-PHONE-LEN
                   PERFORM VARYING WS-I FROM 15 BY -1 
                     UNTIL WS-I = 0 OR LS-USER-PHONE(WS-I:1) NOT = SPACE
                       SUBTRACT 1 FROM WS-PHONE-LEN
                   END-PERFORM
                   
                   IF WS-PHONE-LEN < 10 OR WS-PHONE-LEN > 11
                       DISPLAY "[Error] Phone must be 10-11 characters."
                   ELSE
                       MOVE "Y" TO WS-VALID
                   END-IF
               END-IF
           END-PERFORM.

           *> Postal Code Validation (5-7 characters)
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "Enter Postal Code (5 to 7 characters): "
               ACCEPT LS-USER-POSTAL
               IF LS-USER-POSTAL = SPACES
                   DISPLAY "[Error] Postal Code cannot be empty."
               ELSE
                   MOVE 10 TO WS-POSTAL-LEN
                   PERFORM VARYING WS-I FROM 10 BY -1 
                     UNTIL WS-I = 0 OR LS-USER-POSTAL(WS-I:1) NOT =SPACE
                       SUBTRACT 1 FROM WS-POSTAL-LEN
                   END-PERFORM
                   
                   IF WS-POSTAL-LEN < 5 OR WS-POSTAL-LEN > 7
                     DISPLAY"[Error]Postal Code must be 5-7 characters."
                   ELSE
                       MOVE "Y" TO WS-VALID
                   END-IF
               END-IF
           END-PERFORM.

           *> Address Validation (Alert if > 50 chars)
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "Enter Address: "
               ACCEPT WS-TEMP-ADDRESS
               IF WS-TEMP-ADDRESS = SPACES
                   DISPLAY "[Error] Address cannot be empty."
               ELSE
                   IF WS-TEMP-ADDRESS(51:50) NOT = SPACES
                       DISPLAY "[Alert] I cut only 50 words"
                   END-IF
                   MOVE WS-TEMP-ADDRESS(1:50) TO LS-USER-ADDRESS
                   MOVE "Y" TO WS-VALID
               END-IF
           END-PERFORM.

           *> Date of Birth Validation (YYYY-MM-DD)
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               DISPLAY "Enter Date of Birth (YYYY-MM-DD): "
               ACCEPT LS-USER-DOB
               IF LS-USER-DOB = SPACES
                   DISPLAY "[Error] Date of Birth cannot be empty."
               ELSE
                   IF LS-USER-DOB(5:1) = "-" AND LS-USER-DOB(8:1) = "-"
                      AND LS-USER-DOB(1:4) IS NUMERIC
                      AND LS-USER-DOB(6:2) IS NUMERIC
                      AND LS-USER-DOB(9:2) IS NUMERIC
                       
                       MOVE LS-USER-DOB(1:4) TO WS-DOB-YY
                       MOVE LS-USER-DOB(6:2) TO WS-DOB-MM
                       MOVE LS-USER-DOB(9:2) TO WS-DOB-DD
                       MOVE "Y" TO WS-VALID

                       IF WS-DOB-YY < 1000 OR WS-DOB-YY > 9999
                           DISPLAY "[Error] Year must be 4 digits."
                           MOVE "N" TO WS-VALID
                       END-IF

                       IF WS-DOB-MM < 1 OR WS-DOB-MM > 12
                           DISPLAY "[Error] Month must be 1 to 12."
                           MOVE "N" TO WS-VALID
                       END-IF

                       IF WS-VALID = "Y"
                           *> Check Leap Year for February
                           DIVIDE WS-DOB-YY BY 4 GIVING WS-QUOT 
                               REMAINDER WS-REM-4
                           DIVIDE WS-DOB-YY BY 100 GIVING WS-QUOT 
                               REMAINDER WS-REM-100
                           DIVIDE WS-DOB-YY BY 400 GIVING WS-QUOT 
                               REMAINDER WS-REM-400

                           EVALUATE WS-DOB-MM
                               WHEN 4 WHEN 6 WHEN 9 WHEN 11
                                   MOVE 30 TO WS-MAX-DAYS
                               WHEN 2
                                   IF WS-REM-400 = 0 OR 
                                       (WS-REM-4 = 0 AND WS-REM-100 
                                       NOT = 0)
                                       MOVE 29 TO WS-MAX-DAYS
                                   ELSE
                                       MOVE 28 TO WS-MAX-DAYS
                                   END-IF
                               WHEN OTHER
                                   MOVE 31 TO WS-MAX-DAYS
                           END-EVALUATE

                           IF WS-DOB-DD < 1 OR WS-DOB-DD > WS-MAX-DAYS
                               DISPLAY "[Error] Invalid Day for the" &
                                "given month."
                               MOVE "N" TO WS-VALID
                           END-IF
                       END-IF
                   ELSE
                       DISPLAY "[Error] Invalid format. Use YYYY-MM-DD."
                   END-IF
               END-IF
           END-PERFORM.

           *> Writing to user.csv
           MOVE LS-APP-ID TO WS-F-APP-ID.
           MOVE LS-USER-NAME TO WS-F-NAME.
           MOVE LS-USER-EMAIL TO WS-F-EMAIL.
           MOVE LS-USER-PHONE TO WS-F-PHONE.
           MOVE LS-USER-POSTAL TO WS-F-POSTAL.
           MOVE LS-USER-ADDRESS TO WS-F-ADDRESS.
           MOVE LS-USER-DOB TO WS-F-DOB.

           OPEN INPUT USER-FILE.
           IF WS-FILE-STATUS = "35"
               OPEN OUTPUT USER-FILE
               WRITE USER-REC FROM 
               "AppID,ApplicantName                ," &
               "Email                          ,Phone          ," &
               "Postal    ,Address                                    "&
                ",DOB"
           ELSE
               CLOSE USER-FILE
               OPEN EXTEND USER-FILE
           END-IF.

           WRITE USER-REC FROM WS-FORMATTED-REC.
           CLOSE USER-FILE.

           EXIT PROGRAM.