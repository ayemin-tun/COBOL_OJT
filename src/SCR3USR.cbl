       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCR3USR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL USER-FILE ASSIGN TO "result/user.csv"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS WS-FILE-STATUS.
               
           SELECT OPTIONAL DEVICE-FILE ASSIGN TO "result/device.csv"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS WS-DEV-STATUS.

           SELECT OPTIONAL PLAN-FILE ASSIGN TO "result/plan.csv"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS WS-DEV-STATUS.

           *> --- Added: Temp File for deleting last record ---
           SELECT OPTIONAL TEMP-FILE ASSIGN TO "result/temp.csv"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS WS-TEMP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-REC                   PIC X(200).
       
       FD  DEVICE-FILE.
       01  DEVICE-REC                 PIC X(120).

       FD  PLAN-FILE.
       01  PLAN-REC                   PIC X(120).

       *> --- Added: Temp File Record ---
       FD  TEMP-FILE.
       01  TEMP-REC                   PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC XX.
       01  WS-DEV-STATUS              PIC XX.
       01  WS-VALID                   PIC X.

       *> --- Added: Variables for choice and temp file ---
       01  WS-TEMP-STATUS             PIC XX.
       01  WS-CHOICE                  PIC X.
       01  WS-TEMP-EOF                PIC X.

      *japanese character count
       01  WS-PHONE-I         PIC 99.
       01  WS-CHAR            PIC X.
       01  WS-INVALID         PIC X.
       01  WS-PHONE-LEN-J     PIC 99.
       01  WS-JAPANESE-FOUND  PIC X(01) VALUE "N".
       01  WS-TEMP-PHONE     PIC X(15) VALUE SPACES.
       01  WS-EMAIL-TALLY    PIC 99.
       
       01  WS-TEMP-ADDRESS            PIC X(100).
       01  WS-PHONE-LEN               PIC 9(2).
       01  WS-POSTAL-LEN              PIC 9(2).
       01  WS-I                       PIC 9(3).
       01  WS-CHAR-CHECK              PIC X.
       01  WS-IS-INVALID-CHAR         PIC X.

       01  WS-AT-COUNT                PIC 9(2).
       01  WS-AT-POS                  PIC 9(2).
       01  WS-EMAIL-LEN               PIC 9(2).
       *> --- အသစ်ထပ်တိုးထားသော Variable ---
       01  WS-AT-DOT-COUNT            PIC 9(2).

       01  WS-USER-EOF                PIC X.
       01  WS-DEV-EOF                 PIC X.
       01  WS-IS-ACTIVE-FOUND         PIC X.
       
       01  WS-READ-U-APPID            PIC X(5).
       01  WS-READ-U-NAME             PIC X(30).
       01  WS-READ-U-EMAIL            PIC X(30).
       01  WS-READ-U-PHONE            PIC X(15).
       01  WS-READ-U-POSTAL           PIC X(10).
       01  WS-READ-U-ADDRESS          PIC X(50).
       01  WS-READ-U-DOB              PIC X(10).
       01  WS-READ-U-REGDATE          PIC X(10).
       
       01  WS-READ-D-APPID            PIC X(5).
       01  WS-READ-D-TYPE             PIC X(20).
       01  WS-READ-D-MODEL            PIC X(30).
       01  WS-READ-D-PRICE            PIC X(15).
       01  WS-READ-D-DATE             PIC X(10).
       01  WS-READ-D-PERIOD           PIC X(3).
       01  WS-READ-D-STATUS           PIC X(10).
       
       01  WS-READ-P-APPID            PIC X(4).
       01  WS-READ-P-PLANNAME         PIC X(10).
       01  WS-READ-P-PERIOD           PIC 9(2).
       01  WS-READ-P-PREMIUM          PIC 9(6).
       01  WS-READ-P-ANSWER           PIC X(20).
       01  WS-READ-P-STATUS           PIC X(10).
       01  WS-READ-P-SCORE            PIC 9(3).
       01  WS-IS-REJECTED             PIC X(01) VALUE 'N'.
       01  WS-PLAN-EOF                PIC X(01) VALUE 'N'.
       01  WS-PLAN-STATUS             PIC X(02).

       01  WS-EXP-YYYY                PIC 9(4).
       01  WS-EXP-MM                  PIC 9(2).
       01  WS-EXP-DD                  PIC 9(2).
       01  WS-EXP-DATE-NUM            PIC 9(8).
       01  WS-PERIOD-YRS              PIC 9(2).

       01  WS-DOB-VARS.
           05 WS-DOB-YY               PIC 9(4).
           05 WS-DOB-MM               PIC 9(2).
           05 WS-DOB-DD               PIC 9(2).
       01  WS-MAX-DAYS                PIC 9(2).
       01  WS-REM-4                   PIC 9(4).
       01  WS-REM-100                 PIC 9(4).
       01  WS-REM-400                 PIC 9(4).
       01  WS-QUOT                    PIC 9(4).
       01  WS-CURRENT-DATE            PIC 9(8).
       01  WS-CURRENT-DATE-STR        PIC X(8).
       01  WS-CURRENT-YEAR            PIC 9(4).
       01  WS-CALCULATED-AGE          PIC 9(3).

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
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-REG-DATE           PIC X(10).

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
           05 LS-DEC-1                PIC X.
           05 LS-DEC-2                PIC X.
           05 LS-DEC-3                PIC X.
           05 LS-DEC-4                PIC X.
           05 LS-STATUS               PIC X(10).
           05 FILLER                  PIC X(20).
           
       PROCEDURE DIVISION USING LS-APP-DATA.
           DISPLAY " ".
           DISPLAY "--- SCREEN 3: APPLICANT INFORMATION ---".
           
           *> --- Applicant Name ---
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
              MOVE SPACES TO LS-USER-NAME
              DISPLAY "Enter Applicant Name: "
              ACCEPT LS-USER-NAME
              IF LS-USER-NAME = SPACES
                 DISPLAY "[Error] Name cannot be empty."
              ELSE
                 MOVE "Y" TO WS-VALID
              END-IF
           END-PERFORM.

           *> --- Email Validation & Expiry Check ---
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
              MOVE SPACES TO LS-USER-EMAIL  
              
              DISPLAY "Enter Email Address: "
              ACCEPT LS-USER-EMAIL
              
              IF FUNCTION TRIM(LS-USER-EMAIL) = SPACES
                 DISPLAY "[Error] Email cannot be empty."
              ELSE
                 MOVE "N" TO WS-INVALID
                 MOVE 0 TO WS-EMAIL-TALLY
                 
                 INSPECT LS-USER-EMAIL TALLYING WS-EMAIL-TALLY
                          FOR ALL X"EF" ALL X"E3"
                 
                 IF WS-EMAIL-TALLY > 0
                    DISPLAY "Japanese characters are not allowed."
                    MOVE "Y" TO WS-INVALID
                 END-IF
                 
                 IF WS-INVALID = "N"
                    PERFORM VALIDATE-EMAIL-AND-CHECK-EXPIRY
                 ELSE
                    MOVE SPACES TO LS-USER-EMAIL
                 END-IF
              END-IF
           END-PERFORM.


           *> --- Phone Number Validation ---
           MOVE "N" TO WS-VALID
           PERFORM UNTIL WS-VALID = "Y"
              MOVE SPACES TO LS-USER-PHONE
              MOVE SPACES TO WS-TEMP-PHONE
              
              DISPLAY "Enter Phone Number (10-11 digits): "
              ACCEPT LS-USER-PHONE
                             
              IF FUNCTION TRIM(LS-USER-PHONE) = SPACES
                 DISPLAY "[Error] Phone cannot be empty."
              ELSE
                 MOVE "N" TO WS-INVALID
                 MOVE 0 TO WS-PHONE-LEN-J
                 
                 INSPECT LS-USER-PHONE TALLYING WS-PHONE-LEN-J
                          FOR ALL X"EF" ALL X"E3"
                 
                 IF WS-PHONE-LEN-J > 0
                    DISPLAY "Err : Japanese characters are not allowed."
                    MOVE "Y" TO WS-INVALID
                 ELSE
                    IF FUNCTION TRIM(LS-USER-PHONE) NOT NUMERIC
                       DISPLAY "Phone must contain only numbers (0-9)."
                       DISPLAY "Letters or symbols are not allowed."
                       MOVE "Y" TO WS-INVALID
                    ELSE
                       MOVE FUNCTION LENGTH
                       (FUNCTION TRIM(LS-USER-PHONE)) 
                         TO WS-PHONE-LEN-J
                         
                       IF WS-PHONE-LEN-J < 10 OR WS-PHONE-LEN-J > 11
                          DISPLAY "Phone must be  10 or 11 digits."
                          MOVE "Y" TO WS-INVALID
                       END-IF
                    END-IF
                 END-IF
                 
                 IF WS-INVALID = "Y"
                    MOVE SPACES TO LS-USER-PHONE
                 ELSE
                    MOVE "Y" TO WS-VALID
                 END-IF
              END-IF
           END-PERFORM.

           
           *> --- Postal Code Validation ---
           MOVE "N" TO WS-VALID
           PERFORM UNTIL WS-VALID = "Y"
              MOVE SPACES TO LS-USER-POSTAL
              
              DISPLAY "Enter Postal Code (5 to 7 digits): "
              ACCEPT LS-USER-POSTAL
           
                IF FUNCTION TRIM(LS-USER-POSTAL) = SPACES
              DISPLAY "[Error] Postal Code cannot be empty."
           ELSE
              MOVE "N" TO WS-INVALID
              
              MOVE 0 TO WS-POSTAL-LEN
              INSPECT LS-USER-POSTAL TALLYING WS-POSTAL-LEN
                       FOR ALL X"EF" ALL X"E3"
              
              IF WS-POSTAL-LEN > 0
                 DISPLAY "Error : Japanese numbers are not allowed."
                 MOVE "Y" TO WS-INVALID
              END-IF
           
              IF WS-INVALID NOT = "Y"
                 IF FUNCTION TRIM(LS-USER-POSTAL) NOT NUMERIC
                     DISPLAY "Postal must contain only numbers (0-9)."
                     DISPLAY "Err : Letters or symbols are not allowed."
                     MOVE "Y" TO WS-INVALID
                 END-IF
              END-IF
           
              IF WS-INVALID NOT = "Y"
                 MOVE FUNCTION LENGTH(FUNCTION TRIM(LS-USER-POSTAL)) 
                    TO WS-POSTAL-LEN
                    
                 IF WS-POSTAL-LEN < 5 OR WS-POSTAL-LEN > 7
                     DISPLAY "Error : Postal code length is wrong."
                     DISPLAY "Error : It must be exactly 5 to 7 digits."
                     MOVE "Y" TO WS-INVALID
                 END-IF
              END-IF
           
              IF WS-INVALID = "N"
                 MOVE "Y" TO WS-VALID
              ELSE
                 MOVE SPACES TO LS-USER-POSTAL
              END-IF
                END-IF
           END-PERFORM.

           *> --- Address Validation ---
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
              MOVE SPACES TO WS-TEMP-ADDRESS
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

           *> --- Date of Birth Validation ---
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
              MOVE SPACES TO LS-USER-DOB
              DISPLAY "Enter Date of Birth (YYYY-MM-DD): "
              ACCEPT LS-USER-DOB
               
              IF LS-USER-DOB = SPACES
                 DISPLAY "[Error] Date of Birth cannot be empty."
              ELSE
                 PERFORM VALIDATE-DOB
              END-IF
           END-PERFORM.

           *> --- Format System Date ---
           ACCEPT WS-CURRENT-DATE-STR FROM DATE YYYYMMDD.
           MOVE WS-CURRENT-DATE-STR(1:4) TO WS-F-REG-DATE(1:4).
           MOVE "-" TO WS-F-REG-DATE(5:1).
           MOVE WS-CURRENT-DATE-STR(5:2) TO WS-F-REG-DATE(6:2).
           MOVE "-" TO WS-F-REG-DATE(8:1).
           MOVE WS-CURRENT-DATE-STR(7:2) TO WS-F-REG-DATE(9:2).

           *> --- Write Data to user.csv ---
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
              "Email               ,Phone          ," &
              "Postal    ,Address                                     "&
              "     ,DOB       ,PolicyDate"
           ELSE
              CLOSE USER-FILE
              OPEN EXTEND USER-FILE
           END-IF.

           WRITE USER-REC FROM WS-FORMATTED-REC.
           CLOSE USER-FILE.

           EXIT PROGRAM.

       *> =========================================================
       *> PARAGRAPHS FOR NESTED LOGIC
       *> =========================================================
       
       VALIDATE-EMAIL-AND-CHECK-EXPIRY.
           MOVE 0 TO WS-AT-COUNT.
           MOVE 0 TO WS-AT-POS.
           MOVE 0 TO WS-EMAIL-LEN.
           MOVE 0 TO WS-AT-DOT-COUNT.
           MOVE "N" TO WS-IS-INVALID-CHAR.
           
           *> 1. Character Check
           MOVE 20 TO WS-EMAIL-LEN.
           PERFORM VARYING WS-I FROM 1 BY 1 
              UNTIL WS-I > 20 OR LS-USER-EMAIL(WS-I:1) = SPACE
              MOVE LS-USER-EMAIL(WS-I:1) TO WS-CHAR-CHECK
              IF NOT ( (WS-CHAR-CHECK >= "a" AND WS-CHAR-CHECK <= "z")
                 OR (WS-CHAR-CHECK >= "A" AND WS-CHAR-CHECK <= "Z")
                 OR (WS-CHAR-CHECK >= "0" AND WS-CHAR-CHECK <= "9")
                 OR WS-CHAR-CHECK = "@" OR WS-CHAR-CHECK = "."
                 OR WS-CHAR-CHECK = "_" OR WS-CHAR-CHECK = "-" )
                 MOVE "Y" TO WS-IS-INVALID-CHAR
              END-IF
           END-PERFORM.
           
           PERFORM VARYING WS-I FROM 20 BY -1 
              UNTIL WS-I = 0 OR LS-USER-EMAIL(WS-I:1) NOT = SPACE
              SUBTRACT 1 FROM WS-EMAIL-LEN
           END-PERFORM.
           
           IF WS-IS-INVALID-CHAR = "Y"
              DISPLAY "[Error] Only English alphabets and numbers."
           ELSE IF WS-EMAIL-LEN < 7 OR WS-EMAIL-LEN > 20
              DISPLAY "[Error] Email length must be 7 to 20 chars."
           ELSE
              INSPECT LS-USER-EMAIL TALLYING WS-AT-COUNT FOR ALL "@"
              *> --- @. ပါမပါ စစ်ဆေးခြင်း ---
              INSPECT LS-USER-EMAIL TALLYING WS-AT-DOT-COUNT 
                                             FOR ALL "@."
              
              IF WS-AT-COUNT = 0
                 DISPLAY "[Error] Write with real email format."
              ELSE
                 *> --- @. တွေ့ရှိပါက Error ပြခြင်း ---
                 IF WS-AT-DOT-COUNT > 0
                    DISPLAY "[Error] Invalid format ('@.' not allowed)."
                 ELSE
                    INSPECT LS-USER-EMAIL TALLYING WS-AT-POS 
                       FOR CHARACTERS BEFORE INITIAL "@"
                    IF WS-AT-POS < 2
                       DISPLAY "[Error] At least 2 chars before '@'."
                    ELSE
                       PERFORM CHECK-DUPLICATE-EXPIRY
                    END-IF
                 END-IF
              END-IF
           END-IF.

       CHECK-DUPLICATE-EXPIRY.
           MOVE "N" TO WS-IS-ACTIVE-FOUND.
           OPEN INPUT USER-FILE.
           IF WS-FILE-STATUS NOT = "35"
              MOVE "N" TO WS-USER-EOF
              PERFORM UNTIL WS-USER-EOF = "Y"
                 READ USER-FILE INTO USER-REC
                 AT END
                    MOVE "Y" TO WS-USER-EOF
                 NOT AT END
                    UNSTRING USER-REC DELIMITED BY ","
                       INTO WS-READ-U-APPID WS-READ-U-NAME
                            WS-READ-U-EMAIL WS-READ-U-PHONE
                            WS-READ-U-POSTAL WS-READ-U-ADDRESS
                            WS-READ-U-DOB WS-READ-U-REGDATE
                    
                    IF FUNCTION TRIM(WS-READ-U-EMAIL) = 
                       FUNCTION TRIM(LS-USER-EMAIL)
                       
                       PERFORM CHECK-PLAN-FILE
                       
                       IF WS-IS-REJECTED = "N"
                          PERFORM CHECK-DEVICE-FILE
                       END-IF
                       
                    END-IF
                 END-READ
              END-PERFORM
              CLOSE USER-FILE
           END-IF.
           
           *> --- UPDATED: User Prompt and Deletion Logic ---
           IF WS-IS-ACTIVE-FOUND = "Y"
              DISPLAY " "
              DISPLAY "[Alert] This Email has an ACTIVE policy."
              DISPLAY "Enter 'Y' to use a different email address."
              DISPLAY "Enter 'N' to delete your current entry and exit."
              DISPLAY "Choice (Y/N): "
              ACCEPT WS-CHOICE
              
              IF WS-CHOICE = 'Y' OR 'y'
                 MOVE SPACES TO LS-USER-EMAIL
                 MOVE "N" TO WS-VALID
              ELSE
                 PERFORM DELETE-LAST-RECORD
                 DISPLAY "Your current entry has been removed."
                 STOP RUN
              END-IF
           ELSE
              MOVE "Y" TO WS-VALID
           END-IF.

       *> =========================================================
       *> DELETE-LAST-RECORD 
       *> Deletes ONLY the records matching the current LS-APP-ID
       *> =========================================================
       DELETE-LAST-RECORD.
           *> --- Clean DEVICE-FILE ---
           MOVE "N" TO WS-DEV-EOF.
           OPEN INPUT DEVICE-FILE.
           OPEN OUTPUT TEMP-FILE.
           PERFORM UNTIL WS-DEV-EOF = "Y"
              READ DEVICE-FILE INTO DEVICE-REC
              AT END
                 MOVE "Y" TO WS-DEV-EOF
              NOT AT END
                 UNSTRING DEVICE-REC DELIMITED BY "," 
                    INTO WS-READ-D-APPID
                 IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-READ-D-APPID)) 
                    = "APPID"
                    OR FUNCTION TRIM(WS-READ-D-APPID) NOT = 
                       FUNCTION TRIM(LS-APP-ID)
                    WRITE TEMP-REC FROM DEVICE-REC
                 END-IF
              END-READ
           END-PERFORM.
           CLOSE DEVICE-FILE TEMP-FILE.

           MOVE "N" TO WS-TEMP-EOF.
           OPEN INPUT TEMP-FILE.
           OPEN OUTPUT DEVICE-FILE.
           PERFORM UNTIL WS-TEMP-EOF = "Y"
              READ TEMP-FILE INTO TEMP-REC
              AT END
                 MOVE "Y" TO WS-TEMP-EOF
              NOT AT END
                 WRITE DEVICE-REC FROM TEMP-REC
              END-READ
           END-PERFORM.
           CLOSE TEMP-FILE DEVICE-FILE.

           *> --- Clean USER-FILE (If any partial data was saved) ---
           MOVE "N" TO WS-USER-EOF.
           OPEN INPUT USER-FILE.
           OPEN OUTPUT TEMP-FILE.
           PERFORM UNTIL WS-USER-EOF = "Y"
              READ USER-FILE INTO USER-REC
              AT END
                 MOVE "Y" TO WS-USER-EOF
              NOT AT END
                 UNSTRING USER-REC DELIMITED BY "," 
                    INTO WS-READ-U-APPID
                 IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-READ-U-APPID)) 
                    = "APPID"
                    OR FUNCTION TRIM(WS-READ-U-APPID) NOT = 
                       FUNCTION TRIM(LS-APP-ID)
                    WRITE TEMP-REC FROM USER-REC
                 END-IF
              END-READ
           END-PERFORM.
           CLOSE USER-FILE TEMP-FILE.

           MOVE "N" TO WS-TEMP-EOF.
           OPEN INPUT TEMP-FILE.
           OPEN OUTPUT USER-FILE.
           PERFORM UNTIL WS-TEMP-EOF = "Y"
              READ TEMP-FILE INTO TEMP-REC
              AT END
                 MOVE "Y" TO WS-TEMP-EOF
              NOT AT END
                 WRITE USER-REC FROM TEMP-REC
              END-READ
           END-PERFORM.
           CLOSE TEMP-FILE USER-FILE.

           *> --- Clean PLAN-FILE (If any partial data was saved) ---
           MOVE "N" TO WS-PLAN-EOF.
           OPEN INPUT PLAN-FILE.
           IF WS-DEV-STATUS NOT = "35"
              OPEN OUTPUT TEMP-FILE
              PERFORM UNTIL WS-PLAN-EOF = "Y"
                 READ PLAN-FILE INTO PLAN-REC
                 AT END
                    MOVE "Y" TO WS-PLAN-EOF
                 NOT AT END
                    UNSTRING PLAN-REC DELIMITED BY "," 
                       INTO WS-READ-P-APPID
                    IF FUNCTION UPPER-CASE
                       (FUNCTION TRIM(WS-READ-P-APPID)) 
                       = "APPID"
                       OR FUNCTION TRIM(WS-READ-P-APPID) NOT = 
                          FUNCTION TRIM(LS-APP-ID)
                       WRITE TEMP-REC FROM PLAN-REC
                    END-IF
                 END-READ
              END-PERFORM
              CLOSE PLAN-FILE TEMP-FILE

              MOVE "N" TO WS-TEMP-EOF
              OPEN INPUT TEMP-FILE
              OPEN OUTPUT PLAN-FILE
              PERFORM UNTIL WS-TEMP-EOF = "Y"
                 READ TEMP-FILE INTO TEMP-REC
                 AT END
                    MOVE "Y" TO WS-TEMP-EOF
                 NOT AT END
                    WRITE PLAN-REC FROM TEMP-REC
                 END-READ
              END-PERFORM
              CLOSE TEMP-FILE PLAN-FILE
           END-IF.


       CHECK-PLAN-FILE.
           MOVE "N" TO WS-IS-REJECTED.
           OPEN INPUT PLAN-FILE.
           IF WS-DEV-STATUS NOT = "35"
              MOVE "N" TO WS-PLAN-EOF
              PERFORM UNTIL WS-PLAN-EOF = "Y"
                 READ PLAN-FILE INTO PLAN-REC
                 AT END
                    MOVE "Y" TO WS-PLAN-EOF
                 NOT AT END
                    UNSTRING PLAN-REC DELIMITED BY ","
                       INTO WS-READ-P-APPID WS-READ-P-PLANNAME
                            WS-READ-P-PERIOD WS-READ-P-PREMIUM
                            WS-READ-P-ANSWER WS-READ-P-STATUS
                            WS-READ-P-SCORE
                    
                    IF FUNCTION TRIM(WS-READ-P-APPID) = 
                       FUNCTION TRIM(WS-READ-U-APPID)
                       
                       *> Status သည် Reject ဖြစ်နေလျှင်
                       IF FUNCTION TRIM(WS-READ-P-STATUS) = "REJECT"
                       OR FUNCTION TRIM(WS-READ-P-STATUS) = "REJECTED"
                          MOVE "Y" TO WS-IS-REJECTED
                       END-IF
                       
                       MOVE "Y" TO WS-PLAN-EOF
                    END-IF
                 END-READ
              END-PERFORM
              CLOSE PLAN-FILE
           END-IF.


       CHECK-DEVICE-FILE.
           OPEN INPUT DEVICE-FILE.
           IF WS-DEV-STATUS NOT = "35"
              MOVE "N" TO WS-DEV-EOF
              PERFORM UNTIL WS-DEV-EOF = "Y"
                 READ DEVICE-FILE INTO DEVICE-REC
                 AT END
                    MOVE "Y" TO WS-DEV-EOF
                 NOT AT END
                    UNSTRING DEVICE-REC DELIMITED BY ","
                       INTO WS-READ-D-APPID WS-READ-D-TYPE
                            WS-READ-D-MODEL WS-READ-D-PRICE
                            WS-READ-D-DATE WS-READ-D-PERIOD
                    
                    IF FUNCTION TRIM(WS-READ-D-APPID) = 
                       FUNCTION TRIM(WS-READ-U-APPID)
                       
                       PERFORM CALCULATE-EXPIRY
                       MOVE "Y" TO WS-DEV-EOF
                    END-IF
                 END-READ
              END-PERFORM
              CLOSE DEVICE-FILE
           END-IF.

       CALCULATE-EXPIRY.
           IF WS-READ-U-REGDATE(5:1) = "-"
              MOVE WS-READ-U-REGDATE(1:4) TO WS-EXP-YYYY
              MOVE WS-READ-U-REGDATE(6:2) TO WS-EXP-MM
              MOVE WS-READ-U-REGDATE(9:2) TO WS-EXP-DD
              
              COMPUTE WS-PERIOD-YRS = 
                 FUNCTION NUMVAL(WS-READ-D-PERIOD) / 12
              ADD WS-PERIOD-YRS TO WS-EXP-YYYY
              
              COMPUTE WS-EXP-DATE-NUM = 
                 (WS-EXP-YYYY * 10000) + 
                 (WS-EXP-MM * 100) + WS-EXP-DD
              
              ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
              
              IF WS-EXP-DATE-NUM >= WS-CURRENT-DATE
                 MOVE "Y" TO WS-IS-ACTIVE-FOUND
                 MOVE "Y" TO WS-USER-EOF
              END-IF
           END-IF.

       VALIDATE-DOB.
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
                       IF WS-REM-400=0 OR 
                           (WS-REM-4=0 AND WS-REM-100 NOT=0)
                          MOVE 29 TO WS-MAX-DAYS
                       ELSE
                          MOVE 28 TO WS-MAX-DAYS
                       END-IF
                    WHEN OTHER
                       MOVE 31 TO WS-MAX-DAYS
                 END-EVALUATE

                 IF WS-DOB-DD < 1 OR WS-DOB-DD > WS-MAX-DAYS
                    DISPLAY "[Error] Invalid Day for month."
                    MOVE "N" TO WS-VALID
                 END-IF
                 
                 IF WS-VALID = "Y"
                    ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
                    COMPUTE WS-CURRENT-YEAR = WS-CURRENT-DATE / 10000
                    COMPUTE WS-CALCULATED-AGE = 
                       WS-CURRENT-YEAR - WS-DOB-YY
                    
                    IF WS-CALCULATED-AGE < 10 OR WS-CALCULATED-AGE > 100
                       DISPLAY "[Error] Age must be 10 to 100 years."
                       MOVE "N" TO WS-VALID
                    END-IF
                 END-IF
              END-IF
           ELSE
              DISPLAY "[Error] Invalid format. Use YYYY-MM-DD."
           END-IF.