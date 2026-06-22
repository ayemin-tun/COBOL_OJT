       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCR1DEV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL DEVICE-FILE ASSIGN TO "result/device.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
               
           SELECT OPTIONAL MODEL-FILE ASSIGN TO "data/models.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MODEL-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  DEVICE-FILE.
       01  DEVICE-REC                 PIC X(120).
       
       FD  MODEL-FILE.
       01  MODEL-REC                  PIC X(60).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC XX.
       01  WS-MODEL-STATUS            PIC XX.
       01  WS-EOF                     PIC X VALUE "N".
       01  WS-EOF-MODEL               PIC X VALUE "N".
       
       01  WS-LAST-ID                 PIC 9(4) VALUE 1000.
       01  WS-NEW-ID                  PIC 9(4).

       01  WS-VALID                   PIC X.
       01  WS-TEMP-PRICE              PIC X(10).
       01  WS-TEMP-PERIOD             PIC X(3).

       *> Variables for Model Selection
       01  WS-IN-TYPE                 PIC X(10).
       01  WS-IN-NUM                  PIC X(2).
       01  WS-IN-NAME                 PIC X(30).
       01  WS-USER-CHOICE             PIC 9(2).
       01  WS-CHOICE-STR              PIC X(2).
       01  WS-FOUND                   PIC X.
       
       *> Array to store filtered models temporarily
       01  WS-MODEL-TABLE.
           05 WS-M-ENTRY OCCURS 20 TIMES INDEXED BY M-IDX.
              10 WS-M-NUM             PIC 9(2).
              10 WS-M-NAME            PIC X(30).
       01  WS-MODEL-COUNT             PIC 9(2) VALUE 0.

       *> Added for Date Validation
       01  WS-DATE-VARS.
           05 WS-DATE-YY              PIC 9(4).
           05 WS-DATE-MM              PIC 9(2).
           05 WS-DATE-DD              PIC 9(2).
       01  WS-MAX-DAYS                PIC 9(2).
       01  WS-REM-4                   PIC 9(4).
       01  WS-REM-100                 PIC 9(4).
       01  WS-REM-400                 PIC 9(4).
       01  WS-QUOT                    PIC 9(4).
       
       *> Added for Future Date Check
       01  WS-SYS-DATE                PIC 9(8).
       01  WS-INPUT-DATE-NUM          PIC 9(8).

       01  WS-FORMATTED-REC.
           05 WS-F-APP-ID             PIC X(5).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-DEVICE-TYPE        PIC X(20).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-DEVICE-MODEL       PIC X(30).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-PRICE              PIC X(15).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-DATE               PIC X(15).
           05 FILLER                  PIC X VALUE ",".
           05 WS-F-PERIOD             PIC X(15).

       01  WS-READ-REC.
           05 WS-R-APP-ID             PIC X(4).
           05 FILLER                  PIC X(116).

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
           05 LS-DEC-1                PIC X.
           05 LS-DEC-2                PIC X.
           05 LS-DEC-3                PIC X.
           05 LS-DEC-4                PIC X.
           05 LS-STATUS               PIC X(10).
           05 FILLER                  PIC X(20).
           
       PROCEDURE DIVISION USING LS-APP-DATA.
           DISPLAY " ".
           DISPLAY "--- SCREEN 1: QUOTATION (DEVICE INFO) ---".
           
           *> Device Type Selection
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               MOVE SPACES TO LS-DEVICE-TYPE
               DISPLAY "Enter Device Type (IOS / Android): "
               ACCEPT LS-DEVICE-TYPE
               
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(LS-DEVICE-TYPE)) 
                  = "IOS"
                   MOVE "IOS" TO LS-DEVICE-TYPE
                   MOVE "Y" TO WS-VALID
               ELSE IF FUNCTION UPPER-CASE
                   (FUNCTION TRIM(LS-DEVICE-TYPE)) 
                       = "ANDROID"
                   MOVE "ANDROID" TO LS-DEVICE-TYPE
                   MOVE "Y" TO WS-VALID
               ELSE
                   DISPLAY "[Error] Invalid Device Type!"
                   DISPLAY "        Please enter IOS or Android."
               END-IF
           END-PERFORM.
           
           *> Read models.csv and display valid models
           OPEN INPUT MODEL-FILE.
           IF WS-MODEL-STATUS = "35"
               DISPLAY "[Error] models.csv file not found!"
               EXIT PROGRAM
           END-IF.

           MOVE "N" TO WS-EOF-MODEL.
           MOVE 0 TO WS-MODEL-COUNT.
           DISPLAY " "
           DISPLAY"Available Models for "FUNCTION TRIM(LS-DEVICE-TYPE)

           PERFORM UNTIL WS-EOF-MODEL = "Y"
               READ MODEL-FILE
                   AT END 
                       MOVE "Y" TO WS-EOF-MODEL
                   NOT AT END
                       UNSTRING MODEL-REC DELIMITED BY ","
                           INTO WS-IN-TYPE WS-IN-NUM WS-IN-NAME
                           
                       IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-IN-TYPE)) 
                          = FUNCTION UPPER-CASE
                          (FUNCTION TRIM(LS-DEVICE-TYPE))
                           
                           ADD 1 TO WS-MODEL-COUNT
                           MOVE FUNCTION NUMVAL(WS-IN-NUM) 
                                TO WS-M-NUM(WS-MODEL-COUNT)
                           MOVE WS-IN-NAME TO WS-M-NAME(WS-MODEL-COUNT)
                           
                           DISPLAY 
                           "[ " WS-IN-NUM"] "FUNCTION TRIM(WS-IN-NAME)
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE MODEL-FILE.

           *> Device Model Selection Validation
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               MOVE SPACES TO WS-CHOICE-STR
               DISPLAY "Select Device Model (Enter Number): "
               ACCEPT WS-CHOICE-STR
               
               IF WS-CHOICE-STR NOT = SPACES AND 
                  FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-CHOICE-STR)) = 0
                   
                   COMPUTE WS-USER-CHOICE=FUNCTION NUMVAL(WS-CHOICE-STR)
                   MOVE "N" TO WS-FOUND
                   
                   PERFORM VARYING M-IDX FROM 1 BY 1 
                     UNTIL M-IDX > WS-MODEL-COUNT
                       IF WS-USER-CHOICE = WS-M-NUM(M-IDX)
                           MOVE WS-M-NAME(M-IDX) TO LS-DEVICE-MODEL
                           MOVE "Y" TO WS-FOUND
                       END-IF
                   END-PERFORM
                   
                   IF WS-FOUND = "Y"
                       DISPLAY "Selected: " 
                           FUNCTION TRIM(LS-DEVICE-MODEL)
                       MOVE "Y" TO WS-VALID
                   ELSE
                       DISPLAY"[Error]Number not in the list.Try again."
                   END-IF
               ELSE
                   DISPLAY "[Error] Please enter a valid number."
               END-IF
           END-PERFORM.

           *> Purchase Price Validation
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               MOVE SPACES TO WS-TEMP-PRICE
               DISPLAY "Enter Purchase Price (JPY): "
               ACCEPT WS-TEMP-PRICE
               
               IF WS-TEMP-PRICE = SPACES OR 
                  FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-TEMP-PRICE)) > 0
                   DISPLAY "[Error] Invalid Price! Must be numeric."
               ELSE
                   COMPUTE LS-PURCHASE-PRICE = 
                           FUNCTION NUMVAL(WS-TEMP-PRICE)
                   
                   IF LS-PURCHASE-PRICE < 10000 OR 
                       LS-PURCHASE-PRICE > 200000
                       DISPLAY "[Error] Price out of range! "
                               "Must be between 10,000 and 200,000 JPY."
                   ELSE
                       MOVE "Y" TO WS-VALID
                   END-IF
               END-IF
           END-PERFORM.

           *> Purchase Date Validation (Format, Logic, and Future Date)
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               MOVE SPACES TO LS-PURCHASE-DATE
               DISPLAY "Enter Purchase Date (YYYY-MM-DD): "
               ACCEPT LS-PURCHASE-DATE
               IF LS-PURCHASE-DATE = SPACES
                   DISPLAY "[Error] Purchase Date cannot be empty."
               ELSE
                   IF LS-PURCHASE-DATE(5:1) = "-" AND 
                      LS-PURCHASE-DATE(8:1) = "-" AND 
                      LS-PURCHASE-DATE(1:4) IS NUMERIC AND 
                      LS-PURCHASE-DATE(6:2) IS NUMERIC AND 
                      LS-PURCHASE-DATE(9:2) IS NUMERIC
                       
                       MOVE LS-PURCHASE-DATE(1:4) TO WS-DATE-YY
                       MOVE LS-PURCHASE-DATE(6:2) TO WS-DATE-MM
                       MOVE LS-PURCHASE-DATE(9:2) TO WS-DATE-DD
                       MOVE "Y" TO WS-VALID

                       IF WS-DATE-YY < 1000 OR WS-DATE-YY > 9999
                           DISPLAY "[Error] Year must be 4 digits."
                           MOVE "N" TO WS-VALID
                       END-IF

                       IF WS-DATE-MM < 1 OR WS-DATE-MM > 12
                           DISPLAY "[Error] Month must be 1 to 12."
                           MOVE "N" TO WS-VALID
                       END-IF

                       IF WS-VALID = "Y"
                           *> Check Leap Year for February
                           DIVIDE WS-DATE-YY BY 4 GIVING WS-QUOT 
                                                REMAINDER WS-REM-4
                           DIVIDE WS-DATE-YY BY 100 GIVING WS-QUOT 
                                                REMAINDER WS-REM-100
                           DIVIDE WS-DATE-YY BY 400 GIVING WS-QUOT 
                                                REMAINDER WS-REM-400

                           EVALUATE WS-DATE-MM
                               WHEN 4 WHEN 6 WHEN 9 WHEN 11
                                   MOVE 30 TO WS-MAX-DAYS
                               WHEN 2
                                   IF WS-REM-400 = 0 OR 
                                     (WS-REM-4 = 0 AND 
                                      WS-REM-100 NOT = 0)
                                       MOVE 29 TO WS-MAX-DAYS
                                   ELSE
                                       MOVE 28 TO WS-MAX-DAYS
                                   END-IF
                               WHEN OTHER
                                   MOVE 31 TO WS-MAX-DAYS
                           END-EVALUATE

                           IF WS-DATE-DD < 1 OR WS-DATE-DD > WS-MAX-DAYS
                               DISPLAY "[Error] Invalid Day for month."
                               MOVE "N" TO WS-VALID
                           END-IF
                           
                           *> Future Date Check
                           IF WS-VALID = "Y"
                               ACCEPT WS-SYS-DATE FROM DATE YYYYMMDD
                               
                               COMPUTE WS-INPUT-DATE-NUM = 
                                 (WS-DATE-YY * 10000) + 
                                 (WS-DATE-MM * 100) + WS-DATE-DD
                                 
                               IF WS-INPUT-DATE-NUM > WS-SYS-DATE
                                  DISPLAY "[Error] Future date is " &
                                  "not allowed. Please enter"&
                                  " a valid date."
                                   MOVE "N" TO WS-VALID
                               END-IF
                           END-IF
                       END-IF
                   ELSE
                       DISPLAY "[Error] Invalid format. Use YYYY-MM-DD."
                   END-IF
               END-IF
           END-PERFORM.

           *> Coverage Period Validation (Fixed to 12, 24, 36)
           MOVE "N" TO WS-VALID.
           PERFORM UNTIL WS-VALID = "Y"
               MOVE SPACES TO WS-TEMP-PERIOD
               DISPLAY "Enter Coverage Period (12, 24, or 36 Months): "
               ACCEPT WS-TEMP-PERIOD
              
               IF FUNCTION TRIM(WS-TEMP-PERIOD) = "12" OR 
                  FUNCTION TRIM(WS-TEMP-PERIOD) = "24" OR 
                  FUNCTION TRIM(WS-TEMP-PERIOD) = "36"
                   
                   COMPUTE LS-COVERAGE-PERIOD = 
                          FUNCTION NUMVAL(FUNCTION TRIM(WS-TEMP-PERIOD))
                   MOVE "Y" TO WS-VALID
               ELSE
                   DISPLAY "[Error] Invalid Period! Please enter"&
                   " 12, 24, or 36."
               END-IF
           END-PERFORM.

           *> Auto Generate App ID and Write to File
           OPEN INPUT DEVICE-FILE.
           IF WS-FILE-STATUS = "35"
               MOVE 1001 TO WS-NEW-ID
               CLOSE DEVICE-FILE
               OPEN OUTPUT DEVICE-FILE
               WRITE DEVICE-REC FROM 
               "AppID,DeviceType          ,DeviceModel              ," &
               "PurchasePrice  ,PurchaseDate   ,CoveragePeriod "
           ELSE
               MOVE "N" TO WS-EOF
               PERFORM UNTIL WS-EOF = "Y"
                   READ DEVICE-FILE INTO WS-READ-REC
                       AT END 
                           MOVE "Y" TO WS-EOF
                       NOT AT END
                           IF WS-R-APP-ID(1:1) >= "0" AND 
                              WS-R-APP-ID(1:1) <= "9"
                               MOVE WS-R-APP-ID TO WS-LAST-ID
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE DEVICE-FILE
               COMPUTE WS-NEW-ID = WS-LAST-ID + 1
               OPEN EXTEND DEVICE-FILE
           END-IF.

           MOVE WS-NEW-ID TO LS-APP-ID.
           MOVE WS-NEW-ID TO WS-F-APP-ID.
           MOVE LS-DEVICE-TYPE TO WS-F-DEVICE-TYPE.
           MOVE LS-DEVICE-MODEL TO WS-F-DEVICE-MODEL.
           MOVE LS-PURCHASE-PRICE TO WS-F-PRICE.
           MOVE LS-PURCHASE-DATE TO WS-F-DATE.
           MOVE LS-COVERAGE-PERIOD TO WS-F-PERIOD.

           WRITE DEVICE-REC FROM WS-FORMATTED-REC.
           CLOSE DEVICE-FILE.

           DISPLAY " ".
           DISPLAY "[System] Auto-Generated Application ID: " WS-NEW-ID.

           EXIT PROGRAM.