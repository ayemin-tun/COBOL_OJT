       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-STATUS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "result/user.csv"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS WS-USER-STATUS.
               
           SELECT DEVICE-FILE ASSIGN TO "result/device.csv"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS WS-DEV-STATUS.

           SELECT PLAN-FILE ASSIGN TO "result/plan.csv"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS WS-PLAN-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-REC                   PIC X(200).
       
       FD  DEVICE-FILE.
       01  DEVICE-REC                 PIC X(120).

       FD  PLAN-FILE.
       01  PLAN-REC                   PIC X(120).

       WORKING-STORAGE SECTION.
       *> File Statuses
       01  WS-USER-STATUS             PIC XX.
       01  WS-DEV-STATUS              PIC XX.
       01  WS-PLAN-STATUS             PIC XX.

       *> End of File Flags
       01  WS-USER-EOF                PIC X VALUE 'N'.
       01  WS-DEV-EOF                 PIC X VALUE 'N'.
       01  WS-PLAN-EOF                PIC X VALUE 'N'.
       
       *> Search Flags
       01  WS-USER-FOUND              PIC X VALUE 'N'.

       *> User Inputs
       01  WS-SEARCH-EMAIL            PIC X(30) VALUE SPACES.
       
       *> Loop Control Variable
       01  WS-LOOP-CHOICE             PIC X VALUE 'Y'.

       *> Display Formatters
       01  WS-PREMIUM-DISP            PIC ZZZ,ZZ9.
       01  WS-PRICE-DISP              PIC ZZZ,ZZ9.

       *> Read Variables for Unstring
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
       
       01  WS-READ-P-APPID            PIC X(4).
       01  WS-READ-P-PLANNAME         PIC X(10).
       01  WS-READ-P-PERIOD           PIC X(2).
       01  WS-READ-P-PREMIUM          PIC X(6).
       01  WS-READ-P-ANSWER           PIC X(20).
       01  WS-READ-P-STATUS           PIC X(10).
       01  WS-READ-P-SCORE            PIC X(3).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           *> Loop continues until user inputs 'N' or 'n'
           PERFORM PROCESS-SEARCH 
              UNTIL WS-LOOP-CHOICE = 'N' OR WS-LOOP-CHOICE = 'n'.

           DISPLAY "=== THANK YOU FOR USING INSURANCE INQUIRY ===".
           PERFORM END-PROGRAM.

       *> ---------------------------------------------------------
       *> Process Single Search Inquiry Task
       *> ---------------------------------------------------------
       PROCESS-SEARCH.
           MOVE "N" TO WS-USER-EOF.
           MOVE "N" TO WS-USER-FOUND.
           MOVE SPACES TO WS-SEARCH-EMAIL.

           DISPLAY " ".
           DISPLAY "=== SEARCH APPLICANT INSURANCE STATUS ===".
           DISPLAY "Enter Applicant Email to Check: ".
           ACCEPT WS-SEARCH-EMAIL.

           IF FUNCTION TRIM(WS-SEARCH-EMAIL) = SPACES
              DISPLAY "[Error] Email cannot be empty."
           ELSE
              *> Scan and locate matching Email in USER-FILE
              OPEN INPUT USER-FILE
              IF WS-USER-STATUS = "35"
                 DISPLAY "[Error] User database file not found."
                 MOVE "N" TO WS-LOOP-CHOICE
              ELSE
                 PERFORM UNTIL WS-USER-EOF = "Y" OR WS-USER-FOUND = "Y"
                    READ USER-FILE INTO USER-REC
                       AT END
                          MOVE "Y" TO WS-USER-EOF
                       NOT AT END
                          UNSTRING USER-REC DELIMITED BY ","
                             INTO WS-READ-U-APPID WS-READ-U-NAME
                                  WS-READ-U-EMAIL WS-READ-U-PHONE
                                  WS-READ-U-POSTAL WS-READ-U-ADDRESS
                                  WS-READ-U-DOB WS-READ-U-REGDATE
                          
                          *> If email matches exactly, fetch remaining records
                          IF FUNCTION TRIM(WS-READ-U-EMAIL) = 
                             FUNCTION TRIM(WS-SEARCH-EMAIL)
                             MOVE "Y" TO WS-USER-FOUND
                             PERFORM FETCH-DEVICE-DETAILS
                             PERFORM FETCH-PLAN-DETAILS
                             PERFORM DISPLAY-DETAILS
                          END-IF
                    END-READ
                 END-PERFORM
                 CLOSE USER-FILE

                 IF WS-USER-FOUND = "N"
                    DISPLAY "[Alert] No record found for Email: " 
                            FUNCTION TRIM(WS-SEARCH-EMAIL)
                 END-IF
              END-IF
           END-IF.

           *> Prompt user for iterative search execution
           DISPLAY "Do you want to check another applicant? (Y/N): ".
           ACCEPT WS-LOOP-CHOICE.

       *> ---------------------------------------------------------
       *> Fetch Device Details Matching Current Application ID
       *> ---------------------------------------------------------
       FETCH-DEVICE-DETAILS.
           MOVE "N" TO WS-DEV-EOF.
           OPEN INPUT DEVICE-FILE.
           IF WS-DEV-STATUS NOT = "35"
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
                          MOVE "Y" TO WS-DEV-EOF
                       END-IF
                 END-READ
              END-PERFORM
           END-IF.
           CLOSE DEVICE-FILE.

       *> ---------------------------------------------------------
       *> Fetch Plan Details and Batch Run Status Using App ID
       *> ---------------------------------------------------------
       FETCH-PLAN-DETAILS.
           MOVE "N" TO WS-PLAN-EOF.
           OPEN INPUT PLAN-FILE.
           IF WS-PLAN-STATUS NOT = "35"
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
                          MOVE "Y" TO WS-PLAN-EOF
                       END-IF
                 END-READ
              END-PERFORM
           END-IF.
           CLOSE PLAN-FILE.

       *> ---------------------------------------------------------
       *> Format and Render Detailed Inquiry Status Report
       *> ---------------------------------------------------------
       DISPLAY-DETAILS.
           MOVE FUNCTION NUMVAL(WS-READ-P-PREMIUM) TO WS-PREMIUM-DISP.
           MOVE FUNCTION NUMVAL(WS-READ-D-PRICE) TO WS-PRICE-DISP.

           DISPLAY " ".
           DISPLAY "   *********************************************".
           DISPLAY "   * SMART DEVICE INSURANCE CO.                 *".
           DISPLAY "   * INQUIRY STATUS REPORT                      *".
           DISPLAY "   *********************************************".
           DISPLAY "   Policy Date: " WS-READ-U-REGDATE.
           DISPLAY "   App ID     : " WS-READ-U-APPID.
           DISPLAY "   ---------------------------------------------".
           DISPLAY "   [CUSTOMER DETAILS]".
           DISPLAY "   Name       : " FUNCTION TRIM(WS-READ-U-NAME).
           DISPLAY "   Phone      : " FUNCTION TRIM(WS-READ-U-PHONE).
           DISPLAY "   Email      : " FUNCTION TRIM(WS-READ-U-EMAIL).
           DISPLAY "   Address    : " FUNCTION TRIM(WS-READ-U-ADDRESS).
           DISPLAY "   ---------------------------------------------".
           DISPLAY "   [DEVICE DETAILS]".
           DISPLAY "   Type       : " FUNCTION TRIM(WS-READ-D-TYPE).
           DISPLAY "   Model      : " FUNCTION TRIM(WS-READ-D-MODEL).
           DISPLAY "   Price      : " WS-PRICE-DISP " JPY".
           DISPLAY "   ---------------------------------------------".
           DISPLAY "   [PLAN & EVALUATION]".
           DISPLAY "   Plan Name  : " FUNCTION TRIM(WS-READ-P-PLANNAME).
           DISPLAY "   Period     : " 
           FUNCTION TRIM(WS-READ-D-PERIOD) " Months".
           DISPLAY "   Eval Score : " WS-READ-P-SCORE " Pts".
           DISPLAY "   ---------------------------------------------".
           DISPLAY "   TOTAL PREMIUM DUE  : " WS-PREMIUM-DISP " JPY".
           DISPLAY "   >> CURRENT STATUS  : " 
           FUNCTION TRIM(WS-READ-P-STATUS).
           DISPLAY "   *********************************************".
           DISPLAY " ".

       END-PROGRAM.
           EXIT PROGRAM.