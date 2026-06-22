       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCR5CMP.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PREMIUM-DISP            PIC ZZZ,ZZ9.
       01  WS-PRICE-DISP              PIC ZZZ,ZZ9.
       01  WS-CURRENT-DATE            PIC 9(8).
       01  WS-DATE-STR.
           05 WS-YYYY                 PIC 9(4).
           05 FILLER                  PIC X VALUE "-".
           05 WS-MM                   PIC 9(2).
           05 FILLER                  PIC X VALUE "-".
           05 WS-DD                   PIC 9(2).

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
           05 FILLER                  PIC X(20).

       PROCEDURE DIVISION USING LS-APP-DATA.
           MOVE LS-PREMIUM TO WS-PREMIUM-DISP.
           MOVE LS-PURCHASE-PRICE TO WS-PRICE-DISP.

           *> System ဆီမှ ဒီနေ့ရက်စွဲကို ယူခြင်း
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.
           MOVE WS-CURRENT-DATE(1:4) TO WS-YYYY.
           MOVE WS-CURRENT-DATE(5:2) TO WS-MM.
           MOVE WS-CURRENT-DATE(7:2) TO WS-DD.

           DISPLAY " ".
           DISPLAY "   *********************************************".
           DISPLAY "   * SMART DEVICE INSURANCE CO.         *".
           DISPLAY "   * OFFICIAL RECEIPT              *".
           DISPLAY "   *********************************************".
           DISPLAY "   Date       : " WS-DATE-STR.
           DISPLAY "   Receipt No : " LS-APP-ID.
           DISPLAY "   ---------------------------------------------".
           DISPLAY "   [CUSTOMER DETAILS]".
           DISPLAY "   Name       : " FUNCTION TRIM(LS-USER-NAME).
           DISPLAY "   Phone      : " FUNCTION TRIM(LS-USER-PHONE).
           DISPLAY "   Email      : " FUNCTION TRIM(LS-USER-EMAIL).
           DISPLAY "   ---------------------------------------------".
           DISPLAY "   [DEVICE DETAILS]".
           DISPLAY "   Type       : " FUNCTION TRIM(LS-DEVICE-TYPE).
           DISPLAY "   Model      : " FUNCTION TRIM(LS-DEVICE-MODEL).
           DISPLAY "   Price      : " WS-PRICE-DISP " JPY".
           DISPLAY "   ---------------------------------------------".
           DISPLAY "   [PLAN & COVERAGE]".
           DISPLAY "   Plan       : " FUNCTION TRIM(LS-PLAN-NAME).
           DISPLAY "   Period     : " LS-COVERAGE-PERIOD " Months".
           DISPLAY "   ---------------------------------------------".
           DISPLAY "   TOTAL PREMIUM DUE  : " WS-PREMIUM-DISP " JPY".
           DISPLAY "   Application Status : " FUNCTION TRIM(LS-STATUS).
           DISPLAY "   *********************************************".
           DISPLAY "   * Thank you for choosing our services!    *".
           DISPLAY "   *********************************************".
           DISPLAY " ".

           EXIT PROGRAM.
           