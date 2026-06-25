       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATPLAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT IN-FILE ASSIGN TO "result/plan.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT PLAN-MASTER-FILE ASSIGN TO "data/plan_master.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                     PIC X(150).
       FD  PLAN-MASTER-FILE.
       01  MASTER-RECORD              PIC X(150).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC XX.
       01  WS-EOF                     PIC X VALUE "N".
       01  WS-ROW-COUNT               PIC 9(4) VALUE 0.
       01  WS-IDX                     PIC 9(4) VALUE 0.
       *> --- (ပြင်ဆင်ချက်) Master File ဖတ်သည့်နေရာတွင် သုံးမည့် Variable များ ---
       01  WS-MASTER-EOF              PIC X VALUE "N".
       01  WS-FOUND-FLAG              PIC X VALUE "N".
       01  WS-M-RECORD-BUFFER         PIC X(150).
       01  WS-M-PLAN-NAME             PIC X(15).
       01  WS-M-PLAN-DESC             PIC X(60).
       01  WS-M-PLAN-SCORE            PIC 9(03).

       *> --- MEMORY TABLE (ARRAY) TO STORE DATA ---
       01  WS-BATCH-TABLE.
           05 WS-ROW OCCURS 100 TIMES.
               10 WS-APP-ID           PIC X(4).
               10 WS-PLAN             PIC X(10).
               10 WS-PERIOD           PIC X(2).
               10 WS-PREMIUM          PIC X(6).
               10 WS-ANSWERS          PIC X(20).
               10 WS-STATUS           PIC X(10).
               10 WS-SCORE            PIC 9(3).

       *>  Format Layout
       01  WS-OUT-FORMATTED.
           05 WS-O-APP-ID             PIC X(4).
           05 FILLER                  PIC X VALUE ",".
           05 WS-O-PLAN               PIC X(10).
           05 FILLER                  PIC X VALUE ",".
           05 WS-O-PERIOD             PIC X(2).
           05 FILLER                  PIC X VALUE ",".
           05 WS-O-PREMIUM            PIC X(6).
           05 FILLER                  PIC X VALUE ",".
           05 WS-O-ANSWERS            PIC X(20).
           05 FILLER                  PIC X VALUE ",".
           05 WS-O-STATUS             PIC X(10).
           05 FILLER                  PIC X VALUE ",".
           05 WS-O-SCORE              PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY " "
           DISPLAY "=== STARTING BATCH PROCESS ==="

           *> --------------------------------------------------------
           *> (1) add file data into array table 
           *> --------------------------------------------------------
           OPEN INPUT IN-FILE.
           IF WS-FILE-STATUS = "35"
               DISPLAY "[Error] result/plan.csv not found!"
               STOP RUN
           END-IF.

           *> skip header role 
           READ IN-FILE INTO IN-REC.

           MOVE "N" TO WS-EOF.
           MOVE 0 TO WS-ROW-COUNT.

           PERFORM UNTIL WS-EOF = "Y" OR WS-ROW-COUNT >= 100
               READ IN-FILE INTO IN-REC
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-ROW-COUNT
                       
                       *> use unstring to add data into csv 
                       UNSTRING IN-REC DELIMITED BY ","
                           INTO WS-APP-ID(WS-ROW-COUNT)
                                WS-PLAN(WS-ROW-COUNT)
                                WS-PERIOD(WS-ROW-COUNT)
                                WS-PREMIUM(WS-ROW-COUNT)
                                WS-ANSWERS(WS-ROW-COUNT)
                                WS-STATUS(WS-ROW-COUNT)
                                WS-SCORE(WS-ROW-COUNT)
               END-READ
           END-PERFORM.
           CLOSE IN-FILE.

           DISPLAY "[Batch] Read records into memory."

           *> --------------------------------------------------------
           *> (2) check status 
           *> --------------------------------------------------------
           PERFORM VARYING WS-IDX FROM 1 BY 1 
           UNTIL WS-IDX > WS-ROW-COUNT
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-STATUS(WS-IDX))) 
                  = "PENDING" 
               *> --- Score is less than 40 approve 
                  PERFORM  CHECKSCORE

               END-IF
           END-PERFORM.

           *> --------------------------------------------------------
           *> OVER WRITE FILE 
           *> --------------------------------------------------------
           DISPLAY "[Batch] Updating plan.csv with updated status..."
           OPEN OUTPUT IN-FILE.

           *> HEader rewrite 
           WRITE IN-REC FROM 
           "AppID,PlanName,Period,Premium,Answers             ," &
           "Status    ,Score".

           *> using loop to add data from array to file 
           PERFORM VARYING WS-IDX FROM 1 BY 1
            UNTIL WS-IDX > WS-ROW-COUNT
               MOVE WS-APP-ID(WS-IDX)  TO WS-O-APP-ID
               MOVE WS-PLAN(WS-IDX)    TO WS-O-PLAN
               MOVE WS-PERIOD(WS-IDX)  TO WS-O-PERIOD
               MOVE WS-PREMIUM(WS-IDX) TO WS-O-PREMIUM
               MOVE WS-ANSWERS(WS-IDX) TO WS-O-ANSWERS
               MOVE WS-STATUS(WS-IDX)  TO WS-O-STATUS
               MOVE WS-SCORE(WS-IDX)   TO WS-O-SCORE

               WRITE IN-REC FROM WS-OUT-FORMATTED
           END-PERFORM.

           CLOSE IN-FILE.
           DISPLAY "=== BATCH PROCESS COMPLETED SUCCESSFULLY ==="
           STOP RUN.
       
       *> --------------------------------------------------------
       *> (ပြင်ဆင်ချက်) Dynamic Plan Master File တိုက်စစ်သည့် နေရာ
       *> --------------------------------------------------------
       CHECKSCORE.
           MOVE "N" TO WS-MASTER-EOF.
           MOVE "N" TO WS-FOUND-FLAG.
           
           OPEN INPUT PLAN-MASTER-FILE.
           
           PERFORM UNTIL WS-MASTER-EOF = "Y" OR WS-FOUND-FLAG = "Y"
               READ PLAN-MASTER-FILE INTO WS-M-RECORD-BUFFER
                   AT END
                       MOVE "Y" TO WS-MASTER-EOF
                   NOT AT END
                       *> CSV မှ Comma ခံပြီး Data များ ခွဲထုတ်ခြင်း
                       UNSTRING WS-M-RECORD-BUFFER DELIMITED BY ","
                           INTO WS-M-PLAN-NAME
                                WS-M-PLAN-DESC
                                WS-M-PLAN-SCORE
                       END-UNSTRING
                       
           *> Space အပိုများကြောင့် အမှားမပြအောင် TRIM လုပ်ပြီး တိုက်စစ်ခြင်း
                       IF FUNCTION TRIM(WS-PLAN(WS-IDX)) = 
                          FUNCTION TRIM(WS-M-PLAN-NAME)
                            MOVE "Y" TO WS-FOUND-FLAG
                            
           *> Hardcode မဟုတ်တော့ဘဲ File မှရလာသော Score နှင့် နှိုင်းယှဉ်ခြင်း
                            IF WS-SCORE(WS-IDX) <= WS-M-PLAN-SCORE
                                 MOVE "APPROVED" TO WS-STATUS(WS-IDX)
                            ELSE
                                 MOVE "REJECTED" TO WS-STATUS(WS-IDX)
                            END-IF
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE PLAN-MASTER-FILE.
           
           *> Master File ထဲတွင် အဆိုပါ Plan အမျိုးအစား ရှာမတွေ့ခဲ့ပါက
           IF WS-FOUND-FLAG = "N"
                DISPLAY "[Error] Unknown plan type: " 
                        FUNCTION TRIM(WS-PLAN(WS-IDX))
                MOVE "REJECTED" TO WS-STATUS(WS-IDX)
           END-IF.
