       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELDEV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEVICE-FILE ASSIGN TO "data/device.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "tmp/temp_del.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  DEVICE-FILE.
       01  DEVICE-REC                 PIC X(120).
       FD  TEMP-FILE.
       01  TEMP-REC                   PIC X(120).

       WORKING-STORAGE SECTION.
       01  WS-EOF                     PIC X VALUE "N".
       01  WS-REC-ID                  PIC 9(4).

       LINKAGE SECTION.
       01  LS-APP-ID                  PIC 9(4).

       PROCEDURE DIVISION USING LS-APP-ID.
           OPEN INPUT DEVICE-FILE.
           OPEN OUTPUT TEMP-FILE.

           MOVE "N" TO WS-EOF.
           PERFORM UNTIL WS-EOF = "Y"
               READ DEVICE-FILE INTO DEVICE-REC
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       
                       UNSTRING DEVICE-REC DELIMITED BY "," 
                       INTO WS-REC-ID
                       IF WS-REC-ID NOT = LS-APP-ID
                           WRITE TEMP-REC FROM DEVICE-REC
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE DEVICE-FILE.
           CLOSE TEMP-FILE.

           OPEN INPUT TEMP-FILE.
           OPEN OUTPUT DEVICE-FILE.
           MOVE "N" TO WS-EOF.
           PERFORM UNTIL WS-EOF = "Y"
               READ TEMP-FILE INTO TEMP-REC
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       WRITE DEVICE-REC FROM TEMP-REC
               END-PERFORM
       
           CLOSE TEMP-FILE.
           CLOSE DEVICE-FILE.
           EXIT PROGRAM.