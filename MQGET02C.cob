       IDENTIFICATION DIVISION.
       PROGRAM-ID. MQGET02C.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  MQ-OD-AREA.
           COPY CMQODV.
       01  MQ-MD-AREA.
           COPY CMQMDV.
       01  MQ-GMO-AREA.
           COPY CMQGMOV.
       01  MQ-CONST-AREA.
           COPY CMQV.

       01 WS-QUEUE-NAME-TEXT   PIC X(48) VALUE SPACES.
       01 WS-MSG-LEN           PIC S9(9) COMP VALUE 100.
       01 WS-MSG-DATA-LEN      PIC S9(9) COMP VALUE 0.
       01 WS-MSG-TEXT          PIC X(100) VALUE SPACES.

       01 WS-HCONN             PIC S9(9) COMP VALUE 0.
       01 WS-HOBJ              PIC S9(9) COMP VALUE 0.
       01 WS-COMP-CODE         PIC S9(9) COMP VALUE 0.
       01 WS-REASON-CODE       PIC S9(9) COMP VALUE 0.
       01 WS-MSG-SAIDA         PIC X(120) VALUE SPACES.

       01  WS-COMP-CODE-DISP   PIC Z(8)9.
       01  WS-REASON-CODE-DISP PIC Z(8)9.
       01  WS-RESP-DISP        PIC Z(8)9.

       01  WS-TS-QUEUE         PIC X(8) VALUE 'TEST    '.
       01  WS-TS-ITEM          PIC S9(4) COMP VALUE 1.
       01  WS-TS-RECORD        PIC X(120) VALUE SPACES.
       01  WS-TS-RESP          PIC S9(8) COMP VALUE 0.
       01  WS-TERM-MSG         PIC X(72) VALUE SPACES.
       01  WS-OPER-MSG         PIC X(80) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCESS.

           STRING 'LAB.LOCAL.APP.REQ.CICS'
               DELIMITED BY SIZE
               INTO WS-QUEUE-NAME-TEXT
           END-STRING.

           INITIALIZE MQOD.
           MOVE 'OD  '              TO MQOD-STRUCID.
           MOVE 1                   TO MQOD-VERSION.
           MOVE 1                   TO MQOD-OBJECTTYPE.
           MOVE WS-QUEUE-NAME-TEXT  TO MQOD-OBJECTNAME.

           CALL 'MQOPEN' USING WS-HCONN
                               MQOD
                               MQOO-INPUT-SHARED
                               WS-HOBJ
                               WS-COMP-CODE
                               WS-REASON-CODE.

           IF WS-COMP-CODE NOT = MQCC-OK
               MOVE WS-COMP-CODE   TO WS-COMP-CODE-DISP
               MOVE WS-REASON-CODE TO WS-REASON-CODE-DISP

               STRING 'ERRO#MQOPEN '
                      WS-COMP-CODE-DISP
                      WS-REASON-CODE-DISP DELIMITED BY SIZE
                INTO WS-MSG-SAIDA

               PERFORM GRAVA-LOG-TS
           ELSE
               INITIALIZE MQMD
               MOVE 'MD  '        TO MQMD-STRUCID
               MOVE 1             TO MQMD-VERSION
               MOVE MQMI-NONE     TO MQMD-MSGID
               MOVE MQCI-NONE     TO MQMD-CORRELID

               INITIALIZE MQGMO
               MOVE 'GMO '        TO MQGMO-STRUCID
               MOVE 1             TO MQGMO-VERSION
               COMPUTE MQGMO-OPTIONS = MQGMO-NO-WAIT + MQGMO-CONVERT

               MOVE 100 TO WS-MSG-LEN

               CALL 'MQGET' USING WS-HCONN
                                  WS-HOBJ
                                  MQMD
                                  MQGMO
                                  WS-MSG-LEN
                                  WS-MSG-TEXT
                                  WS-MSG-DATA-LEN
                                  WS-COMP-CODE
                                  WS-REASON-CODE

               IF WS-COMP-CODE = MQCC-OK
                   STRING 'MSG: '
                       WS-MSG-TEXT(1:WS-MSG-DATA-LEN) DELIMITED BY SIZE
                       INTO WS-MSG-SAIDA
                   END-STRING
               ELSE
                MOVE WS-COMP-CODE   TO WS-COMP-CODE-DISP
                MOVE WS-REASON-CODE TO WS-REASON-CODE-DISP

                STRING 'ERRO#MQGET '
                       WS-COMP-CODE-DISP
                       WS-REASON-CODE-DISP DELIMITED BY SIZE
                 INTO WS-MSG-SAIDA

               END-IF

               PERFORM GRAVA-LOG-TS

               MOVE 0 TO WS-COMP-CODE
               MOVE 0 TO WS-REASON-CODE

               CALL 'MQCLOSE' USING WS-HCONN
                                    WS-HOBJ
                                    MQCO-NONE
                                    WS-COMP-CODE
                                    WS-REASON-CODE
           END-IF.

           MOVE 'PROGRAMA MQGET02C EXECUTADO' TO WS-TERM-MSG

           EXEC CICS SEND
               FROM(WS-TERM-MSG)
               LENGTH(72)
               ERASE
               NOHANDLE
           END-EXEC.

           EXEC CICS RETURN END-EXEC.


       GRAVA-LOG-TS.

           STRING WS-MSG-SAIDA DELIMITED BY SIZE
               INTO WS-TS-RECORD
           END-STRING.

           EXEC CICS WRITEQ TS
               QUEUE(WS-TS-QUEUE)
               FROM(WS-TS-RECORD)
               ITEM(WS-TS-ITEM)
               RESP(WS-TS-RESP)
           END-EXEC.

           IF WS-TS-RESP = DFHRESP(NORMAL)
               ADD 1 TO WS-TS-ITEM
           ELSE
               MOVE 'ERRO AO GRAVAR TS' TO WS-OPER-MSG
               EXEC CICS WRITE OPERATOR
                   TEXT(WS-OPER-MSG)
               END-EXEC
           END-IF.
           EXIT.

       END PROGRAM MQGET02C.
