       IDENTIFICATION DIVISION.
       PROGRAM-ID. MQRESWIN.

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
       01  MQ-PMO-AREA.
           COPY CMQPMOV.
      *
       01  WS-QUEUE-NAME-TEXT  PIC X(48) VALUE SPACES.
       01  WS-MSG-LEN          PIC S9(9) COMP VALUE 56.
       01  WS-MSG-DATA-LEN     PIC S9(9) COMP VALUE 0.
       01  WS-HCONN            PIC S9(9) COMP VALUE 0.
       01  WS-HOBJ             PIC S9(9) COMP VALUE 0.
       01  WS-COMP-CODE        PIC S9(9) COMP VALUE 0.
       01  WS-REASON-CODE      PIC S9(9) COMP VALUE 0.
      *
       01  WS-PUT-QUEUE        PIC X(48) VALUE SPACES.
       01  WS-PUT-LEN          PIC S9(9) COMP VALUE 0.
       01  WS-PUT-COMP         PIC S9(9) COMP VALUE 0.
       01  WS-PUT-REASON       PIC S9(9) COMP VALUE 0.
       01  WS-PUT-HCONN        PIC S9(9) COMP VALUE 0.
       01  WS-PUT-HOBJ         PIC S9(9) COMP VALUE 0.
      *
       01  WS-COMP-CODE-DISP   PIC 9(10).
       01  WS-REASON-CODE-DISP PIC 9(10).
       01  WS-RESP-DISP        PIC Z(9)9.
      *
       01  WS-MSG-ENTRADA      PIC X(56) VALUE SPACES.
       01  WS-RESP             PIC S9(08) COMP VALUE 0.
       01  WS-RESP2            PIC S9(08) COMP VALUE 0.
      *
       01  WS-MSG-SAIDA        PIC X(80) VALUE SPACES.
       01  WS-MSG-SAIDA-PUT    PIC X(80) VALUE SPACES.
       01  WS-MSG-LEN-OUT      PIC S9(04) COMP VALUE 80.
       01  WS-OPER-MSG         PIC X(120) VALUE SPACES.
      *
       01  WS-TS-QUEUE         PIC X(8) VALUE 'WIN     '.
       01  WS-TS-ITEM          PIC S9(4) COMP VALUE 1.
       01  WS-TS-RECORD        PIC X(120) VALUE SPACES.
       01  WS-TS-RESP          PIC S9(8) COMP VALUE 0.
      *
       01 WS-ABSTIME           PIC S9(15) COMP-3.
       01 WS-DATA              PIC X(10) VALUE SPACES.
       01 WS-HORA              PIC X(08) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       MAIN-PARA.

           PERFORM OBTER-MENSAGEM-MQ.


           IF WS-RESP = 0
              MOVE SPACES TO  WS-MSG-SAIDA
              MOVE "SIM, SEMPRE ESTIVE AQUI" TO WS-MSG-SAIDA
              PERFORM RESP-MENSAGEM-MQ
           END-IF.

           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
      *===========================================================
       OBTER-MENSAGEM-MQ.

           STRING 'LAB.LOCAL.APP.REQ.CICS'
               DELIMITED BY SIZE
               INTO WS-QUEUE-NAME-TEXT
           END-STRING.

           MOVE SPACES TO WS-MSG-SAIDA

           INITIALIZE MQOD
           MOVE 'OD  '             TO MQOD-STRUCID
           MOVE 1                  TO MQOD-VERSION
           MOVE 1                  TO MQOD-OBJECTTYPE
           MOVE WS-QUEUE-NAME-TEXT TO MQOD-OBJECTNAME

           CALL 'MQOPEN' USING WS-HCONN
                               MQOD
                               MQOO-INPUT-SHARED
                               WS-HOBJ
                               WS-COMP-CODE
                               WS-REASON-CODE

           IF WS-COMP-CODE NOT = MQCC-OK

              MOVE WS-COMP-CODE   TO WS-COMP-CODE-DISP
              MOVE WS-REASON-CODE TO WS-REASON-CODE-DISP

              STRING 'ERRO MQOPEN CC='
                     WS-COMP-CODE-DISP DELIMITED BY SIZE
                     ' RC='
                     WS-REASON-CODE-DISP DELIMITED BY SIZE
                INTO WS-MSG-SAIDA
              END-STRING

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

            MOVE 56 TO WS-MSG-LEN

            CALL 'MQGET' USING WS-HCONN
                               WS-HOBJ
                               MQMD
                               MQGMO
                               WS-MSG-LEN
                               WS-MSG-ENTRADA
                               WS-MSG-DATA-LEN
                               WS-COMP-CODE
                               WS-REASON-CODE

            MOVE WS-COMP-CODE   TO WS-COMP-CODE-DISP
            MOVE WS-REASON-CODE TO WS-REASON-CODE-DISP

            IF WS-COMP-CODE = MQCC-OK

               STRING 'MSG RECEBIDA: '
                      WS-MSG-ENTRADA DELIMITED BY SIZE
                 INTO WS-MSG-SAIDA
               END-STRING
               PERFORM GRAVA-LOG-TS

            ELSE

               IF WS-REASON-CODE = MQRC-NO-MSG-AVAILABLE
                  STRING 'FILA VAZIA CC='
                    WS-COMP-CODE-DISP DELIMITED BY SIZE
                    ' RC='
                    WS-REASON-CODE-DISP DELIMITED BY SIZE
                    INTO WS-MSG-SAIDA
                  END-STRING
                  PERFORM GRAVA-LOG-TS
               ELSE

                  STRING 'ERRO MQGET CC='
                         WS-COMP-CODE-DISP DELIMITED BY SIZE
                         ' RC='
                         WS-REASON-CODE-DISP DELIMITED BY SIZE
                    INTO WS-MSG-SAIDA
                  END-STRING
                  PERFORM GRAVA-LOG-TS
               END-IF
            END-IF

            CALL 'MQCLOSE' USING WS-HCONN
                                 WS-HOBJ
                                 MQCO-NONE
                                 WS-COMP-CODE
                                 WS-REASON-CODE
           END-IF.

           EXIT.
      *===========================================================
       RESP-MENSAGEM-MQ.

           STRING 'LAB.REMOTE.QM01.RECEBE.ZOS'
               DELIMITED BY SIZE
               INTO WS-PUT-QUEUE
           END-STRING.

           COMPUTE WS-PUT-LEN = FUNCTION LENGTH(WS-MSG-SAIDA).

           INITIALIZE MQOD.
           MOVE 'OD  '             TO MQOD-STRUCID.
           MOVE 1                  TO MQOD-VERSION.
           MOVE 1                  TO MQOD-OBJECTTYPE.
           MOVE WS-PUT-QUEUE       TO MQOD-OBJECTNAME.

           CALL 'MQOPEN' USING WS-HCONN
                               MQOD
                               MQOO-OUTPUT
                               WS-PUT-HOBJ
                               WS-PUT-COMP
                               WS-PUT-REASON.

           IF WS-PUT-COMP = MQCC-OK
               INITIALIZE MQMD
               MOVE 'MD  '         TO MQMD-STRUCID
               MOVE 1              TO MQMD-VERSION
               MOVE MQMT-DATAGRAM  TO MQMD-MSGTYPE
               MOVE MQEI-UNLIMITED TO MQMD-EXPIRY
               MOVE 'MQSTR   '     TO MQMD-FORMAT
               MOVE 'PMO '         TO MQPMO-STRUCID
               MOVE 1              TO MQPMO-VERSION
               MOVE MQPMO-NONE     TO MQPMO-OPTIONS

               CALL 'MQPUT' USING WS-HCONN
                                  WS-PUT-HOBJ
                                  MQMD
                                  MQPMO
                                  WS-PUT-LEN
                                  WS-MSG-SAIDA
                                  WS-PUT-COMP
                                  WS-PUT-REASON

               CALL 'MQCLOSE' USING WS-PUT-HCONN
                                    WS-PUT-HOBJ
                                    MQCO-NONE
                                    WS-PUT-COMP
                                    WS-PUT-REASON
           ELSE
               MOVE WS-PUT-COMP   TO WS-COMP-CODE-DISP
               MOVE WS-PUT-REASON TO WS-REASON-CODE-DISP

               STRING WS-COMP-CODE-DISP DELIMITED BY SIZE
                       WS-REASON-CODE-DISP DELIMITED BY SIZE
                 INTO WS-MSG-SAIDA
               END-STRING
           END-IF.
               PERFORM GRAVA-LOG-TS
           EXIT.
      *
      *====================================================
      * Grava log na ts CONS
      *====================================================
       GRAVA-LOG-TS.

           EXEC CICS ASKTIME
                ABSTIME(WS-ABSTIME)
           END-EXEC.

           EXEC CICS FORMATTIME
                ABSTIME(WS-ABSTIME)
                DDMMYYYY(WS-DATA)
                DATESEP('-')
           END-EXEC.

           EXEC CICS FORMATTIME
                ABSTIME(WS-ABSTIME)
                TIME(WS-HORA)
                TIMESEP(':')
           END-EXEC.

           STRING WS-DATA ' ' WS-HORA ' '
                  WS-MSG-SAIDA DELIMITED BY SIZE
             INTO WS-TS-RECORD
           END-STRING

      *    MOVE WS-MSG-SAIDA TO WS-TS-RECORD

           EXEC CICS WRITEQ TS
               QUEUE(WS-TS-QUEUE)
               FROM(WS-TS-RECORD)
               ITEM(WS-TS-ITEM)
               RESP(WS-TS-RESP)
           END-EXEC.

           IF WS-TS-RESP = DFHRESP(NORMAL)
               ADD 1 TO WS-TS-ITEM
           ELSE
               MOVE 'Erro ao grava TS ' TO WS-OPER-MSG
               MOVE WS-TS-QUEUE         TO WS-OPER-MSG
               EXEC CICS WRITE OPERATOR
                   TEXT(WS-OPER-MSG)
               END-EXEC
           END-IF.

           EXIT.
      *
       END PROGRAM MQRESWIN.
