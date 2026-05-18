       IDENTIFICATION DIVISION.
       PROGRAM-ID. MQGET01B.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  MQ-CONSTANTS.
           COPY CMQV.
           COPY CMQODV.
           COPY CMQMDV.
           COPY CMQGMOV.

       01  WS-QMGR-NAME           PIC X(48) VALUE 'CSQ7'.
       01  WS-QUEUE-NAME-TEXT     PIC X(48) VALUE SPACES.
       01  WS-MSG-LEN             PIC S9(9) COMP VALUE 100.
       01  WS-MSG-DATA-LEN        PIC S9(9) COMP VALUE 0.
       01  WS-MSG-TEXT            PIC X(100) VALUE SPACES.
       01  WS-HCONN               PIC S9(9) COMP VALUE 0.
       01  WS-HOBJ                PIC S9(9) COMP VALUE 0.
       01  WS-COMP-CODE           PIC S9(9) COMP VALUE 0.
       01  WS-REASON-CODE         PIC S9(9) COMP VALUE 0.
       01  WS-DISP-COMP           PIC Z(9)9.
       01  WS-DISP-REAS           PIC Z(9)9.

       LINKAGE SECTION.
       01  LS-PARM.
           05 LS-PARM-LEN         PIC S9(4) COMP.
           05 LS-PARM-DATA        PIC X(48).

       PROCEDURE DIVISION USING LS-PARM.
       MAIN-PROCESS.
           DISPLAY '============================================'.
           DISPLAY 'PROGRAMA MQGET01B - LER MENSAGEM DA FILA'.
           DISPLAY '============================================'.
           DISPLAY ' '

           IF LS-PARM-LEN > 0 AND LS-PARM-LEN <= 48
               MOVE LS-PARM-DATA(1:LS-PARM-LEN) TO WS-QUEUE-NAME-TEXT
               DISPLAY 'FILA INFORMADA: ' WS-QUEUE-NAME-TEXT
           ELSE
               DISPLAY 'INFORMAR A FILA NO PARM'
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

           CALL 'MQCONN' USING WS-QMGR-NAME
                               WS-HCONN
                               WS-COMP-CODE
                               WS-REASON-CODE.

           IF WS-COMP-CODE NOT = MQCC-OK
               DISPLAY 'FALHA NO MQCONN'
               PERFORM MOSTRA-ERRO
               GOBACK.

           INITIALIZE MQOD.
           MOVE 'OD  '             TO MQOD-STRUCID.
           MOVE 1                  TO MQOD-VERSION.
           MOVE 1                  TO MQOD-OBJECTTYPE.
           MOVE WS-QUEUE-NAME-TEXT TO MQOD-OBJECTNAME.

           CALL 'MQOPEN' USING WS-HCONN
                               MQOD
                               MQOO-INPUT-SHARED
                               WS-HOBJ
                               WS-COMP-CODE
                               WS-REASON-CODE.

           IF WS-COMP-CODE NOT = MQCC-OK
               DISPLAY 'FALHA NO MQOPEN'
               PERFORM MOSTRA-ERRO
               GOBACK.

           INITIALIZE MQMD.
           MOVE 'MD  '        TO MQMD-STRUCID.
           MOVE 1             TO MQMD-VERSION.
           MOVE 500           TO MQMD-CODEDCHARSETID.
           MOVE 'MQSTR   '    TO MQMD-FORMAT.

           INITIALIZE MQGMO.
           MOVE 'GMO '        TO MQGMO-STRUCID.
           MOVE 1             TO MQGMO-VERSION.
           MOVE MQGMO-NO-WAIT TO MQGMO-OPTIONS.

           MOVE 100           TO WS-MSG-LEN.

           CALL 'MQGET' USING WS-HCONN
                              WS-HOBJ
                              MQMD
                              MQGMO
                              WS-MSG-LEN
                              WS-MSG-TEXT
                              WS-MSG-DATA-LEN
                              WS-COMP-CODE
                              WS-REASON-CODE.

           IF WS-COMP-CODE = MQCC-OK
               DISPLAY 'SUCESSO - MENSAGEM LIDA!'
               DISPLAY ' '
               DISPLAY 'TAMANHOO: ' WS-MSG-DATA-LEN
               DISPLAY 'MENSAGEM: ' WS-MSG-TEXT
           ELSE
               IF WS-REASON-CODE = MQRC-NO-MSG-AVAILABLE
                   DISPLAY 'NENHUMA MENSAGEM DISPONIVEL NA FILA'
               ELSE
                   DISPLAY 'FALHA NO MQGET'
                   PERFORM MOSTRA-ERRO
               END-IF
           END-IF.

           CALL 'MQCLOSE' USING WS-HCONN WS-HOBJ MQCO-NONE
                                WS-COMP-CODE WS-REASON-CODE.
           CALL 'MQDISC'  USING WS-HCONN WS-COMP-CODE WS-REASON-CODE.

           DISPLAY '============================================'.
           GOBACK.

       MOSTRA-ERRO.
           MOVE WS-COMP-CODE TO WS-DISP-COMP.
           MOVE WS-REASON-CODE TO WS-DISP-REAS.
           DISPLAY 'ERRO : CC=' WS-DISP-COMP ' RC=' WS-DISP-REAS.
