       IDENTIFICATION DIVISION.
       PROGRAM-ID. MQPUT01B.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  MQ-CONSTANTS.
           COPY CMQV.
           COPY CMQODV.
           COPY CMQMDV.
           COPY CMQPMOV.

       01  WS-QMGR-NAME           PIC X(48) VALUE 'CSQ7'.
       01  WS-QUEUE-NAME-TEXT     PIC X(48) VALUE SPACES.
       01  WS-MSG-LEN             PIC S9(9) COMP VALUE 100.
       01  WS-HCONN               PIC S9(9) COMP VALUE 0.
       01  WS-HOBJ                PIC S9(9) COMP VALUE 0.
       01  WS-COMP-CODE           PIC S9(9) COMP VALUE 0.
       01  WS-REASON-CODE         PIC S9(9) COMP VALUE 0.
       01  WS-DISP-COMP           PIC Z(9)9.
       01  WS-DISP-REAS           PIC Z(9)9.

       01  WS-FULL-MSG            PIC X(100) VALUE SPACES.
       01  WS-JOB-INFO            PIC X(50)  VALUE SPACES.
       01  WS-MSG-TEXT            PIC X(80)  VALUE SPACES.
       01  WS-TAM-REAL            PIC S9(9) COMP VALUE 0.

       01  WS-PARM-LEN            PIC S9(4) COMP VALUE 0.

       01  WS-MSG-LENGTH          PIC S9(9) COMP VALUE 0.

       LINKAGE SECTION.
       01  LS-PARM.
           05 LS-PARM-LEN         PIC S9(4) COMP.
           05 LS-PARM-DATA        PIC X(48).

       PROCEDURE DIVISION USING LS-PARM.
       MAIN-PROCESS.

           DISPLAY '============================================'.
           DISPLAY 'PROGRAMA MQPUT01B - ENVIO COM MQMSG'.
           DISPLAY '============================================'.

      *    OBTER FILA VIA PARM DO JCL
           IF LS-PARM-LEN > 0 AND LS-PARM-LEN <= 48
               MOVE LS-PARM-DATA(1:LS-PARM-LEN) TO WS-QUEUE-NAME-TEXT
               DISPLAY 'FILA INFORMADA VIA PARM: ' WS-QUEUE-NAME-TEXT
           ELSE
               DISPLAY 'INFORMAR A FILA NO PARM'
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.

      *    LER MENSAGEM DO CARTAO MQMSG
           ACCEPT WS-MSG-TEXT FROM SYSIN

      *    VERIFICAR SE A MENSAGEM FOI INFORMADA
           IF WS-MSG-TEXT = SPACES
               DISPLAY 'MENSAGEM VAZIA - INFORME NO CARTAO MQMSG'
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF.

      *    CALCULAR TAMANHO REAL DA MENSAGEM
           MOVE 0 TO WS-MSG-LENGTH
           INSPECT FUNCTION REVERSE(WS-MSG-TEXT)
               TALLYING WS-MSG-LENGTH FOR LEADING SPACES
           COMPUTE WS-MSG-LENGTH = 80 - WS-MSG-LENGTH

           DISPLAY 'MENSAGEM INFORMADA: ' WS-MSG-TEXT(1:WS-MSG-LENGTH)
           DISPLAY 'TAMANHO: ' WS-MSG-LENGTH

      *    MONTAR IDENTIFICACAO DO JOB
           STRING 'TM: ' DELIMITED BY SIZE
                  FUNCTION CURRENT-DATE(1:16)
                  DELIMITED BY SIZE
                  INTO WS-JOB-INFO
           END-STRING.

           DISPLAY 'IDENTIFICACAO: ' WS-JOB-INFO.

      *    MONTAR MENSAGEM COMPLETA
           STRING WS-JOB-INFO  DELIMITED BY '  '
                  ' | '        DELIMITED BY SIZE
                  WS-MSG-TEXT(1:WS-MSG-LENGTH)
                               DELIMITED BY SIZE
                  INTO WS-FULL-MSG
           END-STRING.

      *    EXIBIR COMO FICOU
           DISPLAY ' '.
           DISPLAY '--- MENSAGEM COMPLETA ---'.
           DISPLAY '[' WS-FULL-MSG ']'.
           DISPLAY '--------------------------'.
           DISPLAY ' '.

      *    USAR TAMANHO REAL
           MOVE WS-MSG-LENGTH TO WS-MSG-LEN.

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
                               MQOO-OUTPUT
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
           MOVE 8             TO MQMD-MSGTYPE.
           MOVE -1            TO MQMD-EXPIRY.
           MOVE 273           TO MQMD-ENCODING.
           MOVE 500           TO MQMD-CODEDCHARSETID.
           MOVE 'MQSTR   '    TO MQMD-FORMAT.

           MOVE 'PMO '        TO MQPMO-STRUCID.
           MOVE 1             TO MQPMO-VERSION.
           MOVE MQPMO-NONE    TO MQPMO-OPTIONS.

           CALL 'MQPUT' USING WS-HCONN
                              WS-HOBJ
                              MQMD
                              MQPMO
                              WS-MSG-LEN
                              WS-FULL-MSG
                              WS-COMP-CODE
                              WS-REASON-CODE.

           IF WS-COMP-CODE NOT = MQCC-OK
               DISPLAY 'FALHA NO MQPUT'
               PERFORM MOSTRA-ERRO
           ELSE
               DISPLAY 'SUCESSO - MENSAGEM ENVIADA!'.

           CALL 'MQCLOSE' USING WS-HCONN WS-HOBJ MQCO-NONE
                                WS-COMP-CODE WS-REASON-CODE.
           CALL 'MQDISC'  USING WS-HCONN WS-COMP-CODE WS-REASON-CODE.

           DISPLAY '============================================'.
           GOBACK.

       MOSTRA-ERRO.
           MOVE WS-COMP-CODE TO WS-DISP-COMP.
           MOVE WS-REASON-CODE TO WS-DISP-REAS.
           DISPLAY 'ERRO : CC=' WS-DISP-COMP ' RC=' WS-DISP-REAS.