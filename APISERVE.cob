       IDENTIFICATION DIVISION.
       PROGRAM-ID. APISERVE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-RESPONSE-CODE     PIC S9(8) COMP.
       01 WS-RESPONSE-CODE2    PIC S9(8) COMP.

      * Dados recebidos
       01 WS-REQUEST-DATA      PIC X(500).
       01 WS-REQUEST-LEN       PIC S9(8) COMP.

      * Dados de resposta
       01 WS-RESPONSE-JSON     PIC X(500).
       01 WS-RESPONSE-LEN      PIC S9(8) COMP.

      * Headers HTTP
       01 WS-MEDIA-TYPE        PIC X(50) 
           VALUE 'application/json'.
       01 WS-CHARSET           PIC X(12) VALUE 'UTF-8'.
       01 WS-HTTP-STATUS       PIC S9(8) COMP VALUE 200.
       01 WS-HTTP-STATUS-MSG   PIC X(20) VALUE 'OK'.

      * Dados do CICS
       01 WS-CICS-JOB          PIC X(8).
       01 WS-CICS-USER         PIC X(8).
       01 WS-CICS-TIME         PIC X(8).
       01 WS-CICS-DATE         PIC X(8).

      * Variáveis auxiliares
       01 WS-TEMP-LEN          PIC S9(8) COMP.

       PROCEDURE DIVISION.

      * Obter informações do ambiente CICS
           EXEC CICS ASSIGN
               USERID(WS-CICS-USER)
               APPLID(WS-CICS-JOB)
               RESP(WS-RESPONSE-CODE)
           END-EXEC

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'CICSA02' TO WS-CICS-JOB
               MOVE 'CICSUSR' TO WS-CICS-USER
           END-IF

           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CICS-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CICS-TIME

      * Receber dados da requisição (se houver)
           EXEC CICS WEB RECEIVE
               INTO(WS-REQUEST-DATA)
               MAXLENGTH(500)
               LENGTH(WS-REQUEST-LEN)
               RESP(WS-RESPONSE-CODE)
           END-EXEC

      * Montar JSON de resposta
           MOVE SPACES TO WS-RESPONSE-JSON

           STRING 
               '{"status":"ok","message":"API CICS OK!",'
               '"data":"'          WS-CICS-DATE '"'
               ',"time":"'         WS-CICS-TIME(1:6) '"'
               ',"cics_job":"'     WS-CICS-JOB  '"'
               ',"cics_user":"'    WS-CICS-USER '"'
               ',"timestamp":"2026-06-02T00:00:00Z"}'
               DELIMITED BY SIZE 
               INTO WS-RESPONSE-JSON
           END-STRING

      * Calcular tamanho real
           MOVE 0 TO WS-RESPONSE-LEN
           MOVE 500 TO WS-TEMP-LEN
           PERFORM UNTIL WS-TEMP-LEN = 0
               IF WS-RESPONSE-JSON(WS-TEMP-LEN:1) NOT = SPACE
                   MOVE WS-TEMP-LEN TO WS-RESPONSE-LEN
                   MOVE 0 TO WS-TEMP-LEN
               ELSE
                   SUBTRACT 1 FROM WS-TEMP-LEN
               END-IF
           END-PERFORM

           IF WS-RESPONSE-LEN = 0
               MOVE 160 TO WS-RESPONSE-LEN
           END-IF

      * Enviar resposta HTTP
           EXEC CICS WEB SEND
               FROM(WS-RESPONSE-JSON)
               FROMLENGTH(WS-RESPONSE-LEN)
               MEDIATYPE(WS-MEDIA-TYPE)
               CHARACTERSET(WS-CHARSET)
               STATUSCODE(WS-HTTP-STATUS)
               STATUSTEXT(WS-HTTP-STATUS-MSG)
               STATUSLEN(2)
               RESP(WS-RESPONSE-CODE)
               RESP2(WS-RESPONSE-CODE2)
           END-EXEC

           DISPLAY 'Resposta enviada: ' WS-RESPONSE-JSON(1:WS-RESPONSE-LEN)

           EXEC CICS RETURN END-EXEC.