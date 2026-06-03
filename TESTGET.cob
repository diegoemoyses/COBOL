       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTGET.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-RESPONSE-CODE     PIC S9(8) COMP.
       01 WS-RESPONSE-CODE2    PIC S9(8) COMP.
       01 WS-TOKEN             PIC X(8).

      * Dados da resposta
       01 WS-JSON-RESPONSE     PIC X(2000).
       01 WS-RESP-LEN          PIC S9(8) COMP.

      * HTTP Status
       01 WS-HTTP-STATUS       PIC S9(8) COMP.
       01 WS-HTTP-STATTEXT     PIC X(100).
       01 WS-STATTEXT-LEN      PIC S9(8) COMP VALUE 100.

       PROCEDURE DIVISION.

           DISPLAY 'Consultando ISS Location...'

      * 1. Abrir conexao HTTP
           EXEC CICS WEB OPEN
               URIMAP('URIGET')
               SESSTOKEN(WS-TOKEN)
               RESP(WS-RESPONSE-CODE)
               RESP2(WS-RESPONSE-CODE2)
           END-EXEC

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               DISPLAY 'Erro WEB OPEN'
               DISPLAY 'RESP : ' WS-RESPONSE-CODE
               DISPLAY 'RESP2: ' WS-RESPONSE-CODE2
               EXEC CICS RETURN END-EXEC
           END-IF

           DISPLAY 'WEB OPEN OK - Token: ' WS-TOKEN

      * 2. Enviar GET e receber resposta
           EXEC CICS WEB CONVERSE
               SESSTOKEN(WS-TOKEN)
               METHOD(DFHVALUE(GET))
               INTO(WS-JSON-RESPONSE)
               MAXLENGTH(2000)
               TOLENGTH(WS-RESP-LEN)
               STATUSCODE(WS-HTTP-STATUS)
               STATUSTEXT(WS-HTTP-STATTEXT)
               STATUSLEN(WS-STATTEXT-LEN)
               RESP(WS-RESPONSE-CODE)
               RESP2(WS-RESPONSE-CODE2)
           END-EXEC

           DISPLAY '============================================'
           DISPLAY 'RESULTADO:'
           DISPLAY 'CICS RESP : ' WS-RESPONSE-CODE
           DISPLAY 'CICS RESP2: ' WS-RESPONSE-CODE2
           DISPLAY 'HTTP STATUS: ' WS-HTTP-STATUS
           DISPLAY 'RESP DADOS : ' 
               WS-JSON-RESPONSE(1:WS-RESP-LEN)
           DISPLAY '============================================'

      * 3. Fechar conexao
           EXEC CICS WEB CLOSE
               SESSTOKEN(WS-TOKEN)
               RESP(WS-RESPONSE-CODE2)
           END-EXEC

           DISPLAY 'Programa finalizado.'
           EXEC CICS RETURN END-EXEC.