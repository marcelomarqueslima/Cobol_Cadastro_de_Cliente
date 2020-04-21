      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INICIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL CLIENTES ASSIGN TO "./clientes.dat"
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS ID_CLIENTE
                  ALTERNATE KEY CLI_NOME WITH DUPLICATES
                  ALTERNATE KEY CLI_ALT_BUSCA WITH DUPLICATES
                  STATUS ST-FILE.
       DATA DIVISION.
       FILE SECTION.

       FD CLIENTES.

       01  REG-CLIENTES.
           03 ID_CLIENTE.
               05 CLI_ID                 PIC 9(8).
           03 CLI_SALDO                  PIC S9(8)V9(3).
           03 CLI_NOME                   PIC X(60).
           03 CLI_DIRECAO                PIC X(80).
           03 CLI_CEP                    PIC X(10).
           03 CLI_CATEGORIA              PIC X.
           03 CLI_ALT_BUSCA.
               05 CLI_CATEGORIA_BUSCA    PIC X.
               05 CLI_NOME_BUSCA         PIC X(60).
           03 CLI_RAZAOSOCIAL            PIC X(60).
           03 FILLER                     PIC X(240).
       WORKING-STORAGE SECTION.
       01  ST-FILE PIC XX.
       PROCEDURE DIVISION.
       INICIO-PROGRAMA.
            PERFORM ABRO-ARQUIVO.
            PERFORM GRAVA-DADO THRU F-GRAVA-DADO.
            PERFORM FECHA-ARQUIVO.
            STOP RUN.
       ABRO-ARQUIVO.
           OPEN I-O CLIENTES.
           IF ST-FILE > "07"
               DISPLAY "ERROR ABRINDO O ARQUIVO".
       FECHA-ARQUIVO.
           CLOSE CLIENTES.
       GRAVA-DADO.
           INITIALIZE REG-CLIENTES.
           MOVE 1  TO CLI_ID.
           MOVE 100  TO CLI_SALDO.
           MOVE "MARCELO" TO CLI_NOME.
           MOVE "SUL" TO CLI_DIRECAO.

       GRAVA-REGISTRO.
           WRITE REG-CLIENTES.
           IF ST-FILE = "99" GO TO GRAVA-DADO.
           IF ST-FILE > "07"
               DISPLAY "ERROR GRAVANDO O ARQUIVO".

       F-GRAVA-DADO.
           EXIT.

       END PROGRAM INICIO.
