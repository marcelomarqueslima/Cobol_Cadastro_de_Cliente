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
       01  ST-FILE  PIC XX.
       01  X        PIC X.

       77  BANDERA  PIC 9.
       01  SALDO-Z  PIC Z(7)9,99.
       01  SALDO-ZZ PIC ------9,99.

       PROCEDURE DIVISION.
       INICIO-PROGRAMA.
            PERFORM ABRO-ARQUIVO.
            PERFORM LER-DADOS THRU F-LER-DADOS.
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
           MOVE 0  TO CLI_SALDO.
           MOVE "VAR-NOME" TO CLI_NOME.
           MOVE "VAR-DIRECAO" TO CLI_DIRECAO.

       GRAVA-REGISTRO.
           WRITE REG-CLIENTES.
           IF ST-FILE = "99" GO TO GRAVA-DADO.
           IF ST-FILE > "07"
               DISPLAY "ERROR GRAVANDO O ARQUIVO".

       F-GRAVA-DADO.
           EXIT.

       LER-DADOS.
           INITIALIZE REG-CLIENTES.
           START CLIENTES KEY IS NOT LESS THAN ID_CLIENTE.
           READ CLIENTES NEXT RECORD.
           IF ST-FILE = "99" GO TO LER-DADOS.
           IF ST-FILE > "07"
               DISPLAY "ERROR LENDO O ARQUIVO".
       MOSTRA-DADOS.
           MOVE -15,58 TO SALDO-ZZ.
           DISPLAY "ID:" LINE 10 COL 5.
           DISPLAY "SALDO:" LINE 11 COL 5.
           DISPLAY "NOME:" LINE 12 COL 5.
           DISPLAY "DIRECAO:" LINE 13 COL 5.

           DISPLAY CLI_ID LINE 10 COL 30.
           DISPLAY SALDO-ZZ LINE 11 COL 30.
           DISPLAY CLI_NOME LINE 12 COL 30.
           DISPLAY CLI_DIRECAO LINE 13 COL 30.
           ACCEPT X LINE 14 COL 70.
       F-LER-DADOS.
           EXIT.
       END PROGRAM INICIO.
