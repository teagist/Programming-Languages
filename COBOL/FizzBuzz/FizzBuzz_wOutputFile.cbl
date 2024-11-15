      ******************************************************************
      * Author: Houston Brown
      * Date: 10/16/2024
      * Purpose: Implement a FizzBuzz system, where the program
      *          displays the numbers 1-100, replacing every multiple
      *          of 3 with the word "Fizz", every multiple of 5 with
      *          "Buzz", and every multiple of both with "FizzBuzz".
      *          But it will also print all results to an output file
      *          for verification.  The file will be a line sequential
      *          text file.
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. FIZZBUZZ.

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. AMD-Ryzen7.
       OBJECT-COMPUTER. AMD-Ryzen7.
       SPECIAL-NAMES. ALPHABET ALPHA-NAME IS STANDARD-1.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OutputFile ASSIGN TO "output.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           STATUS FileStatus.

       I-O-CONTROL.
      *-----------------------

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD  OutputFile.
       01  OutputRecord        PIC X(100).
      *-----------------------
       WORKING-STORAGE SECTION.
       01   CTR                PIC 999 VALUE 1.
       01   STMT               PIC X(100).

       01  FileStatus          PIC XX.
      *-----------------------

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
            DISPLAY "----- FizzBuzz System -----"
            DISPLAY " "
            DISPLAY "----------"
            DISPLAY "    Info: Attempting to open output file to write"
      -              " results."

           OPEN OUTPUT OutputFile
           IF FileStatus NOT = "00"
               Display "    Error: Unable to open the output file."
               Display "    Status: " FileStatus
               DISPLAY "----------"
               DISPLAY " "
               STOP RUN
           ELSE
               Display "    Success: Output file is open."
               DISPLAY "----------"
               DISPLAY " "
           END-IF

           DISPLAY "Displaying the numbers 1-100."
            PERFORM VARYING CTR FROM 1 BY 1 UNTIL CTR > 100

                EVALUATE TRUE
                    WHEN FUNCTION MOD(CTR, 3) = 0 AND
                    FUNCTION MOD(CTR, 5) = 0
                       MOVE "FizzBuzz" TO STMT
                    WHEN FUNCTION MOD(CTR, 3) = 0
                       MOVE "Fizz" TO STMT
                    WHEN FUNCTION MOD (CTR, 5) = 0
                       MOVE "Buzz" TO STMT
                    WHEN OTHER
                       MOVE CTR TO STMT
                END-EVALUATE

                DISPLAY STMT
                MOVE STMT TO OutputRecord
                WRITE OutputRecord
            END-PERFORM.
            CLOSE OutputFile.

            STOP RUN.
      ** add other procedures here
       END PROGRAM FIZZBUZZ.
