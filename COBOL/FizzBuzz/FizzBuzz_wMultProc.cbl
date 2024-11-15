      ******************************************************************
      * Author: Houston Brown
      * Date: 10/16/2024
      * Purpose: Implement a FizzBuzz system, where the program
      *          displays the numbers 1-100, replacing every multiple
      *          of 3 with the word "Fizz", every multiple of 5 with
      *          "Buzz", and every multiple of both with "FizzBuzz".
      *          This will be done in a separate procedure from the
      *          main procedure and the number of displayed numbers
      *          will be determined by the user.
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

       I-O-CONTROL.
      *-----------------------

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.

      *-----------------------
       WORKING-STORAGE SECTION.
       01   CTR                PIC 999 VALUE 1.
       01   STMT               PIC X(100).
       01   BOUND              PIC 999 VALUE 0.
       01   FIZZ-PARAM         PIC 9 VALUE 3.
       01   BUZZ-PARAM         PIC 9 VALUE 5.
      *-----------------------

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

            DISPLAY "FizzBuzz System"
            DISPLAY " "
            DISPLAY "Enter a number to be the upper limit of the system"
            ACCEPT BOUND.

            PERFORM DISP-PROCEDURE

            STOP RUN.
      ** add other procedures here

       DISP-PROCEDURE.

               PERFORM VARYING CTR FROM 1 BY 1 UNTIL CTR > BOUND
                   EVALUATE TRUE
                       WHEN FUNCTION MOD(CTR, FIZZ-PARAM) = 0 AND
                            FUNCTION MOD(CTR, BUZZ-PARAM) = 0
                           MOVE "FizzBuzz" TO STMT
                       WHEN FUNCTION MOD(CTR, FIZZ-PARAM) = 0
                           MOVE "Fizz" TO STMT
                       WHEN FUNCTION MOD(CTR, BUZZ-PARAM) = 0
                           MOVE "Buzz" TO STMT
                       WHEN OTHER
                           MOVE CTR TO STMT
                   END-EVALUATE
                   DISPLAY STMT
               END-PERFORM.
           EXIT PROGRAM.

       END PROGRAM FIZZBUZZ.
