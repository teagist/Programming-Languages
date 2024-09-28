-------------------------------------------------------------------------------
-- Matrix Manipulation
--
--   The main program will begin by displaying a menu of options to the user
-- and prompting them to make a selection.  Once a valid option is made,
-- then the appropriate function will be called to carry out the user's
-- request.  To exit the program, the user can enter 0.
--
-- Last Update: 9/18/2021
-- Author: Houston Brown
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Sequential_IO;


procedure Main is

   ROW_MAX : constant := 10;
   COL_MAX : constant := 10;

   type MATRIX is array(Integer range 1..ROW_MAX, Integer range 1..COL_MAX)
     of Integer;

   File_Name: constant String := "matrix_vals.txt";
   infile : FILE_TYPE;



   ----------------------------------------------------------------------------
   -- Fill Matrix
   --
   --   The purpose of this function is to open the file containing the matrix
   -- values and populating the matrix.  If the file cannot be found then an
   -- appropriate message will be displayed.
   --
   -- Parameters
   -- number of rows (row) : reading and writing
   -- number of columns (col) : reading and writing
   -- flag for matrix being filled (is_filled) : reading and writing
   -- the matrix (my_matrix) : reading and writing
   ----------------------------------------------------------------------------
   procedure FillMatrix(row : in out Integer; col : in out Integer; is_filled :
                           in out Boolean; my_matrix : in out MATRIX) is
      I : Integer := 1;
      J : Integer := 1;
      n : Integer := 0;

   begin
      Open(infile, In_File, File_Name);

      Get(infile, row);
      Get(infile, col);

      for I in 1 .. row loop
         for J in 1 .. col loop
            Get(infile, n);
            my_matrix(I, J) := n;
         end loop;
      end loop;

      Close(infile);
      is_filled := True;
      Put_Line("The matrix has been populated from the file");

   exception
      when Name_Error =>
         Put_Line(Standard_Error, "File does not exist");
      when others =>
         Put_Line(Standard_Error, "Error while processing file");

   end FillMatrix;



   ----------------------------------------------------------------------------
   -- Print Matrix
   --
   --   The purpose of this function is to print the contents of the matrix.
   -- If the matrix has yet to be populated, then an appropriate message will
   -- be displayed.
   --
   -- Parameters
   -- number of rows (row) : reading only
   -- number of columns (col) : reading only
   -- the matrix (my_matrix) : reading only
   ----------------------------------------------------------------------------
   procedure PrintMatrix(row : in Integer; col : in Integer; my_matrix : in
                            MATRIX) is
      I : Integer := 0;
      J : Integer := 0;

   begin
      for I in 1 .. row loop
         for J in 1 .. col loop
            Put(my_matrix(I, J));
            Put(" ");
         end loop;
         Put_Line("");
      end loop;
   end PrintMatrix;



   ----------------------------------------------------------------------------
   -- Sum Matrix
   --
   --   This function will iterate through the matrix and find the sum of its
   -- contents.  Once this value has been calculated, then it will be returned
   -- to the caller.  If the matrix has not been populated, then an appropriate
   -- message will be displayed.
   --
   -- Parameters
   -- sum of the elements (sum) : reading and writing
   -- number of rows (row) : reading only
   -- number of columns (col) : reading only
   -- the matrix (my_matrix) : reading only
   ----------------------------------------------------------------------------
   procedure SumMatrix(sum : in out Integer; row : in Integer; col : in
                          Integer; my_matrix : in MATRIX) is
      I : Integer := 0;
      J : Integer := 0;

   begin
      for I in 1 .. Row loop
         for J in 1 .. Col loop
            sum := my_matrix(I, J) + sum;
         end loop;
      end loop;
   end SumMatrix;




   ----------------------------------------------------------------------------
   -- Stats Matrix
   --
   --   This function will iterate through the matrix and find the average
   -- value, the minimum value, and the maximum value.  Once these values have
   -- been calculated, then they will be returned to the caller.  If the matrix
   -- has not been populated, then an appropriate message will be displayed.
   --
   -- Parameters
   -- average of elements (avg) : reading and writing
   -- minimum value of the elements (min) : reading and writing
   -- maximum value of the elements (max) : reading and writing
   -- number of rows (row) : reading only
   -- number of columns (col) : reading only
   -- the matrix (my_matrix) : reading only
   ----------------------------------------------------------------------------
   procedure StatsMatrix(avg : in out Integer; min : in out Integer; max : in
                            out Integer; row : in Integer; col : in Integer;
                            my_matrix : in MATRIX) is
      num_elements : Integer := 0;
      I : Integer := 1;
      J : Integer := 1;
      mat_sum : Integer := 0;
      test : Boolean := False;

   begin
      for I in 1 .. row loop
         for J in 1 .. col loop
            mat_sum := my_matrix(I, J) + mat_sum;
            num_elements := num_elements + 1;

            if (my_matrix(I, J) < min) then
               min := my_matrix(I, J);
            end if;

            if (my_matrix(I,J) > max) then
               max := my_matrix(I,J);

            end if;

         end loop;
      end loop;

      avg := mat_sum / num_elements;
   end StatsMatrix;




   ----------------------------------------------------------------------------
   -- DiagMatrix
   --
   --   If the matrix is a square matrix (i.e., the number of rows and columns
   -- are equal) then this function will print only the diagonal values from
   -- the matrix.  If the matrix has not been populated, then an appropriate
   -- message will be displayed.
   --
   -- Parameters
   -- number of rows (row) : reading only
   -- number of columns (col) : reading only
   -- the matrix (my_matrix) : reading only
   ----------------------------------------------------------------------------
   procedure DiagMatrix(row : in Integer; col : in Integer; my_matrix : in
                           MATRIX) is
      column : Integer := 0;
      I : Integer := 0;
      J : Integer := 0;

   begin
      if row = col then
         Put("Diagonal Values: ");
            for I in 1 .. row loop
               for J in 1 .. col loop
                  null;
               end loop;

               column := column + 1;      -- Printing only the diagonal values
               Put(my_matrix(I, column));
               Put(" ");

            end loop;
         Put_Line("");
      else
          Put_Line(Standard_Error, "The array is not a square.");
      end if;
   end DiagMatrix;



   ----------------------------------------------------------------------------
   -- Print Menu
   --
   --   This function will display the available operations that can be
   -- performed on the matrix.
   ----------------------------------------------------------------------------
   procedure PrintMenu is
   begin
      Put_Line("               Matrix Main Menu               ");
      Put_Line(" ____________________________________________ ");
      Put_Line("| 1| Fill the matrix                         |");
      Put_Line("| 2| Print the contents of the matrix        |");
      Put_Line("| 3| Print the sum of the matrix             |");
      Put_Line("| 4| Print the stats of the matrix           |");
      Put_Line("| 5| Print the diagonal values of the matrix |");
      Put_Line("| 6| Display the main menu                   |");
      Put_Line("|__|_________________________________________|");
      Put_Line("");
      Put_Line("To quit, please enter '0'.");
      Put_Line("Please make a selection from these options");
      Put_Line("");

   end PrintMenu;



-------------------------------------------------------------------------------
-- Main Function
-------------------------------------------------------------------------------
   row : Integer := 0;
   col : Integer := 0;

   sum : Integer := 0;
   avg : Integer := 0;
   min_val : Integer := Integer'Last;
   max_val : Integer := Integer'First;

   my_matrix : MATRIX;

   is_filled : Boolean := False;
   option : Integer := -1;

begin
   PrintMenu;
   Put("> ");
   Get(option);

   while option /= 0 loop
      if option = 1 then
         FillMatrix(row, col, is_filled, my_matrix);
         Put_Line("");

      elsif option = 2 then
         if is_filled = True then
            PrintMatrix(row, col, my_matrix);
            Put_Line("");
         else
            Put_Line(Standard_Error, "Please fill the matrix first.");
            Put_Line("");
         end if;


      elsif option = 3 then
         if is_filled = True then
            SumMatrix(sum, row, col, my_matrix);

            Put("Sum: " &Integer' Image(sum));
            Put_Line("");
            Put_Line("");
         else
            Put_Line(Standard_Error, "Please fill the matrix first.");
            Put_Line("");
         end if;

      elsif option = 4 then
         if is_filled = True then
            StatsMatrix(avg, min_val, max_val, row, col, my_matrix);

            Put_Line("Average: " &Integer' Image(avg));
            Put_Line("Minimum Value: " &Integer' Image(min_val));
            Put_Line("Maximum Value: " &Integer' Image(max_val));
            Put_Line("");
         else
            Put_Line(Standard_Error, "Please fill the matrix first.");
            Put_Line("");
         end if;


      elsif option = 5 then
         if is_filled = True then
            DiagMatrix(row, col, my_matrix);
            Put_Line("");
         else
            Put_Line(Standard_Error, "Please fill the matrix first.");
            Put_Line("");
         end if;


      elsif option = 6 then
         PrintMenu;

      else
         Put_Line(Standard_Error, "Invalid entry, please try again.");
         Put_Line("");

      end if;
      Put("> ");
      Get(option);
   end loop;

   exception
      when Data_Error =>
         Put_Line(Standard_Error, "Invalid data entry, closing the program");

end Main;
