with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces.C; use Interfaces.C;

procedure Main is

   type c_struct is record
      a : int;
      b : long;
      c : unsigned;
      d : double;
   end record
      with Convention => C;

    myStruct : c_struct;
begin

   myStruct.a := 5;
   myStruct.b := 10;
   myStruct.c := 15;
   myStruct.d := 20.00;
   Put_Line("---------------------------------------------------");
   Put_Line("                Structs from C");
   Put_Line("     A: " & int'Image(myStruct.a));
   Put_Line("     B: " & long'Image(myStruct.b));
   Put_Line("     C: " & unsigned'Image(myStruct.c));
   Put_Line("     D: " & double'Image(myStruct.d));
   Put_Line("---------------------------------------------------");
end Main;
