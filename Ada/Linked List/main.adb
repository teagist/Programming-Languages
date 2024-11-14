with Ada.Text_IO; use Ada.Text_IO;

procedure ListMech is
   --
   -- Define the list data structures
   --   Note: recursive data structs require forward declaration
   type Node;			-- forward declare
   type Link is access Node;	-- read literally: a "Link" accesses a "Node"

   type Node is			-- now declare the Node type
      record
         Node_Id: Integer;	-- prefer underbar to camel-case
         Next:    Link;
      end record;

   type List is
      record
         Head:   Link;		-- first node in list
         Tail:   Link;		-- last node in list
         Length: Integer := 0;	-- number of nodes in the list
      end record;

   Empty_Exception: exception;

   --
   -- Inserts an ID into the linked list
   --
   procedure Insert( Id: in Integer; Linked_List: in out List) is
   begin
      if Linked_List.Head = null then
         Linked_List.Head := new Node'(Id, null);
         Linked_List.Tail := Linked_List.Head;
         Linked_List.Length := 1;
         Put_Line("List empty => creating head node: " & Integer'Image(Id));
      else
         Put_Line("Creating node: "  & Integer'Image(Id));
         Linked_List.Tail.Next := new Node'(Id, null);
         Linked_List.Length := Linked_List.Length + 1;
         Linked_List.Tail := Linked_List.Tail.Next;
      end if;
   end Insert;

   --
   -- Removes and ID from the linked list (if available)
   --
   procedure Remove( Id: in Integer; Linked_List: in out List ) is
      Curr_Link: Link := Linked_List.Head;
      Prev_Link: Link := null;
      Found_Flag: Boolean := False;
   begin
      -- make sure list is not empty
      if Linked_List.Head = null then
         raise Empty_Exception;
      end if;

      while Curr_Link /= null loop
         if Curr_Link.Node_Id = Id then
            Put_Line("Removing node: "  & Integer'Image(Curr_Link.Node_Id));
            if Curr_Link = Linked_List.Head then
               Linked_List.Head := Curr_Link.Next;
            else
               Prev_Link.Next := Curr_Link.Next;
            end if;
            Found_Flag := true;
            Linked_List.Length := Linked_List.Length - 1;
            exit;
         else
            Prev_Link := Curr_Link;
            Curr_Link := Curr_Link.Next;
         end if;
      end loop;

      if Found_Flag /= true then
         Put_Line("Node not in list: "  & Integer'Image(Id));
      end if;
   end Remove;

   --
   -- Traverses the linked list and prints out the IDs at each node
   --
   procedure Walk( Linked_List: in List ) is
      Link_Index: Link := Linked_List.Head;
   begin
      Put_Line("Walking the list(" & Integer'Image(Linked_List.Length)& "): ");
      loop
         exit when Link_Index = null;
         Put_Line("--> " & Integer'Image(Link_Index.Node_Id));
         Link_Index := Link_Index.Next;
      end loop;
   end Walk;

   -- local variables
   Id_List: List;
   TestVector: array(Positive range <>) of Integer := (1, 2, 3, 4, 5);

begin

   for Index in TestVector'Range loop
      Insert( TestVector(Index), Id_List );
   end loop;
   New_Line;
   Walk( Id_List );
   New_Line;
   Remove( 1, Id_List );
   New_Line;
   Walk( Id_list );
   New_Line;
   for Index in TestVector'Range loop
      Remove( TestVector(Index), Id_List );
   end loop;

   -- list should be empty now
   New_Line;
   Remove( 9, Id_List );

exception
      when Empty_Exception => Put_Line("*** List is empty ***");
end ListMech;
