with Ada.Unchecked_Conversion;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces.C.Pointers;

package body Expat.Wrapper is
   use type Ada.Containers.Count_Type;
   use type Interfaces.C.Strings.chars_ptr;

   function Event (Parser: Parser_Type) return Event_Info
   is begin
      if Parser.Event_Queue.Length = 0 then
         raise No_Event;
      end if;
      return Parser.Event_Queue.Element(1);
   end Event;

   procedure Check_Status (Status: XML_Status) is
   begin
      case Status is
         when XML_STATUS_OK => null;
         when XML_STATUS_SUSPENDED => null;
         when XML_STATUS_ERROR =>
            raise Parse_Error;
      end case;
   end Check_Status;

   procedure Get_More_Events (Parser: in out Parser_Type)
   is
      Buffer: Stream_Element_Array (1..4096);
      Last: Stream_Element_Offset;
      Status: XML_Status;
      function Pun_To_Chars is new Ada.Unchecked_Conversion
        (System.Address, XML_LChar_p);
   begin
      if End_Of_File (Parser.File.all) then
         Parser.Event_Queue.Append ((Kind => End_Of_File));
      else
         Read (Parser.File.all, Buffer, Last);
         Status := XML_Parse (Parser.Parser, 
            s => Pun_To_Chars(Buffer'Address),
            len => C.int(Last),
            isFinal => To_XML_Bool(End_Of_File(Parser.File.all)));
         Check_Status (Status);
      end if;
   end Get_More_Events;

   procedure Next (Parser: in out Parser_Type)
   is begin
      if Parser.Event_Queue.Length > 0 then
         -- Pop event queue
         Parser.Event_Queue.Delete (1);
      end if;
      if Parser.Event_Queue.Length = 0 then
         -- Need more events.
         Get_More_Events (Parser);
      end if;
   end Next;

   function Get_Self (userData: Userdata_Access)
      return not null access Parser_Type
   is begin
      return UD_Type(userData.all).Parser;
   end Get_Self;

   function To_Unbounded_String (S: XML_Char_p)
      return Unbounded_String
   is begin
      return To_Unbounded_String(Interfaces.C.Strings.Value(S));
   end To_Unbounded_String;

   function To_Attributes (S_List: XML_Char_p_p)
      return Attribute_Vectors.Vector
   is
      type Pointless is array (Natural range <>) of aliased XML_Char_p;
      package Char_Pointers is new Interfaces.C.Pointers (
         Index          => Natural,
         Element        => XML_Char_p,
         Element_Array  => Pointless,
         Default_Terminator => null);
      use Char_Pointers;
      P: Char_Pointers.Pointer := Char_Pointers.Pointer(S_List);
      Dest: Attribute_Vectors.Vector;
      Name, Value: Unbounded_String;
   begin
      while P.all /= Interfaces.C.Strings.Null_Ptr loop
         Name := To_Unbounded_String(P.all);
         Increment (P);
         pragma Assert (P.all /= Interfaces.C.Strings.Null_Ptr);
         Value := To_Unbounded_String(P.all);
         Increment (P);
         Dest.Append ((Name, Value));
      end loop;
      return Dest;
   end To_Attributes;

   --- Callbacks ---

   procedure Start_Element_Handler (
      userData    : Userdata_Access;
      name        : XML_Char_p;
      atts        : XML_Char_p_p);
   pragma Convention (C, Start_Element_Handler);
   procedure Start_Element_Handler (
      userData    : Userdata_Access;
      name        : XML_Char_p;
      atts        : XML_Char_p_p)
   is
      Parser      : not null access Parser_Type := Get_Self (userData);
   begin
      Parser.Event_Queue.Append ((
         Kind        => Start_Element,
         Name        => To_Unbounded_String(name),
         Attributes  => To_Attributes(atts)));
   end Start_Element_Handler;

   procedure End_Element_Handler (
      userData    : Userdata_Access;
      name        : XML_Char_p);
   pragma Convention (C, End_Element_Handler);
   procedure End_Element_Handler (
      userData    : Userdata_Access;
      name        : XML_Char_p)
   is
      Parser      : not null access Parser_Type := Get_Self (userData);
   begin
      Parser.Event_Queue.Append ((
         Kind        => End_Element,
         Name        => To_Unbounded_String(name)));
   end End_Element_Handler;

   overriding procedure Initialize (Parser: in out Parser_Type)
   is begin
      Parser.Parser := XML_ParserCreate;
      XML_SetUserData (Parser.Parser, Parser.UD'Unchecked_Access);
      XML_SetElementHandler (Parser.Parser,
         Start_Element_Handler'Access,
         End_Element_Handler'Access);
   end Initialize;

   overriding procedure Finalize (Parser: in out Parser_Type)
   is begin
      XML_ParserFree (Parser.Parser);
   end Finalize;

end Expat.Wrapper;
