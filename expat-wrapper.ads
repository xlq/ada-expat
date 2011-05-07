-- A safe interface to Expat.
-- Instead of event handlers, this wrapper returns events one by one.
-- This allows the event consumer to maintain control flow.

-- TODO: Handle more events.
-- TODO: Handle parse errors.

with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Streams.Stream_IO;

package Expat.Wrapper is

   package String_Vectors is new Ada.Containers.Vectors
     (Positive, Unbounded_String);

   type Attribute is record
      Name, Value: Unbounded_String;
   end record;
   package Attribute_Vectors is new Ada.Containers.Vectors
     (Positive, Attribute);

   -- A parser is tied to a Stream_IO.File_Type, to read from.
   -- Currently only Stream_IO is supported as input.
   type Parser_Type (File: not null access Ada.Streams.Stream_IO.File_Type)
      is new Ada.Finalization.Limited_Controlled with private;

   type Event_Kind is (
      End_Of_File,
      Start_Element,
      End_Element,
      Character_Data,
      Comment,
      Start_Cdata,
      End_Cdata);

   type Event_Type (Kind: Event_Kind := End_Of_File) is
   record
      case Kind is
         when End_Of_File =>
            null;
         when Start_Element | End_Element =>
            Name        : Unbounded_String;
            case Kind is
               when Start_Element =>
                  Attributes  : Attribute_Vectors.Vector;
               when others => null;
            end case;
         when Character_Data | Comment =>
            Text        : Unbounded_String;
         when Start_Cdata | End_Cdata =>
            null;
      end case;
   end record;

   package Event_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Event_Type);

   No_Event: exception;
   Parse_Error: exception;

   -- Return current event (raise No_Event if no current event).
   function Event (Parser: Parser_Type) return Event_Type;

   procedure Next (Parser: in out Parser_Type);

   overriding procedure Initialize (Parser: in out Parser_Type);
   overriding procedure Finalize (Parser: in out Parser_Type);

private

   type UD_Type (Parser: not null access Parser_Type)
      is new Userdata with null record;

   type UD_Access is access all UD_Type;

   type Parser_Type (File: access Ada.Streams.Stream_IO.File_Type)
      is new Ada.Finalization.Limited_Controlled with
   record
      Parser         : XML_Parser;
      Event_Queue    : Event_Vectors.Vector;
      UD             : aliased UD_Type (Parser_Type'Access);
   end record;

end Expat.Wrapper;
