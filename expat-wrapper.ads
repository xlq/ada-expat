-- A safe interface to Expat.
-- Instead of event handlers, this wrapper returns events one by one.
-- This allows the event consumer to maintain control flow.

-- Expat.Wrapper gives Expat a little XML at a time, and collects the
-- generated events in a queue. Once Expat has finished with its piece
-- of XML, it returns, allowing the caller to process the events in
-- its own time.
-- This allows the XML consumer to look something like this:
--    if Event is <outer> then
--       Parser.Next;
--       if Event is <inner> then
--          Parser.Next
--          ...
-- instead of:
--    procedure On_Start_Tag is begin
--       case State is
--          when Found_Outer => (look for inner)
--          when Found_Inner => ...
--          ...
-- making it much easier to process XML.

-- TODO: Handle more events.
-- TODO: Handle parse errors.

with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Streams.Stream_IO;

package Expat.Wrapper is

   type Attribute is record
      Name, Value: Unbounded_String;
   end record;
   package Attribute_Vectors is new Ada.Containers.Vectors
     (Positive, Attribute);

   -- A parser is tied to a Stream_IO.File_Type, to read from.
   -- Currently only Stream_IO is supported as input.
   type Parser_Type (File: not null access Ada.Streams.Stream_IO.File_Type)
      is new Ada.Finalization.Limited_Controlled with private;

   -- Kind of parse event.
   type Event_Kind is (
      End_Of_File,
      Start_Element,
      End_Element,
      Character_Data,
      Comment,
      Start_Cdata,
      End_Cdata);
   
   type Line_Number is new Natural;
   type Column_Number is new Natural;
   type File_Location is record
      Line: Line_Number;
      Column: Column_Number;
   end record;

   -- Information about an event.
   type Event_Info (Kind: Event_Kind := End_Of_File) is
   record
      Where: File_Location;
      case Kind is
         when Start_Element | End_Element =>
            Name: Unbounded_String;
            case Kind is
               when Start_Element =>
                  Attributes: Attribute_Vectors.Vector;
               when others => null;
            end case;
         when Character_Data | Comment =>
            Text: Unbounded_String;
         when End_Of_File | Start_Cdata | End_Cdata => null;
      end case;
   end record;

   No_Event: exception;
   Parse_Error: exception;

   -- Return current event (raise No_Event if no current event).
   function Event (Parser: Parser_Type) return Event_Info;

   procedure Next (Parser: in out Parser_Type);

   overriding procedure Initialize (Parser: in out Parser_Type);
   overriding procedure Finalize (Parser: in out Parser_Type);

private

   function Pos (Parser: Parser_Type) return File_Location;

   package Event_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Event_Info);

   -- The userdata that is passed to Expat.
   type UD_Type (Parser: not null access Parser_Type)
      is new Userdata with null record;

   type Parser_Type (File: access Ada.Streams.Stream_IO.File_Type)
      is new Ada.Finalization.Limited_Controlled with
   record
      Parser         : XML_Parser;
      Event_Queue    : Event_Vectors.Vector;
      UD             : aliased UD_Type (Parser_Type'Access);
   end record;

end Expat.Wrapper;
