with Expat.Wrapper; use Expat.Wrapper;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test is
   F: aliased Ada.Streams.Stream_IO.File_Type;
   Parser: Expat.Wrapper.Parser_Type (F'Access);
   Event: Event_Info;
begin
   Open (F, In_File, "/usr/share/xcb/xproto.xml");
   Parser.Next;
   loop
      Event := Parser.Event;
      exit when Event.Kind = End_Of_File;
      Put_Line (Event_Kind'Image(Event.Kind));
      case Event.Kind is
         when Start_Element
         | End_Element =>
            Put_Line (To_String(Event.Name));
         when others => null;
      end case;
      Parser.Next;
   end loop;
   Close (F);
end Test;
