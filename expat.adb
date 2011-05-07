package body Expat is
   function To_XML_Bool (X: Boolean) return XML_Bool
   is begin
      case X is
         when True => return 1;
         when False => return 0;
      end case;
   end To_XML_Bool;

   function To_XML_Bool (X: Boolean) return C.int
   is begin
      case X is
         when True => return 1;
         when False => return 0;
      end case;
   end To_XML_Bool;
end Expat;
