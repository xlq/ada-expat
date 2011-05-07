-- This is the C interface to expat.
-- It was manually translated from expat.h.
-- For a nicer interface, please use Expat.Wrapper.

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package Expat is
   package C renames Interfaces.C;

   pragma Linker_Options ("-lexpat");

   type XML_ParserStruct is private;
   type XML_Parser is access XML_ParserStruct;
   pragma Convention (C, XML_Parser);

   subtype XML_Bool is C.unsigned_char;
   XML_TRUE: constant XML_Bool := 1;
   XML_FALSE: constant XML_Bool := 0;

   -- XXX: These will be incorrect if XML_LARGE_SIZE is defined.
   subtype XML_Index is C.long;
   subtype XML_Size is C.unsigned_long;

   subtype XML_Char is C.char;
   subtype XML_LChar is C.char;
   subtype XML_Char_p is C.Strings.chars_ptr;
   subtype XML_LChar_p is C.Strings.chars_ptr;
   type XML_Char_p_p is access XML_Char_p;
   type XML_LChar_p_p is access XML_LChar_p;

   type XML_Status is (
      XML_STATUS_ERROR,
      XML_STATUS_OK,
      XML_STATUS_SUSPENDED);
   pragma Convention (C, XML_Status);

   type XML_Error is (
      XML_ERROR_NONE,
      XML_ERROR_NO_MEMORY,
      XML_ERROR_SYNTAX,
      XML_ERROR_NO_ELEMENTS,
      XML_ERROR_INVALID_TOKEN,
      XML_ERROR_UNCLOSED_TOKEN,
      XML_ERROR_PARTIAL_CHAR,
      XML_ERROR_TAG_MISMATCH,
      XML_ERROR_DUPLICATE_ATTRIBUTE,
      XML_ERROR_JUNK_AFTER_DOC_ELEMENT,
      XML_ERROR_PARAM_ENTITY_REF,
      XML_ERROR_UNDEFINED_ENTITY,
      XML_ERROR_RECURSIVE_ENTITY_REF,
      XML_ERROR_ASYNC_ENTITY,
      XML_ERROR_BAD_CHAR_REF,
      XML_ERROR_BINARY_ENTITY_REF,
      XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF,
      XML_ERROR_MISPLACED_XML_PI,
      XML_ERROR_UNKNOWN_ENCODING,
      XML_ERROR_INCORRECT_ENCODING,
      XML_ERROR_UNCLOSED_CDATA_SECTION,
      XML_ERROR_EXTERNAL_ENTITY_HANDLING,
      XML_ERROR_NOT_STANDALONE,
      XML_ERROR_UNEXPECTED_STATE,
      XML_ERROR_ENTITY_DECLARED_IN_PE,
      XML_ERROR_FEATURE_REQUIRES_XML_DTD,
      XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING,
      XML_ERROR_UNBOUND_PREFIX,
      XML_ERROR_UNDECLARING_PREFIX,
      XML_ERROR_INCOMPLETE_PE,
      XML_ERROR_XML_DECL,
      XML_ERROR_TEXT_DECL,
      XML_ERROR_PUBLICID,
      XML_ERROR_SUSPENDED,
      XML_ERROR_NOT_SUSPENDED,
      XML_ERROR_ABORTED,
      XML_ERROR_FINISHED,
      XML_ERROR_SUSPEND_PE,
      XML_ERROR_RESERVED_PREFIX_XML,
      XML_ERROR_RESERVED_PREFIX_XMLNS,
      XML_ERROR_RESERVED_NAMESPACE_URI);
   pragma Convention (C, XML_Error);

   type XML_Content_Type is (
      XML_CTYPE_EMPTY,
      XML_CTYPE_ANY,
      XML_CTYPE_MIXED,
      XML_CTYPE_NAME,
      XML_CTYPE_CHOICE,
      XML_CTYPE_SEQ);
   for XML_Content_Type use (
      XML_CTYPE_EMPTY         => 1,
      XML_CTYPE_ANY           => 2,
      XML_CTYPE_MIXED         => 3,
      XML_CTYPE_NAME          => 4,
      XML_CTYPE_CHOICE        => 5,
      XML_CTYPE_SEQ           => 6);
   pragma Convention (C, XML_Content_Type);

   type XML_Content_Quant is (
      XML_CQUANT_NONE,
      XML_CQUANT_OPT,
      XML_CQUANT_REP,
      XML_CQUANT_PLUS);
   pragma Convention (C, XML_Content_Quant);

   type XML_Content;
   type XML_Content_p is access XML_Content;
   pragma Convention (C, XML_Content_p);

   type XML_Content is record
      ctype       : XML_Content_Type;
      quant       : XML_Content_Quant;
      name        : XML_Char_p;
      numchildren : C.unsigned;
      children    : XML_Content_p;
   end record;
   pragma Convention (C, XML_Content);

   -- Declare tagged base class for userdata to allow safe downcasting.
   type Userdata is tagged null record;
   type Userdata_Access is access all Userdata'Class;
   pragma Convention (C, Userdata_Access);

   type XML_ElementDeclHandler is access procedure (
      userData    : Userdata_Access;
      name        : XML_Char_p;
      model       : XML_Content_p);
   pragma Convention (C, XML_ElementDeclHandler);

   procedure XML_SetElementDeclHandler (
      parser      : XML_Parser;
      eldecl      : XML_ElementDeclHandler);
   pragma Import (C, XML_SetElementDeclHandler, "XML_SetElementDeclHandler");

   type XML_AttlistDeclHandler is access procedure (
      userData    : Userdata_Access;
      elname      : XML_Char_p;
      attname     : XML_Char_p;
      att_type    : XML_Char_p;
      dflt        : XML_Char_p;
      isrequired  : C.int);
   pragma Convention (C, XML_AttlistDeclHandler);

   procedure XML_SetAttlistDeclHandler (
      parser      : XML_Parser;
      attdecl     : XML_AttlistDeclHandler);
   pragma Import (C, XML_SetAttlistDeclHandler, "XML_SetAttlistDeclHandler");

   type XML_XmlDeclHandler is access procedure (
      userData    : Userdata_Access;
      version     : XML_Char_p;
      encoding    : XML_Char_p;
      standalone  : C.int);
   pragma Convention (C, XML_XmlDeclHandler);

   procedure XML_SetXmlDeclHandler (
      parser      : XML_Parser;
      xmldecl     : XML_XmlDeclHandler);
   pragma Import (C, XML_SetXmlDeclHandler, "XML_SetXmlDeclHandler");

   type XML_Memory_Handling_Suite is record
      malloc_fcn  : access function (size: C.size_t) return System.Address;
      realloc_fcn : access function (ptr: System.Address; size: C.size_t) return System.Address;
      free_fcn    : access procedure (ptr: System.Address);
   end record;
   pragma Convention (C, XML_Memory_Handling_Suite);

   function XML_ParserCreate (encoding: XML_Char_p := C.Strings.Null_Ptr)
      return XML_Parser;
   pragma Import (C, XML_ParserCreate, "XML_ParserCreate");

   function XML_ParserCreateNS (
      encoding             : XML_Char_p;
      namespaceSeparator   : XML_Char)
      return XML_Parser;
   pragma Import (C, XML_ParserCreateNS, "XML_ParserCreateNS");

   -- Some functions omitted --

   function XML_ParserReset (
      parser      : XML_Parser;
      encoding    : XML_Char_p)
      return XML_Bool;
   pragma Import (C, XML_ParserReset, "XML_ParserReset");

   type XML_StartElementHandler is access procedure (
      userData    : Userdata_Access;
      name        : XML_Char_p;
      atts        : XML_Char_p_p);
   pragma Convention (C, XML_StartElementHandler);

   type XML_EndElementHandler is access procedure (
      userData    : Userdata_Access;
      name        : XML_Char_p);
   pragma Convention (C, XML_EndElementHandler);

   type XML_CharacterDataHandler is access procedure (
      userData    : Userdata_Access;
      s           : XML_Char_p;
      len         : C.int);
   pragma Convention (C, XML_CharacterDataHandler);

   type XML_ProcessingInstructionHandler is access procedure (
      userData    : Userdata_Access;
      target      : XML_Char_p;
      data        : XML_Char_p);
   pragma Convention (C, XML_ProcessingInstructionHandler);

   type XML_CommentHandler is access procedure (
      userData    : Userdata_Access;
      data        : XML_Char_p);
   pragma Convention (C, XML_CommentHandler);

   type XML_StartCdataSectionHandler is access procedure (
      userData    : Userdata_Access);
   pragma Convention (C, XML_StartCdataSectionHandler);

   type XML_EndCdataSectionHandler is access procedure (
      userData    : Userdata_Access);
   pragma Convention (C, XML_EndCdataSectionHandler);

   type XML_DefaultHandler is access procedure (
      userData    : Userdata_Access;
      s           : XML_Char_p;
      len         : C.int);
   pragma Convention (C, XML_DefaultHandler);

   -- Some handlers not done yet --

   procedure XML_SetElementHandler (
      parser      : XML_Parser;
      start       : XML_StartElementHandler;
      endh        : XML_EndElementHandler);
   pragma Import (C, XML_SetElementHandler, "XML_SetElementHandler");

   procedure XML_SetStartElementHandler (
      parser      : XML_Parser;
      handler     : XML_StartElementHandler);
   pragma Import (C, XML_SetStartElementHandler, "XML_SetStartElementHandler");

   procedure XML_SetEndElementHandler (
      parser      : XML_Parser;
      handler     : XML_EndElementHandler);
   pragma Import (C, XML_SetEndElementHandler, "XML_SetEndElementHandler");

   procedure XML_SetCharacterDataHandler (
      parser      : XML_Parser;
      handler     : XML_CharacterDataHandler);
   pragma Import (C, XML_SetCharacterDataHandler, "XML_SetCharacterDataHandler");

   procedure XML_SetProcessingInstructionHandler (
      parser      : XML_Parser;
      handler     : XML_ProcessingInstructionHandler);
   pragma Import (C, XML_SetProcessingInstructionHandler, "XML_SetProcessingInstructionHandler");

   procedure XML_SetCommentHandler (
      parser      : XML_Parser;
      handler     : XML_CommentHandler);
   pragma Import (C, XML_SetCommentHandler, "XML_SetCommentHandler");

   procedure XML_SetCdataSectionHandler (
      parser      : XML_Parser;
      start       : XML_StartCdataSectionHandler;
      endh        : XML_EndCdataSectionHandler);
   pragma Import (C, XML_SetCdataSectionHandler, "XML_SetCdataSectionHandler");

   procedure XML_SetStartCdataSectionHandler (
      parser      : XML_Parser;
      handler     : XML_StartCdataSectionHandler);
   pragma Import (C, XML_SetStartCdataSectionHandler, "XML_SetStartCdataSectionHandler");

   procedure XML_SetEndCdataSectionHandler (
      parser      : XML_Parser;
      handler     : XML_EndCdataSectionHandler);
   pragma Import (C, XML_SetEndCdataSectionHandler, "XML_SetEndCdataSectionHandler");

   procedure XML_SetDefaultHandler (
      parser      : XML_Parser;
      handler     : XML_DefaultHandler);
   pragma Import (C, XML_SetDefaultHandler, "XML_SetDefaultHandler");

   procedure XML_SetDefaultHandlerExpand (
      parser      : XML_Parser;
      handler     : XML_DefaultHandler);
   pragma Import (C, XML_SetDefaultHandlerExpand, "XML_SetDefaultHandlerExpand");

   procedure XML_DefaultCurrent (parser: XML_Parser);
   pragma Import (C, XML_DefaultCurrent, "XML_DefaultCurrent");

   procedure XML_SetUserData (
      parser      : XML_Parser;
      userData    : Userdata_Access);
   pragma Import (C, XML_SetUserData, "XML_SetUserData");

   procedure XML_SetEncoding (
      parser      : XML_Parser;
      encoding    : XML_Char_p);
   pragma Import (C, XML_SetEncoding, "XML_SetEncoding");

   procedure XML_UseParserAsHandlerArg (parser: XML_Parser);
   pragma Import (C, XML_UseParserAsHandlerArg, "XML_UseParserAsHandlerArg");

   procedure XML_UseForeignDTD (
      parser      : XML_Parser;
      useDTD      : XML_Bool);
   pragma Import (C, XML_UseForeignDTD, "XML_UseForeignDTD");

   procedure XML_SetBase (
      parser      : XML_Parser;
      base        : XML_Char_p);
   pragma Import (C, XML_SetBase, "XML_SetBase");

   function XML_GetBase (parser: XML_Parser) return XML_Char_p;
   pragma Import (C, XML_GetBase, "XML_GetBase");

   function XML_GetSpecifiedAttributeCount (parser: XML_Parser) return C.int;
   pragma Import (C, XML_GetSpecifiedAttributeCount, "XML_GetSpecifiedAttributeCount");

   function XML_GetIdAttributeIndex (parser: XML_Parser) return C.int;
   pragma Import (C, XML_GetIdAttributeIndex, "XML_GetIdAttributeIndex");

   function XML_Parse (
      parser      : XML_Parser;
      s           : C.Strings.chars_ptr;
      len         : C.int;
      isFinal     : C.int)
      return XML_Status;
   pragma Import (C, XML_Parse, "XML_Parse");

   function XML_GetBuffer (
      parser      : XML_Parser;
      len         : C.int;
      isFinal     : C.int)
      return System.Address;
   pragma Import (C, XML_GetBuffer, "XML_GetBuffer");

   function XML_ParseBuffer (
      parser      : XML_Parser;
      len         : C.int;
      isFinal     : C.int)
      return XML_Status;
   pragma Import (C, XML_ParseBuffer, "XML_ParseBuffer");

   function XML_StopParser (
      parser      : XML_Parser;
      resumable   : XML_Bool)
      return XML_Status;
   pragma Import (C, XML_StopParser, "XML_StopParser");

   function XML_ResumeParser (
      parser      : XML_Parser)
      return XML_Status;
   pragma Import (C, XML_ResumeParser, "XML_ResumeParser");

   type XML_Parsing_Enum is (
      XML_INITIALIZED,
      XML_PARSING,
      XML_FINISHED,
      XML_SUSPENDED);
   pragma Convention (C, XML_Parsing_Enum);

   type XML_ParsingStatus is record
      parsing     : XML_Parsing_Enum;
      finalBuffer : XML_Bool;
   end record;
   pragma Convention (C, XML_ParsingStatus);

   procedure XML_GetParsingStatus (
      parser      : XML_Parser;
      status      : out XML_ParsingStatus);
   pragma Import (C, XML_GetParsingStatus, "XML_GetParsingStatus");

   -- some more omissions --

   function XML_GetErrorCode (parser: XML_Parser)
      return XML_Error;
   pragma Import (C, XML_GetErrorCode, "XML_GetErrorCode");

   function XML_GetCurrentLineNumber (parser: XML_Parser)
      return XML_Size;
   pragma Import (C, XML_GetCurrentLineNumber, "XML_GetCurrentLineNumber");

   function XML_GetCurrentColumnNumber (parser: XML_Parser)
      return XML_Size;
   pragma Import (C, XML_GetCurrentColumnNumber, "XML_GetCurrentColumnNumber");

   function XML_GetCurrentByteIndex (parser: XML_Parser)
      return XML_Index;
   pragma Import (C, XML_GetCurrentByteIndex, "XML_GetCurrentByteIndex");

   procedure XML_FreeContentModel (
      parser      : XML_Parser;
      model       : XML_Content_p);
   pragma Import (C, XML_FreeContentModel, "XML_FreeContentModel");

   procedure XML_ParserFree (parser: XML_Parser);
   pragma Import (C, XML_ParserFree, "XML_ParserFree");

   function XML_ErrorString (code: XML_Error)
      return XML_LChar_p;
   pragma Import (C, XML_ErrorString, "XML_ErrorString");

   function XML_ExpatVersion return XML_LChar_p;
   pragma Import (C, XML_ExpatVersion, "XML_ExpatVersion");

   type XML_Expat_Version is record
      major       : C.int;
      minor       : C.int;
      micro       : C.int;
   end record;

   function XML_ExpatVersionInfo return XML_Expat_Version;
   pragma Import (C, XML_ExpatVersionInfo, "XML_ExpatVersionInfo");

   type XML_FeatureEnum is (
      XML_FEATURE_END,
      XML_FEATURE_UNICODE,
      XML_FEATURE_UNICODE_WCHAR_T,
      XML_FEATURE_DTD,
      XML_FEATURE_CONTEXT_BYTES,
      XML_FEATURE_MIN_SIZE,
      XML_FEATURE_SIZEOF_XML_CHAR,
      XML_FEATURE_SIZEOF_XML_LCHAR,
      XML_FEATURE_NS,
      XML_FEATURE_LARGE_SIZE);
   pragma Convention (C, XML_FeatureEnum);

   type XML_Feature is record
      feature     : XML_FeatureEnum;
      name        : XML_LChar_p;
      value       : C.long;
   end record;
   pragma Convention (C, XML_Feature);

   type XML_Feature_p is access XML_Feature;
   pragma Convention (C, XML_Feature_p);

   function XML_GetFeatureList return XML_Feature_p;
   pragma Import (C, XML_GetFeatureList, "XML_GetFeatureList");

   XML_MAJOR_VERSION: constant := 2;
   XML_MINOR_VERSION: constant := 0;
   XML_MICRO_VERSION: constant := 1;

   -- Some extras
   function To_XML_Bool (X: Boolean) return XML_Bool;
   function To_XML_Bool (X: Boolean) return C.int;
private
   type XML_ParserStruct is null record; -- internal structure
end Expat;
