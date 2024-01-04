unit upreparser3_defs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lacogen_types, Generics.Collections, uasmglobals;

type
  TParserState = (psNone,
                  ppLabel,
                  ppCommand,
                  ppInstruction,
                  ppMacro,
                  ppOperand,
                  psComment,
                  psUnknown,
                  psGlob,
                  psComma,
                  psWhitespace,
                  psDQStr,
                  psDQEsc,
                  psSQChr,
                  psDone);

  TParserProp = record
    State:    TParserState;  // Parser object type
    Payload:  string;        // Payload, the string that caused this
    Token:    integer;       // Token index
    Column:   integer;       // Column number in the line 1..n
    Index:    integer;       // Index of the opcode or directive
    Level:    integer;       // Bracket level at end of object, e.g LOW(( -> 2
    DataType: TLCGParserStackType; // Data type of the parsed item
    IntValue: word;          // Integer value from the main Parser
    StrValue: String;        // String value from the main Parser
    Source:   TExpressionSource; // Variable, constant, etc.
  end;

  TPreparserBase = class(specialize TList<TParserProp>);

implementation

end.

