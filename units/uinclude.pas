unit uinclude;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TIncludeEntry = record
    Filename:   string;
    LineNumber: integer;
    Warnings:   boolean;
  end;

  TIncludeStack = class(specialize TStack<TIncludeEntry>)

  end;

implementation




//-----------------------------------------------------------------------------
//
//  TIncludeStack code
//
//-----------------------------------------------------------------------------

end.

