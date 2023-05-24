unit udmyala;

{$mode ObjFPC}{$H+}

{
    YALA - Yet Another Lexical Analyser
    Constructs a Lexical Analyster by creating from a DFA (relies on udfa.pas)

    Copyright (C)2020-2023 Duncan Munro

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Contact: Duncan Munro  duncan@duncanamps.com
}


interface

uses
  Classes, SysUtils, udmdfa, udmnfa;

type
  TDmLexerToken = record
    Token:     TNFAToken;
    Buf:       string;
    SourcePos: integer;
  end;

  TDmTokenProc = procedure(_lextoken: TDmLexerToken) of object;

  TDmLexer = class(TObject)
    private
      FAccept:    TNFAToken;
      FDFA:       TDFA;
      FOnToken:   TDmTokenProc;
      FPeek:      char;
      FPeekAvail: boolean;
      FSourcePos: integer;
      FState:     TSetIndex;
      FTokenBuf:  string;
      procedure Accept(_posn: int64);
      procedure AcceptEOF(_posn: int64);
      procedure AcceptError(_posn: int64);
      function  Fetch: char;
      procedure FillPeek(_strm: TStream);
      procedure Reset;
      procedure ResetState(_position: Int64);
    public
      constructor Create;
      constructor Create(_dfa: TDFA);
      destructor Destroy; override;
      procedure Tokenise(_strm: TStream);
      property OnToken: TDmTokenProc write FOnToken;
  end;

implementation

constructor TDmLexer.Create;
begin
  inherited Create;
  Reset;
end;

constructor TDmLexer.Create(_dfa: TDFA);
begin
  Create; // Basic create
  FDFA := _dfa;
end;

destructor TDmLexer.Destroy;
begin
  inherited Destroy;
end;

procedure TDmLexer.Accept(_posn: int64);
var b: TDmLexerToken;
begin
  b.Buf       := FTokenBuf;
  b.SourcePos := FSourcePos;
  b.Token     := FAccept;
  if Assigned(FOnToken) then
    FOnToken(b);
  ResetState(_posn);
end;

procedure TDmLexer.AcceptEOF(_posn: int64);
var b: TDmLexerToken;
begin
  b.Buf       := '';
  b.SourcePos := FSourcePos;
  b.Token     := TOKEN_EOF;
  if Assigned(FOnToken) then
    FOnToken(b);
  ResetState(_posn);
end;

procedure TDmLexer.AcceptError(_posn: int64);
var b: TDmLexerToken;
begin
  b.Buf       := FTokenBuf;
  b.SourcePos := FSourcePos;
  b.Token     := TOKEN_ERROR;
  if Assigned(FOnToken) then
    FOnToken(b);
  ResetState(_posn);
end;

function TDmLexer.Fetch: char;
begin
  Fetch := FPeek;
  FPeek := #0;
  FPeekAvail := False;
end;

procedure TDmLexer.FillPeek(_strm: TStream);
begin
  if not FPeekAvail then
    begin
      if _strm.Position < _strm.Size then
        begin
          FPeek := char(_strm.ReadByte);
          FPeekAvail := True;
        end;
    end;
end;

procedure TDmLexer.Reset;
begin
  ResetState(0);
  FPeek      := #0;
  FPeekAvail := False;
end;

procedure TDmLexer.ResetState(_position: Int64);
begin
  FTokenBuf := '';
  FAccept    := NULL_NFA_TOKEN;
  FState     := 0;
  if FPeekAvail then
    FSourcePos := _position - 1
  else
    FSourcePos := _position;
end;

procedure TDmLexer.Tokenise(_strm: TStream);
var newstate: TSetIndex;
begin
  _strm.Position := 0;
  Reset;
  repeat
    FillPeek(_strm);
    if FPeekAvail then
      begin
        newstate := FDFA[FState].Destinations[FPeek];
        if newstate = NULL_INDEX then
          begin // Cannot move forward to new state so accept or error
            if FAccept <> NULL_NFA_TOKEN then
              Accept(_strm.Position)
            else // There's a problem...
              begin  // Grab the character and form an error token
                FTokenBuf := FTokenBuf + Fetch;
                AcceptError(_strm.Position);
              end;
          end
        else
          begin // Moving forward, go to next state
            FState    := newstate;
            FAccept   := FDFA[FState].Accepting;
            FTokenBuf := FTokenBuf + Fetch;
          end;
      end;
  until (_strm.Position = _strm.Size) and (not FPeekAvail);
  if FAccept <> NULL_NFA_TOKEN then // Part formed token available ?
    Accept(_strm.Position);
  FSourcePos := _strm.Position;
  AcceptEOF(_strm.Position);
end;

end.

