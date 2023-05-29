{
    XA80 - Cross Assembler for x80 processors
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

unit ucommand;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TOperand = class(TObject)

  end;

  TOperandList = class(specialize TObjectList<TOperand>)

  end;

  TCommandExec = procedure (const _label: string; _operandlist: TOperandList)  of object;

  TCommandObj = class(TObject)
    public
      CommandExec: TCommandExec;
      CommandName: string;
  end;

  TCommandList = class(specialize TObjectList<TCommandObj>)
    private
      procedure CmdASMERROR(const _label: string; _operandlist: TOperandList);
      procedure CmdASMWARNING(const _label: string; _operandlist: TOperandList);
      procedure CmdCODE(const _label: string; _operandlist: TOperandList);
      procedure CmdCSEG(const _label: string; _operandlist: TOperandList);
      procedure CmdDATA(const _label: string; _operandlist: TOperandList);
      procedure CmdDB(const _label: string; _operandlist: TOperandList);
      procedure CmdDC(const _label: string; _operandlist: TOperandList);
      procedure CmdDD(const _label: string; _operandlist: TOperandList);
      procedure CmdDF(const _label: string; _operandlist: TOperandList);
      procedure CmdDM(const _label: string; _operandlist: TOperandList);
      procedure CmdDS(const _label: string; _operandlist: TOperandList);
      procedure CmdDSEG(const _label: string; _operandlist: TOperandList);
      procedure CmdDW(const _label: string; _operandlist: TOperandList);
      procedure CmdDZ(const _label: string; _operandlist: TOperandList);
      procedure CmdELSE(const _label: string; _operandlist: TOperandList);
      procedure CmdEND(const _label: string; _operandlist: TOperandList);
      procedure CmdENDIF(const _label: string; _operandlist: TOperandList);
      procedure CmdENDM(const _label: string; _operandlist: TOperandList);
      procedure CmdENDR(const _label: string; _operandlist: TOperandList);
      procedure CmdENDW(const _label: string; _operandlist: TOperandList);
      procedure CmdEQU(const _label: string; _operandlist: TOperandList);
      procedure CmdEXTERN(const _label: string; _operandlist: TOperandList);
      procedure CmdEXTERNAL(const _label: string; _operandlist: TOperandList);
      procedure CmdGLOBAL(const _label: string; _operandlist: TOperandList);
      procedure CmdIF(const _label: string; _operandlist: TOperandList);
      procedure CmdIFDEF(const _label: string; _operandlist: TOperandList);
      procedure CmdIFNDEF(const _label: string; _operandlist: TOperandList);
      procedure CmdINCLUDE(const _label: string; _operandlist: TOperandList);
      procedure CmdMACRO(const _label: string; _operandlist: TOperandList);
      procedure CmdMESSAGE(const _label: string; _operandlist: TOperandList);
      procedure CmdORG(const _label: string; _operandlist: TOperandList);
      procedure CmdREPEAT(const _label: string; _operandlist: TOperandList);
      procedure CmdRPT(const _label: string; _operandlist: TOperandList);
      procedure CmdTITLE(const _label: string; _operandlist: TOperandList);
      procedure CmdUDATA(const _label: string; _operandlist: TOperandList);
      procedure CmdUSEG(const _label: string; _operandlist: TOperandList);
      procedure CmdWHILE(const _label: string; _operandlist: TOperandList);
      procedure RegisterCommand(const _cmdname: string; _cmdexec: TCommandExec);
    public
      constructor Create;
      procedure PopulateStringList(_sl: TStringList);
  end;


implementation

constructor TCommandList.Create;
begin
  RegisterCommand('=',		@CmdEQU);
  RegisterCommand('ASMERROR',	@CmdASMERROR);
  RegisterCommand('ASMWARNING',	@CmdASMWARNING);
  RegisterCommand('CODE',	@CmdCODE);
  RegisterCommand('CSEG',	@CmdCSEG);
  RegisterCommand('DATA',	@CmdDATA);
  RegisterCommand('DB',		@CmdDB);
  RegisterCommand('DC',		@CmdDC);
  RegisterCommand('DD',		@CmdDD);
  RegisterCommand('DEFB',	@CmdDB);
  RegisterCommand('DEFC',	@CmdDC);
  RegisterCommand('DEFD',	@CmdDD);
  RegisterCommand('DEFF',	@CmdDF);
  RegisterCommand('DEFM',	@CmdDM);
  RegisterCommand('DEFS',	@CmdDS);
  RegisterCommand('DEFW',	@CmdDW);
  RegisterCommand('DEFZ',	@CmdDZ);
  RegisterCommand('DF',	        @CmdDF);
  RegisterCommand('DM',		@CmdDM);
  RegisterCommand('DS',		@CmdDS);
  RegisterCommand('DSEG',	@CmdDSEG);
  RegisterCommand('DW',		@CmdDW);
  RegisterCommand('DZ',		@CmdDZ);
  RegisterCommand('ELSE',	@CmdELSE);
  RegisterCommand('END',	@CmdEND);
  RegisterCommand('ENDIF',	@CmdENDIF);
  RegisterCommand('ENDM',	@CmdENDM);
  RegisterCommand('ENDR',	@CmdENDR);
  RegisterCommand('ENDW',	@CmdENDW);
  RegisterCommand('EQU',	@CmdEQU);
  RegisterCommand('EXTERN',	@CmdEXTERN);
  RegisterCommand('EXTERNAL',	@CmdEXTERNAL);
  RegisterCommand('GLOBAL',	@CmdGLOBAL);
  RegisterCommand('IF',		@CmdIF);
  RegisterCommand('IFDEF',	@CmdIFDEF);
  RegisterCommand('IFNDEF',	@CmdIFNDEF);
  RegisterCommand('INCLUDE',	@CmdINCLUDE);
  RegisterCommand('MACRO',	@CmdMACRO);
  RegisterCommand('MESSAGE',	@CmdMESSAGE);
  RegisterCommand('ORG',	@CmdORG);
  RegisterCommand('ORIGIN',	@CmdORG);
  RegisterCommand('REPEAT',	@CmdREPEAT);
  RegisterCommand('RPT',	@CmdRPT);
  RegisterCommand('TITLE',	@CmdTITLE);
  RegisterCommand('UDATA',	@CmdUDATA);
  RegisterCommand('USEG',	@CmdUSEG);
  RegisterCommand('WHILE',	@CmdWHILE);
end;

procedure TCommandList.CmdASMERROR(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdASMWARNING(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdCODE(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdCSEG(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdDATA(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdDB(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdDC(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdDD(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdDF(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdDM(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdDS(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdDSEG(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdDW(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdDZ(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdELSE(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdEND(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdENDIF(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdENDM(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdENDR(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdENDW(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdEQU(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdEXTERN(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdEXTERNAL(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdGLOBAL(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdIF(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdIFDEF(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdIFNDEF(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdINCLUDE(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdMACRO(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdMESSAGE(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdORG(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdREPEAT(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdRPT(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdTITLE(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdUDATA(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdUSEG(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.CmdWHILE(const _label: string; _operandlist: TOperandList);
begin
end;

procedure TCommandList.PopulateStringList(_sl: TStringList);
var obj: TCommandObj;
begin
  _sl.Clear;
  for obj in Self do
    _sl.Add(obj.CommandName);
end;

procedure TCommandList.RegisterCommand(const _cmdname: string; _cmdexec: TCommandExec);
var obj: TCommandObj;
begin
  obj := TCommandObj.Create;
  obj.CommandName := _cmdname;
  obj.CommandExec := _cmdexec;
  Add(obj);
end;

end.

