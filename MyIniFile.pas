unit MyIniFile;
{$mode objfpc}
interface

uses
  IniFiles, SysUtils, Classes, Forms;

type
  TMyIniFile = class(TIniFile)
  private
    function StrToHex(s: string): string;
    function HexToStr(s: string): string;
  public
    procedure ReadStrings(const Section, Ident: string; var AStrings: TStringList; ADefault: string = '');
    procedure WriteStrings(const Section, Ident: string; const AStrings: TStringList);
    procedure WriteHexString(const Section, Ident, Value: string);
    function ReadHexString(const Section, Ident: string; const ADefault: string = ''): string;
    procedure ReadHexStrings(const Section, Ident: string; var AStrings: TStringList; ADefault: string = '');
    procedure WriteHexStrings(const Section, Ident: string; const AStrings: TStringList);
  end;

function RowIdent(const Ident: string; I: Integer): string;

implementation
{
  MBY 2008

  Denna unit implementerar extra funktioner hos inifiler.
  WriteHexStr och ReadHexStr konverterar en sträng till en hexsträng så
  att godtyckliga bytes (t.ex. ascii #0 och #26) kan skrivas/läsas.
}

function RowIdent(const Ident: string; I: Integer): string;
begin
  Result := Ident + '_' + IntToStr(I);
end;
{ TMyIniFile }


function TMyIniFile.HexToStr(s: string): string;
var
  I, V: Integer;
begin
  I := 1;
  V := 1;
  SetLength(Result, Length(s) div 2);
  while I < Length(s) do
  begin
    Result[V] := Chr(StrToIntDef('$' + Copy(s, I, 2), 0));
    Inc(V);
    Inc(I, 2);
  end;
end;

function TMyIniFile.ReadHexString(const Section, Ident: string; const ADefault: string = ''): string;
begin
  Result := HexToStr(ReadString(Section, Ident, ADefault));
end;

procedure TMyIniFile.WriteStrings(const Section, Ident: string; const AStrings: TStringList);
var
  I: Integer;
begin
  for I := 0 to AStrings.Count - 1 do
  begin
    WriteString(Section, RowIdent(Ident, I), AStrings.Strings[I]);
  end;
end;

procedure TMyIniFile.ReadStrings(const Section, Ident: string; var AStrings: TStringList; ADefault: string = '');
var
  I: Integer;
begin
//  AStrings.Clear;
  I := 0;
  while ValueExists(Section, RowIdent(Ident, I)) do
  begin
    AStrings.Add(ReadString(Section, RowIdent(Ident, I), ADefault));
    Inc(I);
  end;
end;

function TMyIniFile.StrToHex(s: string): string;
var
  I, V: Integer;
  h: string;
begin
  SetLength(Result, 2 * Length(s));
  V := 1;
  for I := 1 to Length(s) do
  begin
    h := IntToHex(Ord(s[I]), 2);
    Result[V] := h[1];
    Result[V + 1] := h[2];
    Inc(V, 2);
  end;
end;

procedure TMyIniFile.WriteHexString(const Section, Ident, Value: String);
begin
  WriteString(Section, Ident, StrToHex(Value));
end;

procedure TMyIniFile.ReadHexStrings(const Section, Ident: string;
  var AStrings: TStringList; ADefault: string);
var
  I: Integer;
begin
  AStrings.Clear;
  I := 0;
  while ValueExists(Section, RowIdent(Ident, I)) do
  begin
    AStrings.Add(ReadHexString(Section, RowIdent(Ident, I), ADefault));
    Inc(I);
  end;
end;

procedure TMyIniFile.WriteHexStrings(const Section, Ident: string;
  const AStrings: TStringList);
var
  I: Integer;
begin
  for I := 0 to AStrings.Count - 1 do
  begin
    WriteHexString(Section, RowIdent(Ident, I), AStrings.Strings[I]);
  end;
end;

end.
