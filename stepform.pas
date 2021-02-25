unit StepForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  SynEdit;

const
  cNaN = 10E999;
//  cSettingsFile = 'Settings.conf';

type
  TRunMode = (rmNone, rmMonitor, rmCharging, rmDischarging, rmDischargingCR, rmWait, rmLoop, rmEnd);

{
  TBreakPtr = ^TBreak;
  TBreak = record
    Time: TDateTime;
    Duration: TDateTime;
    Current: Extended;
    Voltage: Extended;
    Capacity: Extended;
    Energy: Extended;
    Next: TBreakPtr;
  end;

  TProgram = record
    Token: Integer;
    Cells: Integer;
    Current: Extended;
    Voltage: Extended;
    BreakPtr: TBreakPtr;
  end;}

  TSteps = record
    Mode: TRunMode;
//    ModeText: string;
    Command: string;
    Cells: Integer;
    TestVal: Extended;
    CutVolt: Extended;
    CutAmp: Extended;
    CutTime: Integer;
    CutAmpTime: Integer;  // Cut N minutes after current < CutAmp
    CutCap: Extended;
    CutEnergy: Extended;
    Loop: Integer;
    LoopCounter: Integer;
    CapI: Boolean;        // Loop as long as capacity increases
    EneI: Boolean;        // Loop as long as energy increases
    CV: Extended;         // Constant voltage for c_CCCV mode
  end;

  { TfrmStep }

  TfrmStep = class(TForm)
    Button1: TButton;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    N3: TMenuItem;
    N2: TMenuItem;
    MenuItem11: TMenuItem;
    mniNew: TMenuItem;
    mniOpen: TMenuItem;
    mniSave: TMenuItem;
    mniSaveAs: TMenuItem;
    mniExit: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N1: TMenuItem;
    memStep: TSynEdit;
    odOpen: TOpenDialog;
    sdSave: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure memStepChange(Sender: TObject);
    procedure mniExitClick(Sender: TObject);
    procedure mniNewClick(Sender: TObject);
    procedure mniOpenClick(Sender: TObject);
    procedure mniSaveClick(Sender: TObject);
    procedure mniSaveAsClick(Sender: TObject);
  private
    FCompiled: Boolean;
    FInitialDirIsSet: Boolean;
  public
    procedure Compile;
    property Compiled: Boolean read FCompiled;
    procedure SetInitialDir(ADir: string);
  end;

var
  FSteps: array of TSteps;
  frmStep: TfrmStep;

implementation

{$R *.lfm}

function TestStr(AStr: string; ATest: string): Boolean;
begin
  Result := (Pos(ATest, AStr) = 1);
end;

function MyVal(AStr: string): Extended;
var
  I: Integer;
  s: string;
begin
  Result := 0;
  s := '';
  for I := 1 to Length(AStr) do
  begin
    if AStr[I] in ['0'..'9', '.'] then
    begin
      s := s + AStr[I];
    end;
  end;
  if Length(s) > 0 then
  begin
    Result := StrToFloat(s);
  end;
end;

function GetParam(AStr: string; AParam: Integer): string;
var
  I, S: Integer;
  sl: string;
begin
  S := 1;
  Result := '';
  sl := AStr + ' ';
  for I := 1 to Length(sl) do
  begin
    if sl[I] in [' '] then
    begin
      Dec(AParam);
      if AParam = 0 then
      begin
        Result := Copy(sl, S, I - S);
        Break;
      end;
      S := I + 1;
    end;
  end;
end;

{ TfrmStep }

procedure TfrmStep.Compile;
var
  I, J, N: Integer;
  s, r: string;
begin
  SetLength(FSteps, memStep.Lines.Count);
  for I := 0 to Length(FSteps) - 1 do
  begin
    FSteps[I].Command := 'END';
    FSteps[I].Mode := rmEND;
  end;
  N := 0;
  for I := 0 to memStep.Lines.Count - 1 do
  begin
    s := UpperCase(memStep.Lines.Strings[I]);
    if (Length(s) > 1) and (Copy(s, 1, 2) = '//') then
    begin
      FSteps[N].Mode := rmNone;
      Continue;
    end;
    if Length(GetParam(s, 1)) > 0 then
    begin
      FSteps[N].Mode := rmCharging;
      FSteps[N].Cells := 1;
      FSteps[N].TestVal := 0.1;
      FSteps[N].CutAmp := 0;
      FSteps[N].CutTime := 0;
      FSteps[N].CutVolt := 0.4;
      FSteps[N].CutCap := 0;
      FSteps[N].CutEnergy := 0;
      FSteps[N].CapI := False;
      FSteps[N].EneI := False;
      FSteps[N].LoopCounter := 0;
      J := 2;
      while True do
      begin
        r := GetParam(s, J);
        if Length(r) > 0 then
        begin
          if TestStr(r, 'CAP') then
          begin
            FSteps[N].CutCap := MyVal(r);
          end else if TestStr(r, 'ENERGY') then
          begin
            FSteps[N].CutEnergy := MyVal(r);
          end else if TestStr(r, 'TIME') then
          begin
            FSteps[N].CutTime := Round(MyVal(r));
          end;
        end else
        begin
          Break;
        end;
        Inc(J);
      end;
      r := GetParam(s, 1);
      FSteps[N].Command := r;
      if Copy(r, 1, 2) = 'C_' then
      begin
        J := 2;
        while True do
        begin
          r := GetParam(s, J);
          if Length(r) > 0 then
          begin
            if TestStr(r, 'CELLS') then
            begin
              FSteps[N].Cells := Round(MyVal(r));
            end else if TestStr(r, 'CURR') then
            begin
              FSteps[N].TestVal := MyVal(r);
            end else if TestStr(r, 'CUTAT') then
            begin
              FSteps[N].CutAmpTime := Round(MyVal(r));
            end else if TestStr(r, 'CUTA') then
            begin
              FSteps[N].CutAmp := MyVal(r);
            end else if TestStr(r, 'VOLT') then
            begin
              FSteps[N].CV:= MyVal(r);
            end;
          end else
          begin
            Break;
          end;
          Inc(J);
        end;
      end else if Copy(r, 1, 2) = 'D_' then
      begin
        if r = 'D_CR' then
        begin
          FSteps[N].Mode := rmDischargingCR;
        end else
        begin
          FSteps[N].Mode := rmDischarging;
        end;
        FSteps[N].Cells := 1;
        FSteps[N].TestVal := 0.1;
        FSteps[N].CutAmp := cNaN;
        FSteps[N].CutTime := 0;
        FSteps[N].CutVolt := cNaN;
        J := 2;
        while True do
        begin
          r := GetParam(s, J);
          if Length(r) > 0 then
          begin
            if TestStr(r, 'CUTV') then
            begin
              FSteps[N].CutVolt := MyVal(r);
            end else if TestStr(r, 'CURR') or TestStr(r, 'RES') or TestStr(r, 'POW') then
            begin
              FSteps[N].TestVal := MyVal(r);
            end;
          end else
          begin
            Break;
          end;
          Inc(J);
        end;
      end else if r = 'WAIT' then
      begin
        FSteps[N].Mode := rmWait;
        r := GetParam(s, 2);
        FSteps[N].CutTime := Round(MyVal(r));
      end else if r = 'LOOP' then
      begin
        FSteps[N].Mode := rmLoop;
        J := 2;
        while True do
        begin
          r := GetParam(s, J);
          if r > '' then
          begin
            if TestStr(r, 'CAPI') then
            begin
              FSteps[N].CapI := True;
            end else if TestStr(r, 'ENEI') then
            begin
              FSteps[N].EneI := True;
            end else
            begin
              FSteps[N].Loop := Round(MyVal(r));
            end;
          end else
          begin
            Break;
          end;
          Inc(J);
        end;
        if FSteps[N].Loop = 0 then FSteps[N].Loop := 999999;
      end else if (Copy(r, 1, 4) = 'STOP') or (Copy(r, 1, 3) = 'END') then
      begin
        FSteps[N].Mode := rmEnd;
      end else
      begin
        Dec(N);
      end;
      Inc(N);
    end;
  end;
  FCompiled := True;
end;

procedure TfrmStep.SetInitialDir(ADir: string);
begin
  if not FInitialDirIsSet then
  begin
    sdSave.InitialDir := ADir;
    odOpen.InitialDir := ADir;
    FInitialDirIsSet := True;
  end;
end;

procedure TfrmStep.FormCreate(Sender: TObject);
begin
  FCompiled := False;
  FInitialDirIsSet := False;
end;

procedure TfrmStep.Button1Click(Sender: TObject);
begin
  frmStep.Close;
end;

procedure TfrmStep.memStepChange(Sender: TObject);
begin
  FCompiled := False;
end;

procedure TfrmStep.mniExitClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmStep.mniNewClick(Sender: TObject);
begin
  memStep.Lines.Clear;
  frmStep.Caption := 'Unsaved';
end;

procedure TfrmStep.mniOpenClick(Sender: TObject);
begin
  if odOpen.Execute then
  begin
    memStep.Lines.LoadFromFile(odOpen.FileName);
    frmStep.Caption := odOpen.FileName;
    sdSave.FileName := odOpen.FileName;
  end;
end;

procedure TfrmStep.mniSaveClick(Sender: TObject);
begin
  if sdSave.FileName > '' then
  begin
    memStep.Lines.SaveToFile(sdSave.FileName);
    frmStep.Caption := sdSave.FileName;
  end else
  begin
    mniSaveAsClick(Sender);
  end;
end;

procedure TfrmStep.mniSaveAsClick(Sender: TObject);
begin
  if sdSave.Execute then
  begin
    memStep.Lines.SaveToFile(sdSave.FileName);
    frmStep.Caption := sdSave.FileName;
  end;
end;

end.

