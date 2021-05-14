unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  EditBtn, ComCtrls, Menus, Buttons, ActnList, TAGraph, TASeries,
  TAIntervalSources, TATransformations, TATools, LazSerial, DateUtils,
  TACustomSeries, SynEdit, StepForm, MyIniFile, JLabeledIntegerEdit, math,
  JLabeledFloatEdit, settings, typinfo;//, LCLIntf, LCLType;

const
  cVersion = 'EBC Controller v0.10';

  cstVoltage = 0;
  cstCurrent = 2;
  cstPower = 1;
  cstTime = 3;
  cstCapacity = 4;
  cstCapLocal = 5;  // Capacity computed on host
  cstEnergy = 6;
  cstResistance = 8;
  cstdV = 10;
  cstdA = 12;
  cstDbg1 = 13;
  cstDbg2 = 14;
  cstDbg3 = 15;
  cstMax = 15;

  cConn = 'Conn';
  crcsendpos = 9;
  crcrecvpos = 18;
  cChanged = 'Changed';
  cUnChanged = '';
  cA = 'A';
  cP = 'W';
  cR = 'Ω';
  cCurrent = 'Current:';
  cConnecting = 'Connecting...';
  cNotConnected = 'Not connected';
  cConnected = 'Connected: ';
  cNull = '00.00';

  cName = 'Name';
  cMethod = 'Type';
  cCharge = 'Charge';
  cChargeCV = 'ChargeCV';
  cDischarge = 'Discharge';
  cCommand = 'Command';
  cStop = 'Stop';
  cConnect = 'Connect';
  cDisconnect = 'Disconnect';
  cAdjust = 'Adjust';
  cStart = 'Start';
  cCont = 'Cont';
  cTestVal = 'TestVal';
  cAutoOff = 'AutoOff';
  cVoltInfo = 'CellVoltageInfo';

  cModels = 'Models';
  cIdent = 'Ident';
  cIFactor = 'IFactor';
  cUFactor = 'UFactor';
  cPFactor = 'PFactor';
  cModelName = 'Name_';

  cDefault = 'Default';
  cChargeCurrent = 'ChargeCurrent';
  cDischargeCurrent = 'DischargeCurrent';
  cConstantVoltage = 'ConstantVoltage';
  cCells = 'Cells';
  cDischargePower = 'DischargePower';
  cDischargeResistance = 'DischargeResistance';
  cModeCommand = 'ModeCommand';
  cCutA = 'CutOffA';
  cCutATime = 'CutOffATime';
  cCutV = 'CutOffV';
  cMaxTime = 'MaxTime';
  cIntTime = 'IntegrationTime';

  cView = 'View...';
  cEdit = 'Edit...';

  cCapI = 'CapI: ';
  cEneI = 'EneI: ';

  cStartup = 'Startup';
  cUseLast = 'UseLast';
  cChargeIndex = 'ChargeIndex';
  cDischargeIndex = 'DischargeIndex';
  cStartSelection = 'StartSelection';
  cSelection = 'Selection';
  cProgFile = 'ProgFile';
  cChkSetting = 'CheckSetting';
  cSettings = 'Settings';
  cConf = '.conf';
  cInit = '.init';
  cSaveDir = 'SaveDir';
  cLogDir = 'LogDir';
  cStepDir = 'StepFileDir';
  cTabIndex = 'TabIndex';
  cReadOnly = 'ReadOnly';
  cMonitor = 'Monitor';

  cWinMaximized = 'Maximized';
  cWinWidth = 'Width';
  cWinHeight = 'Height';
  cAppSec = 'Application';
  cSerial = 'Serial';

  // Log table headers
//  cColumns = ' Step     CMD      (Ah)    (Wh)       Time     StartV  EndV';
  cColumns = ' Step  |  CMD    | (Ah)  | (Wh)  |    Time    |StartV| EndV | EndA';
  cCol: array [1..8] of Integer = (7, 9, 7, 7, 12, 6, 6, 6);

type
  TCapacity = (caEBC, caLocal);
  TSendMode = (smStart, smAdjust, smCont, smConnect, smDisconnect, smConnStop);
  TMethod = (mNone, mCharge, mChargeCV, mDischarge);
  TTestVal = (tvCurrent, tvPower, tvResistance);
  TConnState = (csNone, csConnecting, csCapture, csConnected); // csCapture = read settings from instrument

  TConnPacket = record
     Connect: string;
     Disconnect: string;
     Stop: string;
  end;

  TModel = record
     Name: string;
     IFactor: Extended;
     UFactor: Extended;
     Ident: Integer;
     ConnState: TConnState;
     ConnPackets: TConnPacket;
  end;

  TDeltaValue = record
     Time: TDateTime;
     SumV: Extended;
     SumA: Extended;
     Values: Integer;
  end;

  TCSVData = record
    vTime: Integer;
    vVoltage: Extended;
    vCurrent: Extended;
  end;

  TPacket = record
    Name: string;
    Command: string;
    Method: TMethod;
    Start: string;
    Adjust: string;
    Cont: string;
    AutoOff: string;
    TestVal: TTestVal;
    VoltInfo: Extended;
  end;


  TChecks = record
     cCurrent: Extended;
     cDwellTime: Integer;
     cEnergy: Extended;
     cCapacity: Extended;
     ThresholdTime: TDateTime;
     TimerRunning: Boolean;
  end;

  type TDefaults = record
     ChargeI: Extended;
     DischargeI: Extended;
     ConstantU: Extended;
     DischargeP: Extended;
     DischargeR: Extended;
     Cells: Integer;
     ModeName: string;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    acSavePNG: TAction;
    acSaveCSV: TAction;
    acBorderName: TAction;
    acLoadStep: TAction;
    acSettings: TAction;
    alList: TActionList;
    btnConnect: TBitBtn;
    btnAdjust: TButton;
    btnCont: TButton;
    btnProg: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnSkip: TButton;
    Button1: TButton;
    Chart: TChart;
    ChartToolset1: TChartToolset;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    chkAccept: TCheckBox;
    chkCutCap: TCheckBox;
    chkCutEnergy: TCheckBox;
    edtDelim: TEdit;
    edtTerm: TEdit;
    gbSettings: TGroupBox;
    edtCells: TJLabeledIntegerEdit;
    edtCutA: TJLabeledFloatEdit;
    edtCutM: TJLabeledIntegerEdit;
    edtTestVal: TJLabeledFloatEdit;
    edtCutCap: TJLabeledFloatEdit;
    edtCutTime: TJLabeledIntegerEdit;
    edtCutEnergy: TJLabeledFloatEdit;
    edtCutV: TJLabeledFloatEdit;
    edtChargeV: TJLabeledFloatEdit;
    gbStatus: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblCapI: TLabel;
    lblProgTime: TLabel;
    lblModel: TLabel;
    Label10: TLabel;
    lblCutCap: TLabel;
    lblCutEnergy: TLabel;
    lblCutoffV1: TLabel;
    lblMin: TLabel;
    lblStep: TLabel;
    lblStepNum: TLabel;
    lblTestUnit: TLabel;
    ChartAxisTransformationsCurrent: TChartAxisTransformations;

      ChartAxisTransformationsCurrentAutoScaleAxisTransform: TAutoScaleAxisTransform;
    ChartAxisTransformationsVoltage: TChartAxisTransformations;

      ChartAxisTransformationsVoltageAutoScaleAxisTransform: TAutoScaleAxisTransform;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    edtDevice: TEdit;
    gbConn: TGroupBox;
    Label5: TLabel;
    lblTimer: TLabel;
    lsCurrent: TLineSeries;
    lsInvisibleCurrent: TLineSeries;
    lsInvisibleVoltage: TLineSeries;
    lsVoltage: TLineSeries;
    memLog: TMemo;
    memStepLog: TMemo;
    mniSetName: TMenuItem;
    N4: TMenuItem;
    mniLoadStep: TMenuItem;
    N3: TMenuItem;
    mniSettings: TMenuItem;
    N2: TMenuItem;
    mniDoLog: TMenuItem;
    mniSetCSVLogfile: TMenuItem;
    N1: TMenuItem;
    mniSaveCSV: TMenuItem;
    mniSavePNG: TMenuItem;
    pcProgram: TPageControl;
    mnPopup: TPopupMenu;
    mnSerial: TPopupMenu;
    rgDischarge: TRadioGroup;
    rgCharge: TRadioGroup;
    sdLogCSV: TSaveDialog;
    sdPNG: TSaveDialog;
    sdCSV: TSaveDialog;
    Serial: TLazSerial;
    shaCapI: TShape;
    shpConn: TShape;
    stRunMode: TStaticText;
    stStepFile: TStaticText;
    tsConsole: TTabSheet;
    tmrWait: TTimer;
    tsProgram: TTabSheet;
    tbxMonitor: TToggleBox;
    tsCharge: TTabSheet;
    tsDischarge: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure mniSerialClick(Sender: TObject);
    procedure pcProgramChange(Sender: TObject);
    procedure SavePNGExecute(Sender: TObject);
    procedure btnAdjustClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnContClick(Sender: TObject);
    procedure btnProgClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure chkCutCapChange(Sender: TObject);
    procedure chkCutEnergyChange(Sender: TObject);
    procedure edtCellsChange(Sender: TObject);
    procedure edtCellsClick(Sender: TObject);
    procedure edtCellsEditingDone(Sender: TObject);
    procedure edtCellsExit(Sender: TObject);
    procedure edtCellsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure edtCellsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtCutTimeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memLogChange(Sender: TObject);
    procedure mniLoadStepClick(Sender: TObject);
    procedure mniSetCSVLogfileClick(Sender: TObject);
    procedure mniSaveCSVClick(Sender: TObject);
    procedure mniSavePNGClick(Sender: TObject);
    procedure mniSetNameClick(Sender: TObject);
    procedure mniSettingsClick(Sender: TObject);
    procedure rgChargeClick(Sender: TObject);
    procedure rgDischargeClick(Sender: TObject);
    procedure tbxMonitorChange(Sender: TObject);
    procedure tmrWaitTimer(Sender: TObject);
    procedure tsChargeEnter(Sender: TObject);
    procedure tsDischargeEnter(Sender: TObject);
  private
    FConfFile: string;
    FStartTime: TDateTime;
    FStepTime: TDateTime;
    FLastTime: TDateTime;
    FData: array of TCSVData;
    FAppDir: string;
    FPackets: array of TPacket;
//    FConnPacket: TConnPacket;
    FPacketIndex: Integer;
    FLogFile: Text;
    FRunMode: TRunMode;
    FChecks: TChecks;
    FSampleCounter: Integer;
    FLastU: Extended;
    FLastI: Extended;
    FProgramStep: Integer; // Points to next step after LoadStep
    FCurrentStep: Integer; // Points to current/last step
    FDefault: TDefaults;
    FWaitCounter: Integer;
    FInProgram: Boolean;
    FEnergy: Extended;
    FStartU: Extended;
    FCurrentCapacity: array [TCapacity] of Extended;     // The capacity (Ah) measured so far from the current cycle
    FLastDisCapacity: Extended;     // Last capacity from the latest discharge cycle
    FCurrentDisCapacity: Extended;  // The capacity measured from the current discharge cycle
    FBeginWaitVoltage: Extended;
    FEndWaitVoltage: Extended;
    stText: array of TStaticText;
    FModels: array of TModel;
    FModel: Integer;
    FConn: TConnPacket;
    FConnState: TConnState;
    FUFactor: Extended;
    FIFactor: Extended;
    FShowJoule: Boolean;
    FShowCoulomb: Boolean;
    FDelta: array [0..1] of TDeltaValue;
    FDeltaIndex: Integer;
    FIntTime: Integer;
    procedure DoHexLog(AText: string);
    function NewDecodeCharge(Data: string): Extended;
    procedure SerialRec(Sender: TObject);
    procedure InterpretPackage(APacket: string; ANow: TDateTime);
    procedure DumpChkSum(lbl: string; snd: string; Pos: Integer);
    procedure SendData(snd: string);
    function EncodeCurrent(Current: Extended): string;
    function EncodePower(Power: Extended): string;
    function DecodeCurrent(Data: string): Extended;
    function EncodeVoltage(Voltage: Extended): string;
    function DecodeVoltage(Data: string): Extended;
    function DecodeCharge(Data: string): Extended;
    function DecodeTimer(Data: string): Integer;
    function EncodeTimer(Data: Integer): string;
    procedure SaveCSVLine(var f: Text; ATime: Integer; ACurrent: Extended; AVoltage: Extended);
    procedure SaveCSV(AFile: string);
    function GetHexPacketFromIni(AIniFile: TMyIniFile; ASection: string; AIdent: string; ADefault: string = ''): string;
    procedure LoadPackets;
//    function MakePacket(AType: TSendMode): string;
    function NewMakePacket(Packet: Integer; AType: TSendMode): string;
    procedure SetupChecks;
    procedure FixLabels(APacket: Integer);
    procedure DoLog(AText: string);
    procedure StartLogging;
    procedure LoadStep;
    function FindPacket(AName: string): Integer;
    function GetPointer(ARadioGroup: TRadioGroup): Integer;
    function MakePacket2(Packet: Integer; SendMode: TSendMode; TestVal, SecondParam: Extended; ATime: Integer): string;
    function MakeConnPacket(SendMode: TSendMode): string;
    procedure EBCBreak(Force: Boolean = False); // Force = True terminates even if a program is running.
    procedure LogStep;
    procedure OffSetting; // Sets labels and button for "off".
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetSettings;
    function GetStepNum: string;
    function GetModelIndex(AModel: Integer): Integer;
    procedure stTextClick(Sender: TObject);
    function GetEnergy(AEnergy: Extended): string;
    function GetCharge(ACharge: Extended): string;
    procedure FreezeEdits;
    procedure UnlockEdits;
    procedure EnumerateSerial;
    procedure RunModeOffOrMonitor;
    procedure SetRunMode(ARunMode: TRunMode);
    procedure TimerOff;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

function ValOk(ANum: Extended): Boolean;
begin
  Result := not (IsNan(ANum));// or IsInf(ANum));
end;

function TextFileCopy(AInFile, AOutFile: string): Boolean;
var
  fi, fo: Text;
  s: string;
begin
  try
    if FileExists(AInFile) then
    begin
      AssignFile(fi, AInFile);
      AssignFile(fo, AOutFile);
      Reset(fi);
      ReWrite(fo);
      while not Eof(fi) do
      begin
        ReadLn(fi, s);
        WriteLn(fo, s);
      end;
      Flush(fo);
      CloseFile(fi);
      CloseFile(fo);
      Result := True;
    end else
    begin
      Result := False;
    end;
  except
    Result := False;
  end;
end;

function AlignL(AStr: string; ALen: Integer): string;
begin
  Result := AStr;
  while Length(Result) < ALen do
  begin
    Result := Result + ' ';
  end;
end;

function AlignR(AStr: string; ALen: Integer): string;
begin
  Result := AStr;
  while Length(Result) < ALen do
  begin
    Result := ' ' + Result;
  end;
end;

function Round1V(U: Extended): Extended;
begin
  Result := 1 + Round(U + 0.500);
end;

function Round100mA(I: Extended): Extended;
begin
  Result := 0.1 + Round(I * 10 + 0.50) / 10;
end;

function NumEdtOk(AStr: string; out AVal: Extended): Boolean;
var
  Code: Integer;
begin
  Val(AStr, AVal, Code);
  Result := (Code = 0);
end;

function MyFloatStr(AVal: Extended): string;
begin
  Result := FloatToStrF(AVal, ffFixed, 18, 3);
end;

function MyTimeToStr(ATime: TDateTime): string;
begin
  Result :=  IntToStr(Trunc(ATime)) + ':' + FormatDateTime('hh:mm:ss', ATime);
end;

function HexToOrd(s: string): Integer;
var
  I: Integer;
  M: Integer;
begin
  Result := 0;
  M := 1;
  for I := Length(s) downto 1 do
  begin
    if s[I] in ['0'..'9'] then
    begin
      Result := Result + M * (Ord(s[I]) - Ord('0'));
    end else if s[I] in ['A'..'F'] then
    begin
      Result := Result + M * (Ord(s[I]) - Ord('A') + 10);
    end;
    M := M * $10;
  end;
end;

function FormatPath(APath: string): string; // Removes "//" or "\\" from paths
var
  P: integer;
  s: string;
begin
  s := PathDelim + PathDelim;
  repeat
    P := Pos(s, APath);
    if P > 0 then
    begin
      APath := Copy(APath, 1, P - 1) + Copy(APath, P + 1, Length(APath));
    end;
  until P = 0;
  Result := APath;
end;


function checksum(s: string; Pos: Integer): Char; // Seems EBC uses a stupid XOR CRC
var
  I: Integer;
begin
  Result := #0;
  for I := 2 to Pos - 1 do
  begin
    Result := Chr(Ord(Result) xor Ord(s[I]));
  end;
end;

{ TfrmMain }

procedure TfrmMain.SerialRec(Sender: TObject);
var
  s: string;
  r: string;
  N: Integer;
  E: TDateTime;
begin
  shpConn.Brush.Color := clRed;
  Application.ProcessMessages;
  r := '';
  N := 0;
  E := Now;

  repeat
    s := Serial.ReadData;
    if Length(s) > 0 then
    begin
      r := r + s;
      Inc(N, Length(s));
    end;
  until (N = 19) or (MillisecondsBetween(Now, E) > 200);

  if N = 19 then
  begin
    InterpretPackage(r, E);
    if FConnState = csConnecting then
    begin
      FModel := GetModelIndex(Ord(r[17]));
      if FModel > -1 then
      begin
        lblModel.Caption := cConnected + FModels[FModel].Name;
        FConnState := csConnected;
        FUFactor := FModels[FModel].UFactor;
        FIFactor := FModels[FModel].IFactor;
      end;
    end;
  end else
  begin
    DumpChkSum('E:', r, crcrecvpos);
  end;
  FLastTime := E;
  shpConn.Brush.Color := clDefault;
  Application.ProcessMessages;
end;

procedure TfrmMain.InterpretPackage(APacket: string; ANow: TDateTime);
var
  P, tmp: Extended;
  dT: Integer;
  T: TDateTime;
begin
  if FSampleCounter > 0 then
  begin
    dT := MillisecondsBetween(ANow, FLastTime);
  end else
  begin
    dT := 2000;
  end;

  T := ANow - FStartTime;

  if chkAccept.Checked or (checksum(APacket, crcrecvpos) = APacket[crcrecvpos]) then
  begin
    FLastI := DecodeCurrent(Copy(APacket, 3, 2));
    FLastU := DecodeVoltage(Copy(APacket, 5, 2));

    FCurrentCapacity[caEBC] := DecodeCharge(Copy(APacket, 7, 2));
    FCurrentCapacity[caLocal] := FCurrentCapacity[caLocal] + FLastI * dT / 3600000;
//    DumpChkSum(FloatToStr(FCurrentCapacity[caEBC]), APacket, crcrecvpos);

    stText[cstVoltage].Caption := MyFloatStr(FLastU) + 'V';
    stText[cstCurrent].Caption := MyFloatStr(FLastI) + 'A';
    P := FLastU * FLastI;
    stText[cstPower].Caption := FloatToStrF(P, ffFixed, 18, 1) + 'W';
    stText[cstCapacity].Caption := GetCharge(FCurrentCapacity[caEBC]);
    stText[cstCapLocal].Caption := GetCharge(FCurrentCapacity[caLocal]) + '(PC);
    tmp := (P * dT) / 3600000;
    if ValOk(tmp) then
    begin
      FEnergy := FEnergy + tmp;
      stText[cstEnergy].Caption := GetEnergy(FEnergy);
    end;
    stText[cstTime].Caption := MyTimeToStr(T);

    if FLastI <> 0 then
    begin
      tmp := FLastU / FLastI;
      if ValOk(tmp) then
      begin
        stText[cstResistance].Caption := MyFloatStr(tmp) + cR;
      end;
    end;


    if FInProgram then
    begin
      lblProgTime.Caption := TimeToStr(ANow - FStepTime);
    end;

    if not (FRunMode in [rmNone]) then
    begin
      SetLength(FData, Length(FData) + 1);
      with FData[Length(FData) - 1] do
      begin
        vTime := DecodeTimer(Copy(APacket, 15, 2));
        vVoltage := FLastU;
        vCurrent := FLastI;
        if mniDoLog.Checked then
        begin
          SaveCSVLine(FLogFile, vTime, FLastI, FLastU);
        end;
      end;
      lsVoltage.AddXY(T, FLastU);
      lsInvisibleVoltage.AddXY(0, Round1V(FLastU));
      lsCurrent.AddXY(T, FLastI);
      lsInvisibleCurrent.AddXY(0, Round100mA(FLastI));
    end;

    FDelta[FDeltaIndex].SumV := FDelta[FDeltaIndex].SumV + FLastU;
    FDelta[FDeltaIndex].SumA := FDelta[FDeltaIndex].SumA + FLastI;
    Inc(FDelta[FDeltaIndex].Values);

{      if dT <> 0 then
    begin
      stText[cstdV].Caption := FloatToStrF((1000000 * (FLastU - lU)) / (dT ), ffFixed, 18, 2) + 'mV/s';
      stText[cstdA].Caption := FloatToStrF((1000000 * (FLastI - lI)) / (dT ), ffFixed, 18, 2) + 'mA/s';
    end;
}

    dT := MillisecondsBetween(FDelta[FDeltaIndex].Time, ANow);
    if dT >= FIntTime then if dT <> 0 then
    begin
      FDelta[FDeltaIndex].SumV := FDelta[FDeltaIndex].SumV / FDelta[FDeltaIndex].Values;
      FDelta[FDeltaIndex].SumA := FDelta[FDeltaIndex].SumA / FDelta[FDeltaIndex].Values;
      if ValOk(FDelta[FDeltaIndex].SumA) then if ValOk(FDelta[FDeltaIndex].SumV) then
      begin
        tmp := (FDelta[FDeltaIndex].SumV - FDelta[FDeltaIndex xor $01].SumV) / dT;
        if ValOk(tmp) then
        begin
          stText[cstdV].Caption := FloatToStrF(60000000 * tmp , ffFixed, 18, 2) + 'mV/m';
        end;
        tmp := (FDelta[FDeltaIndex].SumA - FDelta[FDeltaIndex xor $01].SumA) / dT;
        if ValOk(tmp) then
        begin
          stText[cstdA].Caption := FloatToStrF(60000000 * tmp, ffFixed, 18, 2) + 'mA/m';
        end;
      //  stText[cstDbg1].Caption :=   IntToStr(FDelta[FDeltaIndex].Values);
      //  stText[cstDbg2].Caption := FloatToStr(FDelta[FDeltaIndex].SumV);
      //  stText[cstDbg3].Caption := 'Index: ' + IntToStr(FDeltaIndex) + ' : ' + IntToStr(FDeltaIndex xor $01);
      end;
      FDeltaIndex := FDeltaIndex xor $01;
      FDelta[FDeltaIndex].SumV := 0;
      FDelta[FDeltaIndex].SumA := 0;
      FDelta[FDeltaIndex].Values := 0;
      FDelta[FDeltaIndex].Time := ANow;
    end;

//    DumpChkSum('R:', APacket, crcrecvpos);

    Inc(FSampleCounter);

    if (FRunMode = rmDischargingCR) and (FSampleCounter mod 3 = 0) then
    begin
      SendData(NewMakePacket(FPacketIndex, smAdjust));
    end;

    // AutoOff check
    if (not (FRunMode in [rmNone, rmMonitor, rmWait, rmLoop])) and ((APacket[2] = FPackets[FPacketIndex].AutoOff) or ((FSampleCounter > 3) and (FLastI < 0.0001))) then
    begin
      EBCBreak;
    end;

    // Cutoff checks
    if (FRunMode = rmCharging) and (FSampleCounter > 3) then
    begin
      if FLastI < FChecks.cCurrent then
      begin
        if FChecks.TimerRunning then
        begin
          DoLog('Time: ' + IntToStr(SecondsBetween(FChecks.ThresholdTime, Now)));
          if SecondsBetween(FChecks.ThresholdTime, Now) div 60 >= FChecks.cDwellTime then
          begin
            EBCBreak;
          end;
        end else
        begin
          FChecks.ThresholdTime := Now;
          FChecks.TimerRunning := True;
          DoLog('Timer started.');
          if FChecks.cDwellTime = 0 then
          begin
            EBCBreak;
          end;
        end;
      end;
      if FCurrentCapacity[caEBC] > FChecks.cCapacity then
      begin
        EBCBreak;
      end;
    end;
  end else
  begin
    DumpChkSum('CRC err:', APacket, crcrecvpos);
  end;
end;

procedure TfrmMain.DumpChkSum(lbl: string; snd: string; Pos: Integer);
var
  s: string;
  I: Integer;
begin
  s := '';
  for I := 1 to Length(snd) do
  begin
    s := s + LowerCase(IntToHex(Ord(snd[I]),2));
//    if I < Length(snd) then s := s + '|';
  end;
  DoHexLog(lbl + ' ' + s + ' ' + IntToHex(Ord(checksum(snd, Pos)),2));
end;

procedure TfrmMain.SendData(snd: string);
var
  s: string;
begin
  shpConn.Brush.Color := clGreen;
  Application.ProcessMessages;
  if Length(snd) > 7 then
  begin
    s := snd;
    s[crcsendpos] := checksum(s, crcsendpos);
    Serial.WriteData(s);
    DumpChkSum('S:', s, crcsendpos);
  end;
  shpConn.Brush.Color := clDefault;
end;

function TfrmMain.EncodeVoltage(Voltage: Extended): string;
var
  V: Extended;
  H, L: Integer;
begin
  V := FUFactor * Voltage * 1000;
  H := Trunc(V / 2400);
  L := Trunc(V - (2400 * H)) div 10;
  Result := Chr(H) + Chr(L);
end;

function TfrmMain.EncodeCurrent(Current: Extended): string;
var
  V: Extended;
  H, L: Integer;
begin
  V := FIFactor * Current * 1000; // Convert to mA
  H := Trunc(V / 2400);
  L := Trunc(V - (2400 * H)) div 10;
  Result := Chr(H) + Chr(L);
end;

function TfrmMain.EncodePower(Power: Extended): string;
var
  P: Extended;
  H, L: Integer;
begin
  P := Power;
  H := Round(P / 240);
  L := Trunc(P - (240 * H));
  Result := Chr(H) + Chr(L);
end;

function TfrmMain.EncodeTimer(Data: Integer): string;
var
  H, L: Integer;
begin
  H := Data div 240;
  L := Data - (240 * H);
  Result := Chr(H) + Chr(L);
end;

function TfrmMain.DecodeCurrent(Data: string): Extended;
begin
  Result := (Ord(Data[1])*2400 + 10*Ord(Data[2])) / 1000 / FIFactor;
end;

function TfrmMain.DecodeVoltage(Data: string): Extended;
begin
  Result := (Ord(Data[1])*2400 + 10*Ord(Data[2])) / 10000 / FUFactor;
end;

function TfrmMain.DecodeCharge(Data: string): Extended;
begin
  Result := (Ord(Data[1])*2400 + 10*Ord(Data[2])) / 10000;
end;

function TfrmMain.NewDecodeCharge(Data: string): Extended;
var
  s: string;
begin
  s := Data;
  if s[1] > #$8b then
  begin
    Result := 10 + (((Ord(s[1]) - $8b) + 00)*2400 + 10*Ord(s[2])) / 10000;
  end else
  begin
    Result := (Ord(s[1])*2400 + 10*Ord(s[2])) / 10000;
  end;
end;

function TfrmMain.DecodeTimer(Data: string): Integer;
begin
  Result := Ord(Data[1])*240 + Ord(Data[2]);
end;

procedure TfrmMain.SaveCSVLine(var f: Text; ATime: Integer; ACurrent: Extended;
  AVoltage: Extended);
begin
  WriteLn(f, ATime, ',', #9, MyFloatStr(ACurrent), ',', #9, MyFloatStr(AVoltage));
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  I: Integer;
  Data: string;
  f1, f2: Extended;
begin
  SetLength(Data, 2);
  for I := 0 to 2 do
  begin
    Data[1] := Chr(StrToInt(Copy(memStepLog.Lines.Strings[I], 1, 3)));
    Data[2] := Chr(StrToInt(Copy(memStepLog.Lines.Strings[I], 5, 3)));
//    memStepLog.Lines.Add(IntToHex(Ord(Data[1]), 2) + '  ' + IntToHex(Ord(Data[2]), 2));
    f1 := DecodeCharge(Data);
    f2 := NewDecodeCharge(Data);
    memStepLog.Lines.Add('AsChrg: ' + FloatToStrF(f1, ffFixed, 18, 16) + ' AsNewChrg:' +
       FloatToStrF(f2, ffFixed, 18, 16));
  end;
  //memStepLog.Lines.Add(FloatToStr((41*2400 + 10*255) / 10000));
end;


procedure TfrmMain.SaveCSV(AFile: string);
var
  f: Text;
  I: Integer;
begin
  AssignFile(f, AFile);
  Rewrite(f);
  for I := 0 to Length(FData) - 1 do
  begin
    SaveCSVLine(f, FData[I].vTime, FData[I].vCurrent, FDAta[I].vVoltage);
  end;
  Flush(f);
  CloseFile(f);
end;

function TfrmMain.GetHexPacketFromIni(AIniFile: TMyIniFile; ASection: string;
  AIdent: string; ADefault: string): string;
var
  s, r: string;
  P: Integer;
begin
  s := UpperCase(AIniFile.ReadString(ASection, AIdent, ADefault));
  r := '';
  P := 1;
  while P <= Length(s) do
  begin
    r := r + Chr(HexToOrd(Copy(s, P, 2)));
    Inc(P, 2);
    if P <= Length(s) then
    begin
      if s[P] = ' ' then
      begin
        Inc(P);
      end;
    end;
  end;
  Result := r;
end;

procedure TfrmMain.LoadPackets;
var
  ini: TMyIniFile;
  I, P: Integer;
  N: Integer;
  s: string;
  Sec: string;
begin
  rgCharge.Items.Clear;
  rgDisCharge.Items.Clear;
  SetLength(FPackets, 0);
  ini := TMyIniFile.Create(FConfFile);
  N := 0;
  I := 0;
  repeat
    Sec := IntToStr(I);
    s := ini.ReadString(Sec, cCommand, '');
    if s > '' then
    begin
      N := 0;
      SetLength(FPackets, Length(FPackets) + 1);
      P := Length(FPackets) - 1;
      with FPackets[P] do
      begin
        Command := UpperCase(s);
        Start := GetHexPacketFromIni(ini, Sec, cStart);
        Adjust := GetHexPacketFromIni(ini, Sec, cAdjust);
        Cont := GetHexPacketFromIni(ini, Sec, cCont);
        Name := ini.ReadString(Sec, cName, 'Noname');
        AutoOff := GetHexPacketFromIni(ini, Sec, cAutoOff, 'FF');
        VoltInfo := ini.ReadFloat(Sec, cVoltInfo, 0);
        s := ini.ReadString(Sec, cTestVal, 'I');
        if s = 'P' then
        begin
          TestVal := tvPower;
        end else if s = 'R' then
        begin
          TestVal := tvResistance;
        end else
        begin
          TestVal := tvCurrent;
        end;
        s := ini.ReadString(Sec, cMethod, '');
        if s = cCharge then
        begin
          Method := mCharge;
          rgCharge.Items.AddObject(Name, TObject(Pointer(P)));
        end else if s = cDischarge then
        begin
          Method := mDischarge;
          rgDisCharge.Items.AddObject(Name, TObject(Pointer(P)));
        end else if s = cChargeCV then
        begin
          Method := mChargeCV;
          rgCharge.Items.AddObject(Name, TObject(Pointer(P)));
        end else
        begin
          Method := mNone;
        end;
      end;
    end else
    begin
      Inc(N);
    end;
    Inc(I);
  until N > 10; // Allow for a spacing of 10 in settings file

  SetLength(FModels, 0);
  N := 0;
  I := 0;
  repeat
    Sec := IntToStr(I);
    s := ini.ReadString(cModels, cModelName + Sec, '');
    if s > '' then
    begin
      N := 0;
      SetLength(FModels, Length(FModels) + 1);
      with FModels[Length(FModels) - 1] do
      begin
        FModels[Length(FModels) - 1].Name := s;
        ConnState := csNone;
        Ident := ini.ReadInteger(s, cIdent, 0);
        IFactor := ini.ReadFloat(s, cIFactor, 1);
        UFactor := ini.ReadFloat(s, cUFactor, 1);
        ConnPackets.Connect := GetHexPacketFromIni(ini, s, cConnect);
        ConnPackets.Disconnect := GetHexPacketFromIni(ini, s, cDisconnect);
        ConnPackets.Stop := GetHexPacketFromIni(ini, s, cStop);
      end;
    end else
    begin
      Inc(N);
    end;
    Inc(I);
  until N > 10;
  FConn.Connect := GetHexPacketFromIni(ini, cConn, cConnect);
  FConn.Disconnect := GetHexPacketFromIni(ini, cConn, cDisconnect);
  FConn.Stop := GetHexPacketFromIni(ini, cConn, cStop);
  ini.Free;
end;

function TfrmMain.NewMakePacket(Packet: Integer; AType: TSendMode): string;
begin
  case FPackets[Packet].Method of
    mCharge:
    begin
      Result := MakePacket2(Packet, AType, edtTestVal.Value, edtCells.Value, edtCutTime.Value);
    end;
    mChargeCV:
    begin
      Result := MakePacket2(Packet, AType, edtTestVal.Value, edtChargeV.Value, edtCutTime.Value);
    end;
    mDischarge:
    begin
      Result := MakePacket2(Packet, AType, edtTestVal.Value, edtCutV.Value, edtCutTime.Value);
    end;
  end;
end;

function TfrmMain.MakePacket2(Packet: Integer; SendMode: TSendMode; TestVal,
  SecondParam: Extended; ATime: Integer): string;
var
  p1, p2, p3: string;
  T: Extended;
  LTime: Integer;
begin
  Result := '';
  case SendMode of
  smStart:
    Result := FPackets[Packet].Start;
  smCont:
    Result := FPackets[Packet].Cont;
  smAdjust:
    Result := FPackets[Packet].Adjust;
  smConnStop:;
  smConnect:;
  smDisconnect:;
  end;
  if Result > '' then
  begin
    if ATime = 250 then  //250 is a forbidden value for some reason
    begin
      LTime := 249;
     end else
     begin
       LTime := ATime;
    end;
    p3 := EncodeTimer(LTime);
    case FPackets[Packet].TestVal of
      tvCurrent:
      begin
        p1 := EncodeCurrent(TestVal);
        if FPackets[Packet].Method = mCharge then
        begin
          p2 := Chr(0) + Chr(Round(SecondParam));
        end else
        begin
          p2 := EncodeVoltage(SecondParam);
        end;
      end;
      tvPower:
      begin
        p1 := EncodePower(TestVal);
        p2 := EncodeVoltage(SecondParam);
      end;
      tvResistance:
      begin
        T := FLastU / TestVal;
        p1 := EncodeCurrent(T);
        p2 := EncodeVoltage(SecondParam);
      end;
    end;
    Result[3] := p1[1];
    Result[4] := p1[2];
    Result[5] := p2[1];
    Result[6] := p2[2];
    Result[7] := p3[1];
    Result[8] := p3[2];
//    Result[crcsendpos] := checksum(Result, crcsendpos);
  end;
end;

function TfrmMain.MakeConnPacket(SendMode: TSendMode): string;
begin
  Result := '';
  if FModel >- 1 then
  begin
    case SendMode of
      smConnStop:
      begin
        Result := FModels[FModel].ConnPackets.Stop;
      end;
      smConnect:
      begin
        Result := FConn.Connect; // This should not happen, defaulting to general connect string
        //Result := FModels[FModel].ConnPackets.Connect;
      end;
      smDisconnect:
      begin
        Result := FModels[FModel].ConnPackets.Disconnect;
      end;
    end;
  end else
  begin
    case SendMode of
      smConnStop:
      begin
        Result := FConn.Stop;
      end;
      smConnect:
      begin
        Result := FConn.Connect;
      end;
      smDisconnect:
      begin
        Result := FConn.Disconnect;
      end;
    end;
  end;
end;

procedure TfrmMain.EBCBreak(Force: Boolean);
begin
  btnSkip.Enabled := False;
  if frmSettings.cgSettings.Checked[cForceMon] then
  begin
    tbxMonitor.Checked := True;
  end;
  FChecks.TimerRunning := False;
  SendData(MakeConnPacket(smConnStop));
  RunModeOffOrMonitor;
  if mniDoLog.Checked then
  begin
    Flush(FLogFile);
    CloseFile(FLogFile);
  end;
  mniDoLog.Enabled := True;
  if FInProgram then
  begin
    LogStep;
    if Force then
    begin
      FInProgram := False;
    end else if FWaitCounter = 0 then
    begin
      LoadStep;
    end;
  end;
end;

function TfrmMain.GetModelIndex(AModel: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FModels) to High(FModels) do
  begin
    if AModel = FModels[I].Ident then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TfrmMain.stTextClick(Sender: TObject);
begin
  if Sender is TStaticText then
  begin
    case (Sender as TStaticText).Tag of
      cstEnergy:
      begin
        FShowJoule := not FShowJoule;
        stText[cstEnergy].Caption := GetEnergy(FEnergy);
      end;
      cstCapacity:
      begin
        FShowCoulomb := not FShowCoulomb;
        stText[cstCapacity].Caption := GetCharge(FCurrentCapacity[caEBC]);
      end;
    end;
  end;
end;

function TfrmMain.GetEnergy(AEnergy: Extended): string;
begin
  if FShowJoule then
  begin
    Result := MyFloatStr(AEnergy * 3.600) + 'kJ';
  end else
  begin
    Result := MyFloatStr(AEnergy) + 'Wh';
  end;
end;

function TfrmMain.GetCharge(ACharge: Extended): string;
begin
  if FShowCoulomb then
  begin
    Result := FloatToStrF(ACharge * 3600, ffFixed, 18, 1) + 'As';
  end else
  begin
    Result := MyFloatStr(ACharge) + 'Ah';
  end;
end;

procedure TfrmMain.FreezeEdits;
begin
  rgCharge.Enabled := False;
  rgDischarge.Enabled := False;
  edtCells.Enabled := False;
  edtChargeV.Enabled := False;
  edtCutA.Enabled := False;
  edtCutM.Enabled := False;
  edtTestVal.Enabled := False;
  edtCutTime.Enabled := False;
  edtCutEnergy.Enabled := False;
  chkCutEnergy.Enabled := False;
  edtCutCap.Enabled := False;
  chkCutCap.Enabled := False;
  edtCutV.Enabled := False;
  frmSettings.edtIntTime.Enabled := False;
end;

procedure TfrmMain.UnlockEdits;
var
  I: Integer;
begin
  rgCharge.Enabled := True;
  rgDischarge.Enabled := True;
  edtCutA.Enabled := True;
  edtCutM.Enabled := True;
  edtTestVal.Enabled := True;
  edtCutTime.Enabled := True;
  edtCutEnergy.Enabled := True;
  chkCutEnergy.Enabled := True;
  edtCutCap.Enabled := True;
  chkCutCap.Enabled := True;
  edtCutV.Enabled := True;
  frmSettings.edtIntTime.Enabled := True;
  I := GetPointer(rgCharge);
  if I >-1 then if FPackets[I].Method = mChargeCV then
  begin
    edtChargeV.Enabled := True;
    edtCells.Enabled := False;
    edtChargeV.Value := FPackets[I].VoltInfo;
  end else
  begin
    edtChargeV.Enabled := False;
    edtCells.Enabled := True;
    edtChargeV.Value := edtCells.Value * FPackets[I].VoltInfo;
  end;
end;

procedure TfrmMain.EnumerateSerial;
const
  cDevicePath = '/dev';
var
  M: TMenuItem;
  S: TSearchRec;
begin
  if FindFirst(FormatPath(cDevicePath + PathDelim + 'tty*'), faAnyFile, S) = 0 then
  begin
    repeat
      if (Pos('ttyS', S.Name) > 0) or (Pos('ttyUSB', S.Name) > 0 ) then
      begin
        M := TMenuItem.Create(Self);
        M.Caption := FormatPath(cDevicePath + PathDelim + S.Name);
        M.OnClick := @mniSerialClick;
        mnSerial.Items.Add(M);
      end;
    until FindNext(S) <> 0;
  end;
  FindClose(S);
end;

procedure TfrmMain.RunModeOffOrMonitor;
begin
  if tbxMonitor.Checked then
  begin
    SetRunMode(rmMonitor);
  end else
  begin
    SetRunMode(rmNone);
  end;
end;

procedure TfrmMain.SetRunMode(ARunMode: TRunMode);
begin
  FRunMode := ARunMode;
  case FRunMode of
    rmNone:
    begin
      stRunMode.Caption := 'N';
    end;
    rmMonitor:
    begin
      stRunMode.Caption := 'M';
    end;
    rmCharging:
    begin
      stRunMode.Caption := 'C';
    end;
    rmDischarging:
    begin
      stRunMode.Caption := 'D';
    end;
    rmDischargingCR:
    begin
      stRunMode.Caption := cR;
    end;
    rmWait:
    begin
      stRunMode.Caption := 'W';
    end;
    rmLoop:
    begin
      stRunMode.Caption := 'L';
    end;
    rmEnd:
    begin
      stRunMode.Caption := 'E';
    end;
  end;
end;

procedure TfrmMain.TimerOff;
begin
  lblTimer.Caption := '';
  tmrWait.Enabled := False;
  FEndWaitVoltage := FLastU;
  LogStep;
  LoadStep;
end;

function TfrmMain.GetStepNum: string;
var
  I: Integer;
begin
  Result := '';
  for I := High(FSteps) downto Low(FSteps) do
  begin
    if FSteps[I].Command = 'LOOP' then
    begin
      Result := AlignR(IntToStr(FSteps[I].LoopCounter), 3) + ':' + Result;
    end;
  end;
{  if Length(Result) > 0 then
  begin
    if Result[Length(Result)] = ':' then
    begin
      Result := Copy(Result, 1, Length(Result) - 1);
    end;
  end;}
  Result := Result + IntToStr(FProgramStep);
end;

procedure TfrmMain.LogStep;
var
  I: Integer;
  s: string;
  col: array [2..High(cCol)] of string;
begin
  for I := Low(col) to High(col) do
  begin
    col[I] := '';
  end;

  col[2] := FSteps[FCurrentStep].Command;
  col[5] := MyTimeToStr(Now - FStepTime);
  case FSteps[FCurrentStep].Mode of
    rmNone:;
    rmCharging, rmDischarging, rmDischargingCR:
    begin
      col[3] := MyFloatStr(FCurrentCapacity[caEBC]);
      col[4] := MyFloatStr(FEnergy);//stText[cstEnergy].Caption;
      col[6] := MyFloatStr(FStartU);
      col[7] := MyFloatStr(FLastU);
      col[8] := MyFloatStr(FLastI);
    end;
    rmWait:
    begin
      col[2] := col[2] + ' ' + IntToStr(FSteps[FCurrentStep].CutTime);
      col[6] := MyFloatStr(FBeginWaitVoltage);
      col[7] := MyFloatStr(FEndWaitVoltage);
    end;
    rmLoop:
    begin
      col[2] := col[2] + ' ' + IntToStr(FSteps[FCurrentStep].Loop);
    end;
    rmEnd:
    begin

    end;
  end;
  s := AlignR(GetStepNum, cCol[1]);
  for I := Low(col) to High(col) do
  begin
    s := s + edtDelim.Text + AlignR(col[I], cCol[I]);
  end;

  memStepLog.Lines.Add(s);
end;

procedure TfrmMain.OffSetting;
begin
  tsCharge.Enabled := True;
  tsDisCharge.Enabled := True;
  btnStop.Enabled := False;
  btnStart.Enabled := True;
  btnAdjust.Enabled := True;
  tbxMonitor.Enabled := True;
  FInProgram := False;
  frmStep.memStep.Enabled := True;
  btnProg.Caption := cEdit;
  tmrWait.Enabled := False;
  lblTimer.Caption := '';
  lblCapI.Enabled := False;
  shaCapI.Enabled := False;
  shaCapI.Brush.Color := clDefault;
  lblStep.Caption := '';
  lblStepNum.Caption := '';
  btnSkip.Enabled := False;
  UnlockEdits;
end;

procedure TfrmMain.LoadSettings;
var
  ini: TMyIniFile;
  s, t: string;
  I: Integer;
  Found: Boolean;
begin
  ini := TMyIniFile.Create(FConfFile);

  with FDefault do
  begin
    ChargeI := ini.ReadFloat(cDefault, cChargeCurrent, 0.06);
    DischargeI := ini.ReadFloat(cDefault, cDischargeCurrent, 0.06);
    ConstantU := ini.ReadFloat(cDefault, cConstantVoltage, 5);
    DischargeR := ini.ReadFloat(cDefault, cDischargeResistance, 10.0);
    DischargeP := ini.ReadFloat(cDefault, cDischargePower, 10.0);
    Cells := ini.ReadInteger(cDefault, cCells, 1);
    ModeName := ini.ReadString(cDefault, cModeCommand, '');
  end;

  frmSettings.rgStart.ItemIndex := ini.ReadInteger(cStartup, cStartSelection, 0);
  s := cSelection + '_' + IntToStr(frmSettings.rgStart.ItemIndex);
  rgCharge.ItemIndex := ini.ReadInteger(s, cChargeIndex, -1);
  rgDischarge.ItemIndex := ini.ReadInteger(s, cDischargeIndex, -1);
  edtCells.Value := ini.ReadInteger(s, cCells, 1);
  edtTestVal.Value := ini.ReadFloat(s, cTestVal, 0.06);
  pcProgram.TabIndex := ini.ReadInteger(s, cTabIndex, 0);

  edtCutA.Value := ini.ReadFloat(s, cCutA, FDefault.DischargeI);
  edtCutV.Value := ini.ReadFloat(s, cCutV, 1.00);
  edtCutTime.Value := ini.ReadInteger(s, cMaxTime, 0);
  edtCutM.Value := ini.ReadInteger(s, cCutATime, 0);

  frmSettings.edtProgFile.Text := ini.ReadString(cSettings, cProgFile, '');
  sdCSV.InitialDir := ini.ReadString(cSettings, cSaveDir, FAppDir);
  sdPNG.InitialDir := sdCSV.InitialDir;
  sdLogCSV.InitialDir := ini.ReadString(cSettings, cLogDir, FAppDir);
  t := ini.ReadString(cSettings, cStepDir, FAppDir);
  frmStep.SetInitialDir(t);

  for I := 0 to frmSettings.cgSettings.Items.Count - 1 do
  begin
    frmSettings.cgSettings.Checked[I] := ini.ReadBool(cSettings, cChkSetting + '_' + IntToStr(I), False);
  end;
  if frmSettings.cgSettings.Checked[cAutoLoad] then
  begin
    frmStep.memStep.Lines.LoadFromFile(frmSettings.edtProgFile.FileName);
    stStepFile.Caption := ExtractFileName(frmSettings.edtProgFile.FileName);
    frmStep.sdSave.FileName := frmSettings.edtProgFile.FileName;
    frmStep.odOpen.FileName := frmSettings.edtProgFile.FileName;
    frmStep.Caption := frmSettings.edtProgFile.FileName;
  end;
  frmSettings.edtIntTime.Value := ini.ReadInteger(cSettings, cIntTime, 60);
  tbxMonitor.Checked := ini.ReadBool(cSettings, cMonitor, True);

  frmMain.Width := ini.ReadInteger(cAppSec, cWinWidth, frmMain.Width);
  frmMain.Height := ini.ReadInteger(cAppSec, cWinHeight, frmMain.Height);
  if ini.ReadBool(cAppSec, cWinMaximized, False) then
  begin
    frmMain.WindowState := wsMaximized;
  end;
  edtDevice.Text := ini.ReadString(cAppSec, cSerial, '/dev/ttyUSB0');

  Found := False;
  for I := 0 to mnSerial.Items.Count - 1 do
  begin
    if edtDevice.Text = mnSerial.Items.Items[I].Caption then
    begin
      Found := True;
      Break;
    end;
  end;
  if not Found then
  begin
    if mnSerial.Items.Count > -1 then
    begin
      edtDevice.Text := mnSerial.Items.Items[0].Caption;
    end;
  end;
  ini.Free;
  SetSettings;
end;

procedure TfrmMain.SaveSettings;
var
  ini: TMyIniFile;
  s: string;
  I: Integer;
begin
  ini := TMyIniFile.Create(FConfFile);
  ini.WriteInteger(cStartup, cStartSelection, frmSettings.rgStart.ItemIndex);
  s := cSelection + '_' + IntToStr(frmSettings.rgStart.ItemIndex);
  if not ini.ReadBool(s, cReadOnly, True) then
  begin
    ini.WriteInteger(s, cChargeIndex, rgCharge.ItemIndex);
    ini.WriteInteger(s, cDischargeIndex, rgDischarge.ItemIndex);
    ini.WriteInteger(s, cCells, edtCells.Value);
    ini.WriteFloat(s, cTestVal, edtTestVal.Value);
    ini.WriteInteger(s, cTabIndex, pcProgram.TabIndex);
    ini.WriteFloat(s, cCutA, edtCutA.Value);
    ini.WriteFloat(s, cCutV, edtCutV.Value);
    ini.WriteInteger(s, cMaxTime, edtCutTime.Value);
    ini.WriteInteger(s, cCutATime, edtCutM.Value);
  end;
  ini.WriteString(cSettings, cProgFile, frmSettings.edtProgFile.Text);
  ini.WriteString(cSettings, cSaveDir, sdCSV.InitialDir);
  ini.WriteString(cSettings, cLogDir, sdLogCSV.InitialDir);
  ini.WriteString(cSettings, cStepDir, frmStep.sdSave.InitialDir);
  ini.WriteInteger(cSettings, cIntTime, frmSettings.edtIntTime.Value);

  for I := 0 to frmSettings.cgSettings.Items.Count - 1 do
  begin
    ini.WriteBool(cSettings, cChkSetting + '_' + IntToStr(I), frmSettings.cgSettings.Checked[I]);
  end;
  ini.WriteBool(cSettings, cMonitor, tbxMonitor.Checked);


  ini.WriteBool(cAppSec, cWinMaximized, (frmMain.WindowState = wsMaximized));
  ini.WriteInteger(cAppSec, cWinWidth, frmMain.Width);
  ini.WriteInteger(cAppSec, cWinHeight, frmMain.Height);
  ini.WriteString(cAppSec, cSerial, edtDevice.Text);
  ini.Free;
end;

procedure TfrmMain.SetSettings;
begin
  if frmSettings.cgSettings.Checked[cAutoLoad] then
  begin
    frmStep.memStep.Lines.LoadFromFile(frmSettings.edtProgFile.Text);
    frmStep.Compile;
  end;
  if not frmSettings.cgSettings.Checked[cRememberSaveDir] then
  begin
    sdPNG.InitialDir := FAppDir;
    sdCSV.InitialDir := FAppDir;
  end;
  if not frmSettings.cgSettings.Checked[cRememberAutoLog] then
  begin
    sdLogCSV.InitialDir := FAppDir;
  end;
  if not frmSettings.cgSettings.Checked[cRememberStepDir] then
  begin
    frmStep.SetInitialDir(FAppDir);
  end;
  if pcProgram.ActivePage = tsDischarge then
  begin
    FixLabels(GetPointer(rgDischarge));
  end;
end;

procedure TfrmMain.SetupChecks;
begin
  FChecks.TimerRunning := False;
  if FRunMode = rmCharging then
  begin
    FChecks.cCurrent := edtCutA.Value;
    FChecks.cDwelltime := edtCutM.Value;
  end;
  FChecks.cCapacity := cNaN;
  if chkCutCap.Checked then
  begin
    FChecks.cCapacity := edtCutCap.Value;
    if FChecks.cCapacity < 0.0001 then
    begin
      chkCutCap.Checked := False;
    end;
  end;
  FChecks.cEnergy := cNaN;
  if chkCutEnergy.Checked then
  begin
    FChecks.cEnergy := edtCutEnergy.Value;
    if FChecks.cEnergy < 0.0001 then
    begin
      chkCutEnergy.Checked := False;
    end;
  end;
  DoLog('cCurrent: ' + FloatToStr(FChecks.cCurrent));
  DoLog('Time: ' + IntToStr(FChecks.cDwellTime));
  DoLog('cCapacity: ' + FloatToStr(FChecks.cCapacity));
  DoLog('cEnergy: ' + FloatToStr(FChecks.cEnergy));
end;

procedure TfrmMain.FixLabels(APacket: Integer);
begin
  if APacket > -1 then
  begin
    case FPackets[APacket].TestVal of
      tvCurrent:
      begin
        edtTestVal.EditLabel.Caption := 'Current';
        lblTestUnit.Caption := cA;
      end;
      tvPower:
      begin
        edtTestVal.EditLabel.Caption := 'Power';
        lblTestUnit.Caption := cP;
      end;
      tvResistance:
      begin
        edtTestVal.EditLabel.Caption := 'Resistance';
        lblTestUnit.Caption := cR;
      end;
    end;
  end else
  begin
    edtTestVal.EditLabel.Caption := cCurrent;
    lblTestUnit.Caption := cA;
  end;
end;

procedure TfrmMain.DoLog(AText: string);
begin
  if memLog.Lines.Count > 10000 then
  begin
    memLog.Lines.Delete(0);
  end;
  memLog.Lines.Add(AText);

  memLog.VertScrollBar.Position := 1000000;
//  SendMessage(memLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;


procedure TfrmMain.DoHexLog(AText: string);
begin
  if memStepLog.Lines.Count > 10000 then
  begin
    memStepLog.Lines.Delete(0);
  end;
  memStepLog.Lines.Add(AText);

  memStepLog.VertScrollBar.Position := 1000000;
//  SendMessage(memLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TfrmMain.StartLogging;
begin
  if mniDoLog.Checked then
  begin
    if sdLogCSV.FileName > '' then
    begin
      AssignFile(FLogFile, sdLogCSV.FileName);
    end else
    begin
      mniDoLog.Checked := False;
    end;
  end;
end;

function TfrmMain.FindPacket(AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FPackets) - 1 do
  begin
    if Pos(AName, FPackets[I].Command) > 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TfrmMain.GetPointer(ARadioGroup: TRadioGroup): Integer;
begin
  Result := -1;
  if Assigned(ARadioGroup) then
  begin
    if ARadioGroup.ItemIndex > -1 then
    begin
      Result := Integer(Pointer(ARadioGroup.Items.Objects[ARadioGroup.ItemIndex]));
    end;
  end;
end;


procedure TfrmMain.LoadStep;
var
  I: Integer;
  P2: Extended;
  u: string;
  DoSend: Boolean;
  DoUpdate: Boolean;
begin
  {$push}
  {$boolEval off}
  while (FProgramStep < Length(FSteps)) and (Pos('//', FSteps[FProgramStep].Command) > 0) do
  {$pop}
  begin
    Inc(FProgramStep);
  end;
  if FProgramStep < Length(FSteps) then
  begin
    DoSend := True;
    DoUpdate := True;
    if FSteps[FCurrentStep].Mode in [rmDischarging, rmDischargingCR] then
    begin
      FCurrentDisCapacity := FCurrentCapacity[caEBC];
    end;
    FCurrentStep := FProgramStep;
    FEnergy := 0;
    FCurrentCapacity[caLocal] := 0;
    FWaitCounter := 0;
    FStepTime := Now;
    with FSteps[FProgramStep] do
    begin
      I := FindPacket(Command);
      lblStepNum.Caption := IntToStr(FProgramStep + 1);
      lblStep.Caption := ' ' + Command + ' ';
      SetRunMode(Mode);
      if I > -1 then
      begin
        FixLabels(I);
        FPacketIndex := I;
        case Mode of
          rmCharging:
          begin
            if FPackets[I].Method = mChargeCV then
            begin
              P2 := CV;
            end else
            begin
              P2 := Cells;
            end;
            u := cA;
          end;
          rmDischarging:
          begin
            P2 := CutVolt;
            if FPackets[I].TestVal = tvPower then
            begin
              u := cP;
            end else
            begin
              u := cA;
            end;
          end;
          rmDischargingCR:
          begin
            P2 := CutVolt;
            u := cR;
          end;
          rmWait:
          begin
            FBeginWaitVoltage := FLastU;
            FWaitCounter := CutTime * 60;
            tmrWait.Enabled := True;
            edtTestVal.Value := 0.0;
            DoSend := False;
          end;
          rmLoop:
          begin
            FWaitCounter := 1;
            edtTestVal.Value := 0.0;
            DoSend := False;
            DoUpdate := False;
            if Loop > 0 then
            begin
              if CapI then
              begin
                lblCapI.Enabled := True;
                lblCapI.Caption := cCapI +
                  MyFloatStr(FCurrentDisCapacity) +
                  ' ≥ ' + MyFloatStr(FLastDisCapacity);
                shaCapI.Enabled := True;
                shaCapI.Brush.Color := clLime;
              end;
{              if EneI then
              begin
                lblCapI.Enabled := True;
                lblCapI.Caption := cEneI +
                  FloatToStrF(FCurrentDisEnergy, ffFixed, 18, 3) +
                  ' ≥ ' + FloatToStrF(FLastDisEnergy, ffFixed, 18, 3);
                shaCapI.Enabled := True;
                shaCapI.Brush.Color := clLime;
              end;
}
              if (not (CapI or EneI)) or (FCurrentDisCapacity >= FLastDisCapacity) then
              begin
                Dec(Loop);
                FLastDisCapacity := FCurrentDisCapacity;
                LogStep;
                FProgramStep := 0;
                edtTestVal.Value := 0.0;
                LoadStep;
                Dec(FProgramStep); // Increments in end of recursive call and this call.
                memStepLog.Lines.Add('---');
              end else
              begin
                lblCapI.Enabled := False;
                lblCapI.Caption := cCapI;
                shaCapI.Brush.Color := clDefault;
                tmrWait.Enabled := True; // Triggers next step if loop is finished
              end;
            end else
            begin
              tmrWait.Enabled := True; // Triggers next step if loop is finished
            end;
            Inc(LoopCounter);
          end;
          rmEnd:
          begin
            LogStep;
            FInProgram := False;
            EBCBreak;
            OffSetting;
            DoUpdate := False;
            DoSend := False;
          end;
        end;
        if DoUpdate then
        begin
          edtCutEnergy.Value := CutEnergy;
          chkCutEnergy.Checked := CutEnergy > 0.0001;
          edtCutCap.Value := CutCap;
          chkCutCap.Checked := CutCap > 0.0001;
          edtTestVal.Value := TestVal;
          lblTestUnit.Caption := u;
          edtCutTime.Value := CutTime;
          edtCutA.Value := CutAmp;
          edtCutM.Value := CutAmpTime;
          if Mode = rmCharging then
          begin
            if Pos(cCmdCCCV, Command) > 0 then
            begin
              edtChargeV.Value := CV;
            end else
            begin
              edtCells.Value := Trunc(P2);
            end;
          end else if Mode in [rmDischarging, rmDischargingCR] then
          begin
            edtCutV.Value := CutVolt;
          end;
        end;
        if DoSend then
        begin
          SetupChecks;
          FSampleCounter := 0;
          DoLog('Autooff char: ' + IntToHex(Ord(FPackets[FPacketIndex].AutoOff[1]), 2));
          FStartU := FLastU;
          SendData(MakePacket2(I, smStart, TestVal, P2, CutTime));
        end;
      end else
      begin
        memStepLog.Lines.Add('Syntax error: ' + Command);
      end;
    end;
    Inc(FProgramStep);
    btnSkip.Enabled := True;
  end else
  begin
    FInProgram := False;
    EBCBreak;
    OffSetting;
  end;
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  try
    FChecks.TimerRunning := False;
    if FConnState = csNone then
    begin
      lblModel.Caption := cConnecting;
      FConnState := csConnecting;
      Serial.Device := edtDevice.Text;
      Serial.OnRxData := @SerialRec;
      Serial.Open;
      SendData(MakeConnPacket(smConnStop));
      SendData(MakeConnPacket(smConnect));
      btnConnect.Caption := '&Disconnect';
      RunModeOffOrMonitor;
      btnStart.Enabled := True;
      FLastU := 0;
      shpConn.Visible := True;
      mnSerial.Items.Enabled := False;
    end else
    begin
      SendData(MakeConnPacket(smDisconnect));
      btnConnect.Caption := '&Connect';
      lblModel.Caption := cNotConnected;
      tbxMonitor.Enabled := False;
      shpConn.Visible := False;
      FConnState := csNone;
      FModel := -1;
      Serial.Close;
      mnSerial.Items.Enabled := True;
    end;
  except
    ShowMessage('Could not connect to ' + edtDevice.Text);
  end;
end;

procedure TfrmMain.btnContClick(Sender: TObject);
var
  s: string;
begin
  s := NewMakePacket(FPacketIndex, smCont);
  SendData(s);
end;

procedure TfrmMain.btnAdjustClick(Sender: TObject);
var
  s: string;
begin
  s := NewMakePacket(FPacketIndex, smAdjust);
  SendData(s);
end;

procedure TfrmMain.SavePNGExecute(Sender: TObject);
begin
end;

procedure TfrmMain.mniSerialClick(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    edtDevice.Text := (Sender as TMenuItem).Caption;
  end;
end;


procedure TfrmMain.pcProgramChange(Sender: TObject);
begin
  case pcProgram.TabIndex of
    0:
      btnStart.Enabled := rgCharge.ItemIndex > -1;
    1:
      btnStart.Enabled := rgDischarge.ItemIndex > -1;
    2:
    begin
      btnStart.Enabled := Length(frmStep.memStep.Text) > 2;
    end;
  end;
end;

procedure TfrmMain.btnProgClick(Sender: TObject);
begin
  frmStep.ShowModal;
  stStepFile.Caption := ExtractFileName(frmStep.odOpen.FileName);
  btnStart.Enabled := Length(frmStep.memStep.Text) > 2;
end;

procedure TfrmMain.btnSkipClick(Sender: TObject);
begin
  btnSkip.Enabled := False;
  if FInProgram then
  begin
    if FRunMode = rmWait then
    begin
      FWaitCounter := 1;
//      TimerOff;
    end else
    begin
      EBCBreak;
    end;
  end;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
var
  s: string;
begin
  FPacketIndex := -1;
  if pcProgram.ActivePage = tsCharge then
  begin
    tsDisCharge.Enabled := False;
    if rgCharge.ItemIndex > -1 then
    begin;
      FPacketIndex := GetPointer(rgCharge);
      SetRunMode(rmCharging);
    end;
  end else if pcProgram.ActivePage = tsDischarge then
  begin
    tsCharge.Enabled := False;
    if rgDisCharge.ItemIndex > -1 then
    begin;
      FPacketIndex := GetPointer(rgDisCharge);
      SetRunMode(rmDischarging);
    end;
  end else if pcProgram.ActivePage = tsProgram then
  begin
    //  if not FfrmStep.Compiled then
    begin
      frmStep.Compile;
    end;
    frmStep.memStep.Enabled := False;
    btnProg.Caption := cView;
    btnStart.Enabled := False;
    btnAdjust.Enabled := False;
    btnCont.Enabled := False;
    btnStop.Enabled := True;
    FProgramStep := 0;
    FInProgram := True;
    memStepLog.Lines.Clear;
    memStepLog.Lines.Add(cColumns);
  end;
  if FPacketIndex > -1 then
  begin
    if FPackets[FPacketIndex].TestVal = tvResistance then
    begin
      SetRunMode(rmDischargingCR);
    end;
  end;
  if FInProgram or (FRunMode in [rmCharging, rmDischarging, rmDischargingCR]) then
  begin
    btnStart.Enabled := False;
    btnAdjust.Enabled := True;
    tbxMonitor.Enabled := False;
    btnStop.Enabled := True;
    SetLength(FData, 0);
    lsCurrent.Clear;
    lsVoltage.Clear;
    lsInvisibleVoltage.Clear;
    lsInvisibleCurrent.Clear;
    SetupChecks;
    StartLogging;
    FSampleCounter := 0;
    FDeltaIndex := 0;
    FLastDisCapacity := -1;
    FCurrentCapacity[caLocal] := 0;
    FCurrentCapacity[caEBC] := 0;
    FCurrentDisCapacity := 0;
    FEnergy := 0;
    mniDoLog.Enabled := False;
    FreezeEdits;
    FIntTime := frmSettings.edtIntTime.Value * 1000;
    FStartTime := Now;
    if pcProgram.ActivePage = tsProgram then
    begin
      LoadStep;
      btnSkip.Enabled := True;
    end else
    begin
      s := NewMakePacket(FPacketIndex, smStart);
      FStartU := FLastU;
      SendData(s);
    end;
  end;
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  //FInProgram := False;
  EBCBreak(True);
  OffSetting;
end;

procedure TfrmMain.chkCutCapChange(Sender: TObject);
begin
  if chkCutCap.Checked then
  begin
    edtCutCap.Enabled := True;
    lblCutCap.Enabled := True;
  end else
  begin
    edtCutCap.Enabled := False;
    lblCutCap.Enabled := False;
  end;
end;

procedure TfrmMain.chkCutEnergyChange(Sender: TObject);
begin
  if chkCutEnergy.Checked then
  begin
    edtCutEnergy.Enabled := True;
    lblCutEnergy.Enabled := True;
  end else
  begin
    edtCutEnergy.Enabled := False;
    lblCutEnergy.Enabled := False;
  end;
end;

procedure TfrmMain.edtCellsChange(Sender: TObject);
var
  I: Integer;
begin
  I := GetPointer(rgCharge);
  if I > -1 then
  begin
    edtChargeV.Value := edtCells.Value * FPackets[I].VoltInfo;
  end;
end;

procedure TfrmMain.edtCellsClick(Sender: TObject);
begin
  edtCellsChange(Sender)
end;

procedure TfrmMain.edtCellsEditingDone(Sender: TObject);
begin
  edtCellsChange(Sender)
end;

procedure TfrmMain.edtCellsExit(Sender: TObject);
begin
    edtCellsChange(Sender)
end;

procedure TfrmMain.edtCellsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  edtCellsChange(Sender)
end;

procedure TfrmMain.edtCellsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  edtCellsChange(Sender)
end;

procedure TfrmMain.edtCutTimeChange(Sender: TObject);
begin
{  if edtCutTime.Value = 250 then
  begin
    edtCutTime.Value := 249;
  end;}
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings;
  if FConnState = csConnected then
  begin
    SendData(MakeConnPacket(smDisconnect));
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
const
  cLeft = 2;
  cRight = 140;
var
  I: Integer;
  N: Integer;
//  A: TAction;
begin
  SetRunMode(rmNone);
  FConnState := csNone;
  FInProgram := False;
  FStartTime := Now;
  FShowJoule := False;
  FShowCoulomb := False;
  FDelta[0].Time := Now;
  FDelta[1].Time := Now;

  SetLength(stText, cstMax + 1);
  for I := Low(stText) to High(stText) do
  begin
    stText[I] := TStaticText.Create(Self);
    stText[I].Tag := I;
    stText[I].OnClick := @stTextClick;
    stText[I].Parent := gbStatus;
    stText[I].Font.Name := 'Liberation Mono';
    stText[I].Font.Size := 12;
    stText[I].Font.Style := [fsBold];
    stText[I].Height := 24;
    stText[I].Top := 25*(I div 2)+0;
    if Odd(I) then
    begin
      stText[I].Left := cRight;
      stText[I].SendToBack;
      stText[I].Width := gbStatus.Width - cRight - cLeft;
    end else
    begin
      stText[I].Left := cLeft;
      stText[I].BringToFront;
      stText[I].Width := cRight - cLeft + 2 ;
    end;
  end;

  stText[cstVoltage].Caption := '00.000V';
  stText[cstVoltage].Font.Color := clBlue;
  stText[cstCurrent].Caption := '00.000A';
  stText[cstCurrent].Font.Color := clRed;
  stText[cstTime].Caption := '00:00:00';
  stText[cstTime].Font.Color := clMaroon;
  stText[cstPower].Caption := '000.0W';
  stText[cstPower].Font.Color :=  $0050FF;//FF2600;
  stText[cstCapacity].Caption := '000.000Ah';
  stText[cstEnergy].Caption := '000.000Wh';

  lblModel.Caption := cNotConnected;
  stRunMode.BringToFront;
  N := 0;
  for I := 0 to rgCharge.Items.Count -1 do
  begin
    rgCharge.Items.Objects[I] := TObject(Pointer(N));
    Inc(N);
  end;
  for I := 0 to rgDisCharge.Items.Count -1 do
  begin
    rgDisCharge.Items.Objects[I] := TObject(Pointer(N));
    Inc(N);
  end;
  frmStep := TfrmStep.Create(Self);
  frmSettings := TfrmSettings.Create(Self);
  FAppDir := ExtractFilePath(Application.ExeName);
  FConfFile := ChangeFileExt(Application.ExeName, cConf);
  if not FileExists(FConfFile) then
  begin
    TextFileCopy(ChangeFileExt(Application.ExeName, cInit), FConfFile);
  end;
  FixLabels(-1);
  EnumerateSerial;
  LoadPackets;
  LoadSettings;
//  SetSettings;
  if sdLogCSV.FileName = '' then
  begin
    mniDoLog.Checked := False;
  end;
  OffSetting;
  rgCharge.OnClick := @rgChargeClick;
  rgDischarge.OnClick := @rgDischargeClick;
  btnStart.Enabled := False;
  memStepLog.Lines.Clear;
//  memStepLog.Lines.Add(cVersion);
  memStepLog.Lines.Add('041 155');
  memStepLog.Lines.Add('140 168');
  memStepLog.Lines.Add('144 109');

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  btnConnect.SetFocus;
end;

procedure TfrmMain.memLogChange(Sender: TObject);
begin

end;

procedure TfrmMain.mniLoadStepClick(Sender: TObject);
begin
  if frmStep.odOpen.Execute then
  begin
    frmStep.memStep.Lines.LoadFromFile(frmStep.odOpen.FileName);
    frmStep.Caption := frmStep.odOpen.FileName;
    frmStep.sdSave.FileName := frmStep.odOpen.FileName;
    stStepFile.Caption := ExtractFileName(frmStep.odOpen.FileName);
  end;
end;

procedure TfrmMain.mniSetCSVLogfileClick(Sender: TObject);
begin
  if sdLogCSV.Execute then
  begin
  end;
end;

procedure TfrmMain.mniSaveCSVClick(Sender: TObject);
begin
  if sdCSV.Execute then
  begin
    SaveCSV(sdCSV.FileName);
  end;
end;

procedure TfrmMain.mniSavePNGClick(Sender: TObject);
begin
  if sdPNG.Execute then
  begin
    Chart.SaveToFile(TPortableNetworkGraphic, sdPNG.FileName);
  end;
end;

procedure TfrmMain.mniSetNameClick(Sender: TObject);
var
  s: string;
begin
  s := '';
  if InputQuery('Set border name', 'Name', s) then
  begin
    frmMain.Caption := s;
  end;
end;

procedure TfrmMain.mniSettingsClick(Sender: TObject);
begin
  frmSettings.ShowModal;
  SaveSettings;
end;

procedure TfrmMain.rgChargeClick(Sender: TObject);
var
  I: Integer;
begin
  btnStart.Enabled := True;
  btnAdjust.Enabled := False;
  edtCells.Value := FDefault.Cells;
  edtTestVal.Value := FDefault.ChargeI;
  I := GetPointer(rgCharge);
  if I >-1 then if FPackets[I].Method = mChargeCV then
  begin
    btnStart.Enabled := True;
    edtChargeV.Enabled := True;
    edtCells.Enabled := False;
    edtChargeV.Value := FPackets[I].VoltInfo;
  end else
  begin
    edtChargeV.Enabled := False;
    edtCells.Enabled := True;
    edtChargeV.Value := edtCells.Value * FPackets[I].VoltInfo;
  end;
end;

procedure TfrmMain.rgDischargeClick(Sender: TObject);
var
  I: Integer;
begin
  I := GetPointer(rgDischarge);
  FixLabels(I);
  if (I > -1) {and (I <> FPacketIndex)} then
  begin
    btnStart.Enabled := True;
    case FPackets[I].TestVal of
      tvCurrent:
      begin
        edtTestVal.Value := FDefault.DischargeI;
      end;
      tvPower:
      begin
        edtTestVal.Value := FDefault.DischargeP;
      end;
      tvResistance:
      begin
        edtTestVal.Value := FDefault.DischargeR;
      end;
    end;
  end;
//  FPacketIndex := I;
end;

procedure TfrmMain.tbxMonitorChange(Sender: TObject);
begin
  if FRunMode = rmNone then
  begin
    if tbxMonitor.Checked then
    begin
      SetRunMode(rmMonitor);
    end;
  end else if FRunMode = rmMonitor then
  begin
    if not tbxMonitor.Checked then
    begin
      SetRunMode(rmNone);
    end;
  end;
end;

procedure TfrmMain.tmrWaitTimer(Sender: TObject);
begin
  if FWaitCounter > 0 then
  begin
    Dec(FWaitCounter);
    lblTimer.Caption := IntToStr(FWaitCounter);
    if FWaitCounter = 0 then
    begin
      TimerOff;
    end;
  end;
end;

procedure TfrmMain.tsChargeEnter(Sender: TObject);
begin
  FixLabels(-1);
  if rgCharge.ItemIndex = -1 then
  begin
    btnStart.Enabled := False;
  end;
end;

procedure TfrmMain.tsDischargeEnter(Sender: TObject);
begin
  FixLabels(GetPointer(rgDischarge));
end;

end.

