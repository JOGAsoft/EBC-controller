unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, JLabeledIntegerEdit;

const
  cAutoLoad = 0;
  cRememberSaveDir = 1;
  cRememberAutoLog = 2;
  cRememberStepDir = 3;
  cForceMon = 4;
  cCaptureSettings = 5;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    Button1: TButton;
    cgSettings: TCheckGroup;
    edtProgFile: TFileNameEdit;
    grpParam: TGroupBox;
    edtIntTime: TJLabeledIntegerEdit;
    rgStart: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure cgSettingsClick(Sender: TObject);
    procedure cgSettingsItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.lfm}

{ TfrmSettings }

procedure TfrmSettings.cgSettingsClick(Sender: TObject);
begin
  edtProgFile.Enabled := cgSettings.Checked[cAutoLoad];
end;

procedure TfrmSettings.Button1Click(Sender: TObject);
begin
  if edtIntTime.Value < 2 then edtIntTime.Value := 2;
end;

procedure TfrmSettings.cgSettingsItemClick(Sender: TObject; Index: integer);
begin
  cgSettingsClick(Sender);
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  cgSettings.CheckEnabled[cCaptureSettings] := False;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  cgSettingsClick(Sender);
end;

end.

