unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  FMX.Layouts;

type
  TForm2 = class(TForm)
    editValue: TEdit;
    butVibrateTestZero: TButton;
    butHasVirator: TButton;
    butCancel: TButton;
    Layout1: TLayout;
    ToolBar1: TToolBar;
    Label1: TLabel;
    Layout2: TLayout;
    labelHasVibrator: TLabel;
    butVibrateTestOne: TButton;
    butVibrateTestTwo: TButton;
    butVibrateTestThree: TButton;
    procedure butHasViratorClick(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
    procedure butVibrateTestZeroClick(Sender: TObject);
    procedure butVibrateTestOneClick(Sender: TObject);
    procedure butVibrateTestTwoClick(Sender: TObject);
    procedure butVibrateTestThreeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function CheckPermission: Boolean;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  System.Permissions, Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, uVibratorHelper;

procedure TForm2.butHasViratorClick(Sender: TObject);
begin
  if TVibratorHelper.hasVibrator then
    labelHasVibrator.Text := 'Vibrator supported'
  else
    labelHasVibrator.Text := 'Vibrator is not supported'
end;

procedure TForm2.butVibrateTestOneClick(Sender: TObject);
const
  ArrayStr: array [0 .. 3] of string = ('200', '1000', '200', '2000');
begin
  // Test 1
  if CheckPermission then
    TVibratorHelper.vibrate(ArrayStr, 2);
end;

procedure TForm2.butVibrateTestThreeClick(Sender: TObject);
begin
  // Test 3: TJavaArray<Int64>
  if CheckPermission then
    TVibratorHelper.vibrate(TJavaArrayHelper.ArrayStrToJavaArrayInt64(['100', '2000', '100', '3000']), -1);
end;

procedure TForm2.butVibrateTestTwoClick(Sender: TObject);
const
  ArrayInt64: array of Int64 = [500, 2000, 500, 2000];
begin
  // Test 2
  if CheckPermission then
    TVibratorHelper.vibrate(ArrayInt64, -1);
end;

procedure TForm2.butVibrateTestZeroClick(Sender: TObject);
var
  Value: string;
begin
  Value := Trim(editValue.Text);
  if CheckPermission then
    if Value.Contains(',') then
      TVibratorHelper.vibrate(Value.Split([',']), -1)
    else
      TVibratorHelper.vibrate(Value.ToInt64);
end;

function TForm2.CheckPermission: Boolean;
begin
  Result := PermissionsService.IsPermissionGranted(JStringToString(TJManifest_permission.JavaClass.vibrate));
end;

procedure TForm2.butCancelClick(Sender: TObject);
begin
  if CheckPermission then
    TVibratorHelper.cancel;
end;

end.
