unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  FMX.Layouts,
  Androidapi.JNIBridge;

type
  TForm2 = class(TForm)
    editValue: TEdit;
    butVibrate: TButton;
    butHasVirator: TButton;
    butCancel: TButton;
    Layout1: TLayout;
    ToolBar1: TToolBar;
    Label1: TLabel;
    Layout2: TLayout;
    labelHasVibrator: TLabel;
    procedure butHasViratorClick(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
    procedure butVibrateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function CreateJavaLongArray(const ASource: TArray<string>): TJavaArray<Int64>;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Vibrator.Android;

procedure TForm2.butHasViratorClick(Sender: TObject);
begin
  if TVibrator.hasVibrator then
    labelHasVibrator.Text := 'Vibrator supported'
  else
    labelHasVibrator.Text := 'Vibrator is not supported'
end;

procedure TForm2.butVibrateClick(Sender: TObject);
var
  Value: string;
  SplitValue: TArray<string>;
begin
  Value := Trim(editValue.Text);
  if Value.Contains(',') then
  begin
    SplitValue := editValue.Text.Split([',']);
    TVibrator.Vibrate(CreateJavaLongArray(SplitValue), -1);
  end
  else
    TVibrator.Vibrate(Value.ToInt64);
end;

function TForm2.CreateJavaLongArray(const ASource: TArray<string>): TJavaArray<Int64>;
var
  i: Integer;
begin
  Result := TJavaArray<Int64>.Create(Length(ASource));
  for i := Low(ASource) to High(ASource) do
    Result.Items[i] := ASource[i].ToInt64;
end;

procedure TForm2.butCancelClick(Sender: TObject);
begin
  TVibrator.cancel;
end;

end.
