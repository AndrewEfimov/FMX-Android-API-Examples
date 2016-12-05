unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;

type
  TFormMain = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  ActiveNetworkInfo.Android;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('CheckPermission: ' + BoolToStr(TActiveNetworkInfo.CheckPermission, True));
  Memo1.Lines.Add('IsConnected: ' + BoolToStr(TActiveNetworkInfo.IsConnected, True));
  Memo1.Lines.Add('GetTypeName: ' + TActiveNetworkInfo.GetTypeName);
  Memo1.Lines.Add('IsWifi: ' + BoolToStr(TActiveNetworkInfo.IsWifi, True));
  Memo1.Lines.Add('IsMobile: ' + BoolToStr(TActiveNetworkInfo.IsMobile, True));
end;

end.
