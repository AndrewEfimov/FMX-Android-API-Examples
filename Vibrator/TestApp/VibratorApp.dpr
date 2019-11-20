program VibratorApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form2},
  uVibratorHelper in '..\class\uVibratorHelper.pas',
  Androidapi.JNI.os.Vibration in '..\class\Androidapi.JNI.os.Vibration.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;

end.
