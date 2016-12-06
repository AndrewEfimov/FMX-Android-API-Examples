program ActiveNetInfo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FormMain},
  ActiveNetworkInfo.Android in '..\class\ActiveNetworkInfo.Android.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
