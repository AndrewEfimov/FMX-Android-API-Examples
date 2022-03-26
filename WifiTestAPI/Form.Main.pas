unit Form.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.Layouts, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView,
  uUniversalReceiver,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,// or Androidapi.JNI.net.wifi (https://github.com/AndrewEfimov/Wrappers/blob/master/Androidapi.JNI.net.wifi.pas)
  Androidapi.Helpers,
  Androidapi.JNI,
  Androidapi.JNIBridge,
  System.Permissions;

type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    Layout1: TLayout;
    tiInfo: TTabItem;
    tiScanResult: TTabItem;
    sbGetInfo: TSpeedButton;
    sbScan: TSpeedButton;
    mInfo: TMemo;
    lvScanResults: TListView;
    Layout2: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbGetInfoClick(Sender: TObject);
    procedure sbScanClick(Sender: TObject);
  private const
    PermissionAccessFineLocation = 'android.permission.ACCESS_FINE_LOCATION';
  private
    FBroadcastReceiver: TBroadcastReceiver;
    FWifiManager: JWifiManager;
    procedure OnReceiveScanResults(context: JContext; intent: JIntent);
    procedure LoadWifiInfo(Sender: TObject; const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
    procedure StartScan(Sender: TObject; const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

{ TFormMain }

function DecToIpv4(const AValue: Cardinal): string;
begin
  // LITTLE_ENDIAN
  Result := Format('%d.%d.%d.%d', [AValue and $FF, (AValue shr 8) and $FF, (AValue shr 16) and $FF, AValue shr 24]);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FBroadcastReceiver := TBroadcastReceiver.Create;
  FBroadcastReceiver.OnReceive := OnReceiveScanResults;
  FBroadcastReceiver.IntentFilterAdd([TJWifiManager.JavaClass.SCAN_RESULTS_AVAILABLE_ACTION]);
  FBroadcastReceiver.registerReceiver;

  FWifiManager := TJWifiManager.Wrap(TAndroidHelper.context.getSystemService(TJContext.JavaClass.WIFI_SERVICE));
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FBroadcastReceiver.Free;
end;

procedure TFormMain.LoadWifiInfo(Sender: TObject; const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
var
  WifiInfo: JWifiInfo;
begin
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    WifiInfo := FWifiManager.getConnectionInfo();

    mInfo.Lines.Clear;
    mInfo.Lines.Add('BSSID: ' + TAndroidHelper.JStringToString(WifiInfo.getBSSID));
    mInfo.Lines.Add('HiddenSSID: ' + WifiInfo.getHiddenSSID.ToString);
    mInfo.Lines.Add('IpAddress: ' + DecToIpv4(WifiInfo.getIpAddress));
    mInfo.Lines.Add('LinkSpeed: ' + WifiInfo.getLinkSpeed.ToString + ' Mbps');
    mInfo.Lines.Add('MacAddress: ' + TAndroidHelper.JStringToString(WifiInfo.getMacAddress));
    mInfo.Lines.Add('NetworkId: ' + WifiInfo.getNetworkId.ToString);
    mInfo.Lines.Add('Rssi: ' + WifiInfo.getRssi.ToString + ' dBm');
    mInfo.Lines.Add('SSID: ' + TAndroidHelper.JStringToString(WifiInfo.getSSID));
    mInfo.Lines.Add('SupplicantState: ' + TAndroidHelper.JStringToString(WifiInfo.getSupplicantState.ToString));
  end;
end;

procedure TFormMain.OnReceiveScanResults(context: JContext; intent: JIntent);
var
  ScanResults: JList;
  I: Integer;
  ScanResultsItem: JScanResult;
  LItem: TListViewItem;
begin
  if intent.getBooleanExtra(TJWifiManager.JavaClass.EXTRA_RESULTS_UPDATED, False) then
  begin
    ScanResults := FWifiManager.getScanResults();

    lvScanResults.Items.Clear;
    lvScanResults.BeginUpdate;
    try
      for I := 0 to ScanResults.size - 1 do
      begin
        ScanResultsItem := TJScanResult.Wrap(ScanResults.get(I));

        LItem := lvScanResults.Items.Add;
        LItem.Text := TAndroidHelper.JStringToString(ScanResultsItem.SSID);
      end;
    finally
      lvScanResults.EndUpdate;
    end;
  end;
end;

procedure TFormMain.sbGetInfoClick(Sender: TObject);
begin
  PermissionsService.RequestPermissions([PermissionAccessFineLocation], LoadWifiInfo);
end;

procedure TFormMain.sbScanClick(Sender: TObject);
begin
  PermissionsService.RequestPermissions([PermissionAccessFineLocation], StartScan);
end;

procedure TFormMain.StartScan(Sender: TObject; const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
begin
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
    FWifiManager.StartScan();
end;

end.
