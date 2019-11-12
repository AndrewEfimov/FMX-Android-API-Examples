unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ListBox;

type
  TFormMain = class(TForm)
    lbListInfo: TListBox;
    ToolBar1: TToolBar;
    Label1: TLabel;
    sbLoadInfo: TSpeedButton;
    procedure sbLoadInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FPermissionReadPhoneState: string;
    procedure AddListItem(const AName, AValue: string);
    procedure AddListHeader(const AName: string);
    procedure LoadInfo;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Permissions, Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.Provider,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, Androidapi.JNI, Androidapi.JNI.Bluetooth;

procedure TFormMain.AddListHeader(const AName: string);
var
  ListBoxGroupHeader: TListBoxGroupHeader;
begin
  ListBoxGroupHeader := TListBoxGroupHeader.Create(lbListInfo);
  ListBoxGroupHeader.Text := AName;
  lbListInfo.AddObject(ListBoxGroupHeader);
end;

procedure TFormMain.AddListItem(const AName, AValue: string);
var
  ListBoxItem: TListBoxItem;
begin
  ListBoxItem := TListBoxItem.Create(lbListInfo);
  ListBoxItem.Text := AName;
  ListBoxItem.ItemData.Detail := AValue;
  lbListInfo.AddObject(ListBoxItem);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FPermissionReadPhoneState := JStringToString(TJManifest_permission.JavaClass.READ_PHONE_STATE);
end;

procedure TFormMain.LoadInfo;
var
  SDK_INT, I, J, K: Integer;
  DeviceName: JString;
  BTAdapter: JBluetoothAdapter;
  SUPPORTED_32_BIT_ABIS, SUPPORTED_64_BIT_ABIS, SUPPORTED_ABIS: TJavaObjectArray<JString>;
  StrSUPPORTED_32, StrSUPPORTED_64, StrSUPPORTED_ABIS: string;
begin
  SDK_INT := TJBuild_VERSION.JavaClass.SDK_INT;

  if SDK_INT < 17 then
    DeviceName := TJSettings_System.JavaClass.getString(TAndroidHelper.ContentResolver, StringToJstring('device_name'))
  else
    DeviceName := TJSettings_Global.JavaClass.getString(TAndroidHelper.ContentResolver, StringToJstring('device_name'));

  if DeviceName = nil then
  begin
    // android.Manifest.permission.BLUETOOTH
    BTAdapter := TJBluetoothAdapter.JavaClass.getDefaultAdapter;
    DeviceName := BTAdapter.getName;
  end;

  lbListInfo.Clear;
  lbListInfo.BeginUpdate;

  AddListHeader('Settings API');
  AddListItem('DEVICE_NAME', JStringToString(DeviceName));

  AddListHeader('Build API');
  AddListItem('BOARD', JStringToString(TJBuild.JavaClass.BOARD)); // API 1
  AddListItem('BOOTLOADER', JStringToString(TJBuild.JavaClass.BOOTLOADER)); // API 8
  AddListItem('BRAND', JStringToString(TJBuild.JavaClass.BRAND)); // API 1

  if SDK_INT <= 21 then
  begin
    AddListItem('CPU_ABI', JStringToString(TJBuild.JavaClass.CPU_ABI)); // API 4 - Depr. API 21 (Use SUPPORTED_ABIS)
    AddListItem('CPU_ABI2', JStringToString(TJBuild.JavaClass.CPU_ABI2)); // API 8 - Depr. API 21 (Use SUPPORTED_ABIS)
  end;

  AddListItem('DEVICE', JStringToString(TJBuild.JavaClass.DEVICE)); // API 1
  AddListItem('DISPLAY', JStringToString(TJBuild.JavaClass.DISPLAY)); // API 3
  AddListItem('FINGERPRINT', JStringToString(TJBuild.JavaClass.FINGERPRINT)); // API 1
  AddListItem('HARDWARE', JStringToString(TJBuild.JavaClass.HARDWARE)); // API 8
  AddListItem('ID', JStringToString(TJBuild.JavaClass.ID)); // API 1
  AddListItem('MANUFACTURER', JStringToString(TJBuild.JavaClass.MANUFACTURER)); // API 4
  AddListItem('MODEL', JStringToString(TJBuild.JavaClass.MODEL)); // API 1
  AddListItem('PRODUCT', JStringToString(TJBuild.JavaClass.PRODUCT)); // API 1

  if SDK_INT <= 15 then
    AddListItem('RADIO', JStringToString(TJBuild.JavaClass.RADIO)) // API 8 - Deprecated API 15 (Use getRadioVersion())
  else
    AddListItem('RADIO', JStringToString(TJBuild.JavaClass.getRadioVersion));

  if SDK_INT <= 26 then
    AddListItem('SERIAL', JStringToString(TJBuild.JavaClass.SERIAL)) // API 9 - Deprecated API 26 (Use getSerial())
  else
    // android.Manifest.permission.READ_PRIVILEGED_PHONE_STATE
    AddListItem('SERIAL', JStringToString(TJBuild.JavaClass.getSerial));


  if SDK_INT >= 21 then
  begin
    SUPPORTED_32_BIT_ABIS := TJBuild.JavaClass.SUPPORTED_32_BIT_ABIS; // API 21
    SUPPORTED_64_BIT_ABIS := TJBuild.JavaClass.SUPPORTED_64_BIT_ABIS; // API 21
    SUPPORTED_ABIS := TJBuild.JavaClass.SUPPORTED_ABIS; // API 21

    StrSUPPORTED_32 := '';
    for I := 0 to SUPPORTED_32_BIT_ABIS.Length - 1 do
      StrSUPPORTED_32 := StrSUPPORTED_32 + JStringToString(SUPPORTED_32_BIT_ABIS.Items[I]) + ' ';

    StrSUPPORTED_64 := '';
    for J := 0 to SUPPORTED_64_BIT_ABIS.Length - 1 do
      StrSUPPORTED_64 := StrSUPPORTED_64 + JStringToString(SUPPORTED_64_BIT_ABIS.Items[J]) + ' ';

    StrSUPPORTED_ABIS := '';
    for K := 0 to SUPPORTED_ABIS.Length - 1 do
      StrSUPPORTED_ABIS := StrSUPPORTED_ABIS + JStringToString(SUPPORTED_ABIS.Items[K]) + ' ';

    AddListItem('SUPPORTED_32_BIT_ABIS', StrSUPPORTED_32);
    AddListItem('SUPPORTED_64_BIT_ABIS', StrSUPPORTED_64);
    AddListItem('SUPPORTED_ABIS', StrSUPPORTED_ABIS);
  end;

  AddListItem('TAGS', JStringToString(TJBuild.JavaClass.TAGS)); // API 1
  AddListItem('TIME', TJBuild.JavaClass.TIME.ToString); // API 1
  AddListItem('TYPE', JStringToString(TJBuild.JavaClass.&TYPE)); // API 1
  AddListItem('USER', JStringToString(TJBuild.JavaClass.USER)); // API 1

  AddListHeader('Build.VERSION API');

  if SDK_INT >= 23 then
    AddListItem('BASE_OS', JStringToString(TJBuild_VERSION.JavaClass.BASE_OS)); // API 23

  AddListItem('CODENAME', JStringToString(TJBuild_VERSION.JavaClass.CODENAME)); // API 4
  AddListItem('INCREMENTAL', JStringToString(TJBuild_VERSION.JavaClass.INCREMENTAL)); // API 1

  if SDK_INT >= 23 then
    AddListItem('PREVIEW_SDK_INT', TJBuild_VERSION.JavaClass.PREVIEW_SDK_INT.ToString); // API 23

  AddListItem('RELEASE', JStringToString(TJBuild_VERSION.JavaClass.RELEASE)); // API 1

  if SDK_INT <= 15 then
    AddListItem('SDK', JStringToString(TJBuild_VERSION.JavaClass.SDK)); // API 1 - Deprecated API 15 (Use SDK_INT)

  AddListItem('SDK_INT', TJBuild_VERSION.JavaClass.SDK_INT.ToString); // API 4

  if SDK_INT >= 23 then
    AddListItem('SECURITY_PATCH', JStringToString(TJBuild_VERSION.JavaClass.SECURITY_PATCH)); // API 23

  lbListInfo.EndUpdate;
end;

procedure TFormMain.sbLoadInfoClick(Sender: TObject);

begin
  PermissionsService.RequestPermissions([FPermissionReadPhoneState],
    procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
        LoadInfo
      else
        ShowMessage('Permission Denied');
    end);
end;

end.
