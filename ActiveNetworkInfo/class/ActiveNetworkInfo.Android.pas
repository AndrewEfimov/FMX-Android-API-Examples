{ *********************************************************************
  *
  * Autor: Efimov A.A.
  * E-mail: infocean@gmail.com
  * GitHub: https://github.com/AndrewEfimov
  * Description: Getting information about connecting.
  * Requirements: You need permission "ACCESS_NETWORK_STATE" in the manifest.
  * Platform: only Android (tested on API16+)
  * IDE: Delphi 10.1 Berlin +
  *
  ******************************************************************** }
unit ActiveNetworkInfo.Android;

interface

uses
  Androidapi.JNI.Net, Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os;

type
  TActiveNetworkInfo = class
  private
    class var FJConnectivityManager: JConnectivityManager;
    class constructor Create;
  public
    /// <summary>Check permission "android.permission.ACCESS_NETWORK_STATE"</summary>
    class function CheckPermission: Boolean;
    /// <summary>Returns details about the currently active default data network.</summary>
    class function GetInfo: JNetworkInfo;
    /// <summary>Indicates whether network connectivity exists and it is possible to establish connections and pass data.</summary>
    class function IsConnected: Boolean;
    /// <summary>Return a human-readable name describe the type of the network, for example "WIFI" or "MOBILE".</summary>
    class function GetTypeName: string;
    /// <summary>Is Wi-Fi connection?</summary>
    class function IsWifi: Boolean;
    /// <summary>Is Mobile connection?</summary>
    class function IsMobile: Boolean;
  end;

implementation

{ TActiveNetworkInfo }

class function TActiveNetworkInfo.CheckPermission: Boolean;
const
  AccessNetworkStatePermission = 'android.permission.ACCESS_NETWORK_STATE';
var
  PackageName, Permission: JString;
  Context: JContext;
  PermissionGranted: Integer;
begin
  PackageName := TAndroidHelper.Context.getPackageName;
  Context := TAndroidHelper.Context;
  Permission := StringToJString(AccessNetworkStatePermission);
  PermissionGranted := TJPackageManager.JavaClass.PERMISSION_GRANTED;

  if TJBuild_VERSION.JavaClass.SDK_INT < 23 then
    Result := Context.getPackageManager.CheckPermission(Permission, PackageName) = PermissionGranted
  else
    Result := Context.checkSelfPermission(Permission) = PermissionGranted;
end;

class constructor TActiveNetworkInfo.Create;
begin
  FJConnectivityManager := nil;
  if CheckPermission then
    FJConnectivityManager := TJConnectivityManager.Wrap
      (TAndroidHelper.Context.getSystemService(TJContext.JavaClass.CONNECTIVITY_SERVICE));
end;

class function TActiveNetworkInfo.GetInfo: JNetworkInfo;
var
  NetworkInfo: JNetworkInfo;
begin
  Result := nil;
  if FJConnectivityManager <> nil then
  begin
    NetworkInfo := FJConnectivityManager.getActiveNetworkInfo();
    if NetworkInfo <> nil then
      Result := NetworkInfo;
  end;
end;

class function TActiveNetworkInfo.IsConnected: Boolean;
var
  NetworkInfo: JNetworkInfo;
begin
  NetworkInfo := GetInfo;
  Result := (NetworkInfo <> nil) and NetworkInfo.IsConnected();
end;

class function TActiveNetworkInfo.IsMobile: Boolean;
var
  NetworkInfo: JNetworkInfo;
begin
  NetworkInfo := GetInfo;
  Result := (NetworkInfo <> nil) and (NetworkInfo.getType() = TJConnectivityManager.JavaClass.TYPE_MOBILE);
end;

class function TActiveNetworkInfo.IsWifi: Boolean;
var
  NetworkInfo: JNetworkInfo;
begin
  NetworkInfo := GetInfo;
  Result := (NetworkInfo <> nil) and (NetworkInfo.getType() = TJConnectivityManager.JavaClass.TYPE_WIFI);
end;

class function TActiveNetworkInfo.GetTypeName: string;
const
  ResultNone = 'NONE';
var
  NetworkInfo: JNetworkInfo;
begin
  NetworkInfo := GetInfo;
  if NetworkInfo <> nil then
    Result := JStringToString(NetworkInfo.GetTypeName())
  else
    Result := ResultNone
end;

end.
