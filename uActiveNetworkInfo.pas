{ *********************************************************************
  *
  * Autor: Efimov A.A.
  * E-mail: infocean@gmail.com
  * GitHub: https://github.com/AndrewEfimov
  * Requirements: You need permission "ACCESS_NETWORK_STATE" in the manifest.
  * Platform: only Android (tested on API16+)
  * IDE: Delphi 10.1 Berlin +
  *
  ******************************************************************** }
unit uActiveNetworkInfo;

interface

uses
  Androidapi.JNI.Net, Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os;

type
  TActiveNetworkInfo = class
  public
    class function CheckPermission: Boolean;
    class function Get: JNetworkInfo;
    class function IsConnected: Boolean;
    class function GetTypeName: string;
    class function IsWifi: Boolean;
    class function IsMobile: Boolean;
  end;

implementation

{ TActiveNetworkInfo }

class function TActiveNetworkInfo.CheckPermission: Boolean;
const
  AccessNetworkStatePermission = 'android.permission.ACCESS_NETWORK_STATE';
var
  PackageName: JString;
begin
  PackageName := TAndroidHelper.Context.getPackageName;
  if (TJBuild_VERSION.JavaClass.SDK_INT < 23) then
    Result := (TAndroidHelper.Context.getPackageManager.CheckPermission(StringToJString(AccessNetworkStatePermission),
      PackageName) = TJPackageManager.JavaClass.PERMISSION_GRANTED)
  else
    Result := (TAndroidHelper.Context.checkSelfPermission(StringToJString(AccessNetworkStatePermission))
      = TJPackageManager.JavaClass.PERMISSION_GRANTED);
end;

class function TActiveNetworkInfo.Get: JNetworkInfo;
var
  ConnectivityManager: JConnectivityManager;
  NetworkInfo: JNetworkInfo;
begin
  Result := nil;
  if CheckPermission then
  begin
    ConnectivityManager := TJConnectivityManager.Wrap
      (TAndroidHelper.Context.getSystemService(TJContext.JavaClass.CONNECTIVITY_SERVICE));
    if (ConnectivityManager <> nil) then
    begin
      NetworkInfo := ConnectivityManager.getActiveNetworkInfo();
      if NetworkInfo <> nil then
        Result := NetworkInfo;
    end;
  end;
end;

class function TActiveNetworkInfo.IsConnected: Boolean;
var
  NetworkInfo: JNetworkInfo;
begin
  NetworkInfo := Get;
  Result := (NetworkInfo <> nil) and NetworkInfo.IsConnected();
end;

class function TActiveNetworkInfo.IsMobile: Boolean;
var
  NetworkInfo: JNetworkInfo;
begin
  NetworkInfo := Get;
  Result := (NetworkInfo <> nil) and (NetworkInfo.getType() = TJConnectivityManager.JavaClass.TYPE_MOBILE);
end;

class function TActiveNetworkInfo.IsWifi: Boolean;
var
  NetworkInfo: JNetworkInfo;
begin
  NetworkInfo := Get;
  Result := (NetworkInfo <> nil) and (NetworkInfo.getType() = TJConnectivityManager.JavaClass.TYPE_WIFI);
end;

class function TActiveNetworkInfo.GetTypeName: string;
var
  NetworkInfo: JNetworkInfo;
begin
  NetworkInfo := Get;
  if NetworkInfo <> nil then
    Result := JStringToString(NetworkInfo.GetTypeName())
  else
    Result := 'NONE'
end;

end.
