# Android-API
Android API helper classes

uActiveNetworkInfo:
  TActiveNetworkInfo = class
  public
    class function CheckPermission: Boolean;
    class function GetInfo: JNetworkInfo;
    class function IsConnected: Boolean;
    class function GetTypeName: string;
    class function IsWifi: Boolean;
    class function IsMobile: Boolean;
  end;