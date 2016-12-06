{ *********************************************************************
  *
  * Autor: Efimov A.A.
  * E-mail: infocean@gmail.com
  * GitHub: https://github.com/AndrewEfimov
  * Description: Class that operates the vibrator on the device. If your process exits, any vibration you started will stop.
  * Requirements: You need permission "VIBRATE" in the manifest.
  * Platform: only Android (tested on API16+)
  * IDE: Delphi 10.1 Berlin +
  *
  ******************************************************************** }
unit Vibrator.Android;

interface

uses
  Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
  Androidapi.JNIBridge;

type
  TVibrator = class
  private
    class var FJVibrator: JVibrator;
    class constructor Create;
  public
    /// <summary>Turn the vibrator off.</summary>
    class procedure Cancel;
    /// <summary>Check permission "android.permission.VIBRATE"</summary>
    class function CheckPermission: Boolean;
    /// <summary>Check whether the hardware has a vibrator.</summary>
    class function HasVibrator: Boolean;
    /// <summary>Vibrate constantly for the specified period of time.</summary>
    class procedure Vibrate(Milliseconds: Int64); overload;
    /// <summary>Vibrate with a given pattern.</summary>
    class procedure Vibrate(Pattern: TJavaArray<Int64>; Repeat_: Integer); overload;
  end;

implementation

{ TVibrator }

class procedure TVibrator.Cancel;
begin
  if HasVibrator then
    FJVibrator.Cancel;
end;

class function TVibrator.CheckPermission: Boolean;
const
  AccessNetworkStatePermission = 'android.permission.VIBRATE';
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

class constructor TVibrator.Create;
begin
  FJVibrator := nil;
  if CheckPermission then
    FJVibrator := TJVibrator.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.VIBRATOR_SERVICE));
end;

class function TVibrator.HasVibrator: Boolean;
begin
  Result := (FJVibrator <> nil) and FJVibrator.HasVibrator;
end;

class procedure TVibrator.Vibrate(Milliseconds: Int64);
begin
  if HasVibrator then
    FJVibrator.Vibrate(Milliseconds);
end;

// pattern: an array of longs of times for which to turn the vibrator on or off.
// repeat: the index into pattern at which to repeat, or -1 if you don't want to repeat.
// array pattern(pause and vibrate in milliseconds): [pause, vibrate, pause, ... , vibrate, pause]
class procedure TVibrator.Vibrate(Pattern: TJavaArray<Int64>; Repeat_: Integer);
begin
  if HasVibrator then
    FJVibrator.Vibrate(Pattern, Repeat_);
end;

end.
