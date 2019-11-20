{ *********************************************************************
  *
  * Autor: Efimov A.A.
  * E-mail: infocean@gmail.com
  * GitHub: https://github.com/AndrewEfimov
  * Permissions: "android.permission.VIBRATE"
  * Platform (API 19+): Android 4.4.2 - 10
  * IDE: Delphi 10.1/10.2/10.3 (Berlin/Tokyo/Rio)
  *
  ******************************************************************** }
unit uVibratorHelper;

interface

uses
  {$IF CompilerVersion >= 33.0}
  System.Permissions,
  {$ENDIF}
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge,
  System.SysUtils, Androidapi.JNI.Os.Vibration;

type
  TJavaArrayHelper = class
  public
    class function ArrayStrToJavaArrayInt64(const ASource: array of string): TJavaArray<Int64>;
    class function ArrayInt64ToJavaArrayInt64(const ASource: array of Int64): TJavaArray<Int64>;
  end;

  TVibratorHelper = class(TObject)
  private
    class var SDK_INT: Integer;
    class var FJVibrator: JVibrator;
    class constructor Create;
    {$IF CompilerVersion < 33.0}
    /// <summary> For Delphi Berlin and Tokyo </summary>
    class function OldCheckPermission: Boolean;
    {$ENDIF}
  public
    /// <summary> Turn the vibrator off. </summary>
    class procedure cancel;
    /// <summary> Check permission "android.permission.VIBRATE" </summary>
    class function CheckPermission: Boolean;
    /// <summary> Check whether the hardware has a vibrator. </summary>
    class function hasVibrator: Boolean;
	/// <summary> Vibrate constantly for the specified period of time. </summary>
    class procedure vibrate(const AMilliseconds: Int64); overload;
    /// <summary> Vibrate with a given pattern. </summary>
    class procedure vibrate(const APattern: TJavaArray<Int64>; const ARepeat: Integer); overload;
    class procedure vibrate(const APattern: array of Int64; const ARepeat: Integer); overload;
    class procedure vibrate(const APattern: array of string; const ARepeat: Integer); overload;
  end;

implementation

{ TJavaArrayHelper }

class function TJavaArrayHelper.ArrayInt64ToJavaArrayInt64(const ASource: array of Int64): TJavaArray<Int64>;
var
  I: Integer;
begin
  Result := TJavaArray<Int64>.Create(Length(ASource));
  for I := Low(ASource) to High(ASource) do
    Result.Items[I] := ASource[I];
end;

class function TJavaArrayHelper.ArrayStrToJavaArrayInt64(const ASource: array of string): TJavaArray<Int64>;
var
  I: Integer;
begin
  Result := TJavaArray<Int64>.Create(Length(ASource));
  for I := Low(ASource) to High(ASource) do
    Result.Items[I] := ASource[I].ToInt64;
end;

{ TVibrator }

class procedure TVibratorHelper.cancel;
begin
  if hasVibrator then
    FJVibrator.cancel;
end;

class function TVibratorHelper.CheckPermission: Boolean;
begin
{$IF CompilerVersion >= 33.0}
  Result := PermissionsService.IsPermissionGranted(JStringToString(TJManifest_permission.JavaClass.vibrate));
{$ELSE}
  Result := OldCheckPermission;
{$ENDIF}
end;

class constructor TVibratorHelper.Create;
begin
  SDK_INT := TJBuild_VERSION.JavaClass.SDK_INT;
  FJVibrator := nil;

  if CheckPermission then
    FJVibrator := TJVibrator.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.VIBRATOR_SERVICE))
end;

class function TVibratorHelper.hasVibrator: Boolean;
begin
  Result := (FJVibrator <> nil) and FJVibrator.hasVibrator;
end;

{$IF CompilerVersion < 33.0}
class function TVibrator.OldCheckPermission: Boolean;
const
  VibratePermission = 'android.permission.VIBRATE';
var
  PackageName, Permission: JString;
  Context: JContext;
  PermissionGranted: Integer;
begin
  PackageName := TAndroidHelper.Context.getPackageName;
  Context := TAndroidHelper.Context;
  Permission := StringToJString(VibratePermission);
  PermissionGranted := TJPackageManager.JavaClass.PERMISSION_GRANTED;

  if SDK_INT < 23 then
    Result := Context.getPackageManager.CheckPermission(Permission, PackageName) = PermissionGranted
  else
    Result := Context.checkSelfPermission(Permission) = PermissionGranted;
end;
{$ENDIF}

class procedure TVibratorHelper.vibrate(const AMilliseconds: Int64);
begin
  if hasVibrator then
    if SDK_INT >= 26 then
      FJVibrator.vibrate(TJVibrationEffect.JavaClass.createOneShot(AMilliseconds,
        TJVibrationEffect.JavaClass.DEFAULT_AMPLITUDE))
    else
      FJVibrator.vibrate(AMilliseconds);
end;

class procedure TVibratorHelper.vibrate(const APattern: array of string; const ARepeat: Integer);
begin
  TVibratorHelper.vibrate(TJavaArrayHelper.ArrayStrToJavaArrayInt64(APattern), ARepeat);
end;

class procedure TVibratorHelper.vibrate(const APattern: array of Int64; const ARepeat: Integer);
begin
  TVibratorHelper.vibrate(TJavaArrayHelper.ArrayInt64ToJavaArrayInt64(APattern), ARepeat);
end;

// pattern: an array of longs of times for which to turn the vibrator on or off.
// repeat: the index into pattern at which to repeat, or -1 if you don't want to repeat.
// array pattern(pause and vibrate in milliseconds): [pause, vibrate, pause, ... , vibrate, pause]
class procedure TVibratorHelper.vibrate(const APattern: TJavaArray<Int64>; const ARepeat: Integer);
begin
  if hasVibrator then
    if SDK_INT >= 26 then
      FJVibrator.vibrate(TJVibrationEffect.JavaClass.createWaveform(APattern, ARepeat))
    else
      FJVibrator.vibrate(APattern, ARepeat);
end;

end.
