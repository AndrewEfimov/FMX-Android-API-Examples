{ *********************************************************************
  *
  * Autor: Efimov A.A.
  * E-mail: infocean@gmail.com
  * GitHub: https://github.com/AndrewEfimov
  * Platform: Android (package android.os classes Vibrator and VibrationEffect)
  * IDE: Delphi 10.1 Berlin +
  * Change: 18.11.2019 All API (Include API 29)
  *
  ******************************************************************** }

unit Androidapi.JNI.os.Vibration;

interface

uses
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.os,
  Androidapi.JNI.Media;

type
  // ===== Forward declarations =====

  JVibrationEffect = interface; // API 26 - android.os.VibrationEffect
  JVibrator = interface; // API  1 - android.os.Vibrator


  // ===== Interface declarations =====

  JVibrationEffectClass = interface(JObjectClass)
    ['{4110B371-A3E8-44F5-8D38-0BFA6A70337E}']
    function _GetDEFAULT_AMPLITUDE: Integer; cdecl;
    function _GetEFFECT_CLICK: Integer; cdecl;
    function _GetEFFECT_DOUBLE_CLICK: Integer; cdecl;
    function _GetEFFECT_HEAVY_CLICK: Integer; cdecl;
    function _GetEFFECT_TICK: Integer; cdecl;
    function _GetCREATOR: JParcelable_Creator; cdecl;
    function createOneShot(milliseconds: Int64; amplitude: Integer): JVibrationEffect; cdecl; // API 26
    function createPredefined(effectId: Integer): JVibrationEffect; cdecl; // API 29
    function createWaveform(timings: TJavaArray<Int64>; amplitudes: TJavaArray<Integer>; repeat_: Integer)
      : JVibrationEffect; cdecl; overload; // API 26
    function createWaveform(timings: TJavaArray<Int64>; repeat_: Integer): JVibrationEffect; cdecl; overload; // API 26
    property DEFAULT_AMPLITUDE: Integer read _GetDEFAULT_AMPLITUDE; // API 26
    property EFFECT_CLICK: Integer read _GetEFFECT_CLICK; // API 29
    property EFFECT_DOUBLE_CLICK: Integer read _GetEFFECT_DOUBLE_CLICK; // API 29
    property EFFECT_HEAVY_CLICK: Integer read _GetEFFECT_HEAVY_CLICK; // API 29
    property EFFECT_TICK: Integer read _GetEFFECT_TICK; // API 29
    property CREATOR: JParcelable_Creator read _GetCREATOR; // API 26
  end;

  [JavaSignature('android/os/VibrationEffect')]
  JVibrationEffect = interface(JObject)
    ['{8DD0B73D-5B00-4C34-ABE0-1AE371FDA048}']
    function describeContents: Integer; cdecl; // API 26
  end;

  TJVibrationEffect = class(TJavaGenericImport<JVibrationEffectClass, JVibrationEffect>)
  end;

  JVibratorClass = interface(JObjectClass)
    ['{D7C021F4-5A57-4831-8258-B97C4E63B61B}']
  end;

  [JavaSignature('android/os/Vibrator')]
  JVibrator = interface(JObject)
    ['{51BBB5B3-ED3F-46C2-89D2-B14FBF46A2F7}']
    procedure cancel; cdecl; // API 1
    function hasAmplitudeControl: Boolean; cdecl; // API 26
    function hasVibrator: Boolean; cdecl; // API 11
    procedure vibrate(milliseconds: Int64); cdecl; overload; // (API 1) Deprecated API 26
    procedure vibrate(vibe: JVibrationEffect); cdecl; overload; // API 26
    procedure vibrate(pattern: TJavaArray<Int64>; repeat_: Integer); cdecl; overload; // (API 1) Deprecated API 26
    procedure vibrate(pattern: TJavaArray<Int64>; repeat_: Integer; attributes: JAudioAttributes); cdecl; overload;
    // (API 21) Deprecated API 26
    procedure vibrate(vibe: JVibrationEffect; attributes: JAudioAttributes); cdecl; overload; // API 26
    procedure vibrate(milliseconds: Int64; attributes: JAudioAttributes); cdecl; overload; // (API 21) Deprecated API 26
  end;

  TJVibrator = class(TJavaGenericImport<JVibratorClass, JVibrator>)
  end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.os.Vibration.JVibrationEffect',
    TypeInfo(Androidapi.JNI.os.Vibration.JVibrationEffect));
  TRegTypes.RegisterType('Androidapi.JNI.os.Vibration.JVibrator', TypeInfo(Androidapi.JNI.os.Vibration.JVibrator));
end;

initialization

RegisterTypes;

end.
