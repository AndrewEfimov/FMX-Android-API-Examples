{ *********************************************************************
  *
  * Autor: Efimov A.A.
  * E-mail: infocean@gmail.com
  * GitHub: https://github.com/AndrewEfimov/
  * Platform: only Android (API 21+)
  * Created: 24.05.2016
  * Updated: 22.03.2022
  *
  ******************************************************************** }

unit uUniversalReceiver;

interface

uses
  Androidapi.JNI.Embarcadero, Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes;

type

  TBroadcastReceiveEvent = procedure(context: JContext; intent: JIntent) of object;

  TBroadcastReceiver = class;

  TListener = class(TJavaLocal, JFMXBroadcastReceiverListener)
  strict private
    FOwner: TBroadcastReceiver;
  public
    constructor Create(AOwner: TBroadcastReceiver);
    procedure OnReceive(context: JContext; intent: JIntent); cdecl;
  end;

  TBroadcastReceiver = class
  strict private
    FListener: TListener;
    FBroadcastReceiver: JFMXBroadcastReceiver;
    FIntentFilter: JIntentFilter;
    FOnReceive: TBroadcastReceiveEvent;
  public
    property OnReceive: TBroadcastReceiveEvent read FOnReceive write FOnReceive;
    constructor Create;
    destructor Destroy; override;
    procedure IntentFilterAdd(const AValue: JString); overload;
    procedure IntentFilterAdd(const AValues: array of JString); overload;
    procedure registerReceiver;
    procedure unregisterReceiver;
  end;

implementation

{ TListener }

constructor TListener.Create(AOwner: TBroadcastReceiver);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TListener.OnReceive(context: JContext; intent: JIntent);
begin
  if Assigned(FOwner.OnReceive) then
    FOwner.OnReceive(context, intent);
end;

{ TBroadcastReceiver }

constructor TBroadcastReceiver.Create;
begin
  inherited Create;

  FListener := TListener.Create(Self);
  FBroadcastReceiver := TJFMXBroadcastReceiver.JavaClass.init(FListener);

  FIntentFilter := TJIntentFilter.Create;
end;

destructor TBroadcastReceiver.Destroy;
begin
  unregisterReceiver;

  inherited Destroy;
end;

procedure TBroadcastReceiver.IntentFilterAdd(const AValues: array of JString);
var
  I: Integer;
begin
  for I := 0 to Length(AValues) - 1 do
    IntentFilterAdd(AValues[I]);
end;

procedure TBroadcastReceiver.IntentFilterAdd(const AValue: JString);
begin
  if not AValue.isEmpty then
    FIntentFilter.addAction(AValue);
end;

procedure TBroadcastReceiver.registerReceiver;
begin
  if FIntentFilter.countActions > 0 then
    TAndroidHelper.Context.registerReceiver(FBroadcastReceiver, FIntentFilter);
end;

procedure TBroadcastReceiver.unregisterReceiver;
begin
  if FBroadcastReceiver <> nil then
    TAndroidHelper.Context.unregisterReceiver(FBroadcastReceiver);

  FBroadcastReceiver := nil;
  FListener.Free;
  FIntentFilter := nil;
end;

end.
