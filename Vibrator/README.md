# FMX: Android API Examples

### unit Vibrator.Android:

 - **class procedure Cancel;** - Turn the vibrator off.
 - **class function CheckPermission: Boolean;** - Check permission "android.permission.VIBRATE"
 - **class function HasVibrator: Boolean;** - Check whether the hardware has a vibrator.
 - **class procedure Vibrate(Milliseconds: Int64);** - Vibrate constantly for the specified period of time.
 - **class procedure Vibrate(Pattern: TJavaArray<Int64>; Repeat_: Integer);** - Vibrate with a given pattern.