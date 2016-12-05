# Android-API

### unit ActiveNetworkInfo.Android:

 - **class function CheckPermission: Boolean;** - Check permission "android.permission.ACCESS_NETWORK_STATE"
 - **class function GetInfo: JNetworkInfo;** - Returns details about the currently active default data network.
 - **class function IsConnected: Boolean;** - Indicates whether network connectivity exists and it is possible to establish connections and pass data.
 - **class function GetTypeName: string;** - Return a human-readable name describe the type of the network, for example "WIFI" or "MOBILE".
 - **class function IsWifi: Boolean;** - Is Wi-Fi connection?
 - **class function IsMobile: Boolean;** - Is Mobile connection?