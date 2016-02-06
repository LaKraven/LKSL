{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014-2016, Simon J Stuart, All Rights Reserved

  Original Source Location: https://github.com/LaKraven/LKSL

  License:
    - You may use this library as you see fit, including use within commercial applications.
    - You may modify this library to suit your needs, without the requirement of distributing
      modified versions.
    - You may redistribute this library (in part or whole) individually, or as part of any
      other works.
    - You must NOT charge a fee for the distribution of this library (compiled or in its
      source form). It MUST be distributed freely.
    - This license and the surrounding comment block MUST remain in place on all copies and
      modified versions of this source code.
    - Modified versions of this source MUST be clearly marked, including the name of the
      person(s) and/or organization(s) responsible for the changes, and a SEPARATE "changelog"
      detailing all additions/deletions/modifications made.

  Disclaimer:
    - Your use of this source constitutes your understanding and acceptance of this
      disclaimer.
    - Simon J Stuart and its employees (including but not limited to directors,
      programmers and clerical staff) cannot be held liable for your use of this source
      code. This includes any losses and/or damages resulting from your use of this source
      code, be they physical, financial, or psychological.
    - There is no warranty or guarantee (implicit or otherwise) provided with this source
      code. It is provided on an "AS-IS" basis.

  Donations:
    - While not mandatory, contributions are always appreciated. They help keep the coffee
      flowing during the long hours invested in this and all other Open Source projects we
      produce.
    - Donations can be made via PayPal to PayPal [at] LaKraven (dot) Com
                                          ^  Garbled to prevent spam!  ^
}
unit LKSL.Events.DLLHost;

interface

{$I LKSL.inc}

{$REGION 'Unit About'}
  {
    About this unit:
      - This unit needs to be explicitly included in any Application/Service intent on consuming one or more Event Engine containing DLL Modules.
  }
{$ENDREGION}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs, {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF MSWINDOWS}
  {$ELSE}
    Classes, SysUtils, SyncObjs, {$IFDEF MSWINDOWS}Windows,{$ENDIF MSWINDOWS}
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types,
  LKSL.Threads.Main,
  LKSL.Streams.Main,
  LKSL.Events.Main, LKSL.Events.DLLCommon,
  LKSL.Generics.Collections,
  LKSL.Streamables.Main;

  {$I LKSL_RTTI.inc}

type
  ELKEventDLLException = class(Exception);
   ELKEventDLLNotLoaded = class(ELKEventDLLException);
   ELKEventDLLNotValid = class(ELKEventDLLException);

procedure LKSLMountEventModule(const APath: String);
procedure LKSLUnMountEventModule(const AName: String);

implementation

type
  TLKEventDLL = class(TLKPersistent)
  private
    FHandle: HMODULE;
    FPath: String;
    FGetInstanceID: function: TGUID; cdecl;
    FPushEvent: procedure(const AEvent: TBytes; const ADelta, AStartTime: LKFloat); cdecl;
    FSetCallback: procedure(const ACallback: TLKEventBytesCallback); cdecl;
  public
    constructor Create(const APath: String); reintroduce;
    destructor Destroy; override;
  end;

  TLKEventDLLList = class(TLKList<TLKEventDLL>);
  TLKEventDLLDictionary = class(TLKDictionary<String, TLKEventDLL>);

  TLKEventDLLHost = class(TLKEventStreamProcessor)
  private
    FEventDLLs: TLKEventDLLList;
    FEventDLLDictionary: TLKEventDLLDictionary;
  protected
    procedure ProcessEventStreamable(const AEventStream: TLKEventStreamable; const ADelta, AStartTime: LKFloat); override;
  public
    class function GetTargetFlag: TLKEventTarget; override;
    constructor Create(const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); override;
    destructor Destroy; override;
  end;

var
  EventDLLHost: TLKEventDLLHost;

procedure LKSLMountEventModule(const APath: String);
begin
  TLKEventDLL.Create(APath);
end;

procedure LKSLUnMountEventModule(const AName: String);
var
  LEventModule: TLKEventDLL;
begin
  if EventDLLHost.FEventDLLDictionary.TryGetValue(Lowercase(AName), LEventModule) then
    LEventModule.Free;
end;

{ TLKEventDLL }

constructor TLKEventDLL.Create(const APath: String);
begin
  inherited Create;
  FPath := APath;
  // TODO -cTLKEventDLL -oSJS: Need to implement solution for POSIX platforms
  {$IF Defined(MSWINDOWS)}
    FHandle := LoadLibrary(PWideChar(APath));
  {.$ELSEIF Defined(POSIX)}
//    FHandle := HMODULE(dlopen(M.AsAnsi(APath, CP_UTF8).ToPointer, pkgLoadingMode));
  {$ELSE}
    {$MESSAGE FATAL 'The unit LKSL.Events.DLLHost.pas cannot be used on this platform!'}
  {$IFEND}

  if FHandle = 0 then
    raise ELKEventDLLNotLoaded.CreateFmt('Unable to load Module "%s"', [APath])
  else
  begin
    {$IF Defined(MSWINDOWS)}
      @FPushEvent := GetProcAddress(FHandle, LKSL_EVENT_PUSH_NAME);
      @FSetCallback := GetProcAddress(FHandle, LKSL_EVENT_SETCALLBACK_NAME);
      @FGetInstanceID := GetProcAddress(FHandle, LKSL_EVENT_GET_INSTANCE_ID);

      if (@FPushEvent = nil) or (@FSetCallback = nil) or (@FGetInstanceID = nil) then
        raise ELKEventDLLNotValid.CreateFmt('Module "%s" is invalid or not compatible.', [APath])
      else
        FSetCallback(LKSLPushEvent);
    {$IFEND}
  end;

  EventDLLHost.FEventDLLs.Add(Self);
  EventDLLHost.FEventDLLDictionary.Add(LowerCase(APath), Self);
end;

destructor TLKEventDLL.Destroy;
var
  LIndex: Integer;
begin
  FSetCallback(nil); // Let the Module know there's no way of passing the Event back
  // TODO -cTLKEventDLL -oSJS: Need to implement solution for POSIX platforms
  {$IF Defined(MSWINDOWS)}
//    FreeLibrary(FHandle);
  {$IFEND}
  LIndex := EventDLLHost.FEventDLLs.IndexOf(Self);
  if LIndex > -1 then
    EventDLLHost.FEventDLLs.Delete(LIndex);

  EventDLLHost.FEventDLLDictionary.Remove(LowerCase(FPath));
  inherited;
end;

{ TLKEventDLLHost }

constructor TLKEventDLLHost.Create(const ARegistrationMode: TLKEventRegistrationMode);
begin
  inherited;
  FEventDLLs := TLKEventDLLList.Create;
  FEventDLLDictionary := TLKEventDLLDictionary.Create;
end;

destructor TLKEventDLLHost.Destroy;
var
  I: Integer;
begin
  for I := FEventDLLs.Count - 1 downto 0 do
    FEventDLLs[I].Free;
  FEventDLLs.Free;
  FEventDLLDictionary.Free;
  inherited;
end;

class function TLKEventDLLHost.GetTargetFlag: TLKEventTarget;
begin
  Result := edRemotes;
end;

procedure TLKEventDLLHost.ProcessEventStreamable(const AEventStream: TLKEventStreamable; const ADelta, AStartTime: LKFloat);
var
  I: Integer;
  LCaret: ILKStreamCaret;
  LStream: ILKStream;
  LBytes: TBytes;
begin
  for I := 0 to FEventDLLs.Count - 1 do
  begin
    if FEventDLLs[I].FGetInstanceID <> AEventStream.BaseEvent.OriginGUID then
    begin
      LStream := TLKMemoryStream.Create;
      LCaret := LStream.NewCaret;
      AEventStream.SaveToStream(LCaret);
      LCaret.Position := 0;
      SetLength(LBytes, LStream.Size);
      LCaret.Read(LBytes[0], LStream.Size);
      FEventDLLs[I].FPushEvent(LBytes, ADelta, AStartTime);
    end;
  end;
end;

initialization
  EventDLLHost := TLKEventDLLHost.Create;
finalization
  EventDLLHost.Free;

end.
