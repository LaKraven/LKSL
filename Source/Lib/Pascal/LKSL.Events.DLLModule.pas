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
unit LKSL.Events.DLLModule;

interface

{$I LKSL.inc}

{$REGION 'Unit About'}
  {
    About this unit:
      - This unit needs to be explicitly included in any DLL module requiring bi-directional Event Engine communication
  }
{$ENDREGION}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types,
  LKSL.Threads.Main,
  LKSL.Streams.Main,
  LKSL.Events.Main, LKSL.Events.DLLCommon,
  LKSL.Streamables.Main;

  {$I LKSL_RTTI.inc}

implementation

type
  TLKEventDLLModule = class(TLKEventStreamProcessor)
  protected
    procedure ProcessEventStreamable(const AEventStream: TLKEventStreamable; const ADelta, AStartTime: LKFloat); override;
  public
    class function GetTargetFlag: TLKEventTarget; override;
  end;

var
  EventDLLModule: TLKEventDLLModule;
  EventBytesCallback: TLKEventBytesCallback = nil;

procedure LKSLSetEventEngineCallback(const AEventBytesCallback: TLKEventBytesCallback); cdecl;
begin
  EventBytesCallback := AEventBytesCallback;
end;

function LKSLGetEventEngineID: TGUID; cdecl;
begin
  Result := LKSLGetEventEngineInstanceGUID;
end;

{ TLKEventDLLModule }

class function TLKEventDLLModule.GetTargetFlag: TLKEventTarget;
begin
  Result := edRemotes;
end;

procedure TLKEventDLLModule.ProcessEventStreamable(const AEventStream: TLKEventStreamable; const ADelta, AStartTime: LKFloat);
var
  LCaret: ILKStreamCaret;
  LStream: ILKStream;
  LBytes: TBytes;
begin
  if Assigned(EventBytesCallback) then
  begin
    if (AEventStream.BaseEvent.OriginGUID = LKSLGetEventEngineID) then
    begin
      LStream := TLKMemoryStream.Create;
      LCaret := LStream.NewCaret;
      AEventStream.SaveToStream(LCaret);
      LCaret.Position := 0;
      SetLength(LBytes, LStream.Size);
      LCaret.Read(LBytes[0], LStream.Size);
      EventBytesCallback(LBytes, ADelta, AStartTime);
    end;
  end;
end;

exports
  LKSLPushEvent name LKSL_EVENT_PUSH_NAME,
  LKSLSetEventEngineCallback name LKSL_EVENT_SETCALLBACK_NAME,
  LKSLGetEventEngineID name LKSL_EVENT_GET_INSTANCE_ID;

initialization
  EventDLLModule := TLKEventDLLModule.Create;
finalization
  EventDLLModule.Free;

end.
