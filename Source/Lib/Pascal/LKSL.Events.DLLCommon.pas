{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014-2015, LaKraven Studios Ltd, All Rights Reserved

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
    - LaKraven Studios Ltd and its employees (including but not limited to directors,
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
unit LKSL.Events.DLLCommon;

interface

{$I LKSL.inc}

{$REGION 'Unit About'}
  {
    About this unit:
      - This unit is used internally for the Event Engine DLL Connector.
      - DO NOT REFERENCE THIS UNIT IN YOUR OWN UNITS!
  }
{$ENDREGION}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types,
  LKSL.Streams.Main,
  LKSL.Streamables.Main,
  LKSL.Events.Main;

  {$I LKSL_RTTI.inc}

type
  TLKEventBytesCallback = procedure(const AEvent: TBytes; const ADelta, AStartTime: LKFloat);

const
  LKSL_EVENT_PUSH_NAME = 'LKSL_EventPush';
  LKSL_EVENT_SETCALLBACK_NAME = 'LKSL_EventSetCallback';

procedure LKSLPushEvent(const AEvent: TBytes; const ADelta, AStartTime: LKFloat);

implementation

procedure LKSLPushEvent(const AEvent: TBytes; const ADelta, AStartTime: LKFloat);
var
  LCaret: ILKStreamCaret;
  LStream: ILKStream;
  LStreamable: TLKStreamable;
begin
  LStream := TLKMemoryStream.Create;
  LCaret.Write(AEvent[0], Length(AEvent));
  LStreamable := Streamables.CreateStreamableFromStream(LCaret);
  if LStreamable is TLKEventStreamable then
    case TLKEventStreamable(LStreamable).BaseEvent.DispatchMethod of
      edmQueue: TLKEventStreamable(LStreamable).BaseEvent.Queue;
      edmStack: TLKEventStreamable(LStreamable).BaseEvent.Stack;
      else
        LStreamable.Free;
    end;
end;

end.
