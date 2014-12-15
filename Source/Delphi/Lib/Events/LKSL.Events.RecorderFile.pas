{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014, LaKraven Studios Ltd, All Rights Reserved

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
unit LKSL.Events.RecorderFile;

interface

{$I LKSL.inc}

{$MESSAGE WARN 'This unit is not yet ready for real-world use!'}

{$REGION 'Unit About'}
  {
    About this unit:
      - This unit provides a File-outputting Event Recorder.
      - You can Stream Events to the nominated File, and play them back from the File.

    THIS UNIT IS VERY-MUCH "WORK IN PROGRESS" AT THIS STAGE!
    The interfaces may well change, right down to the name of the unit itself... so please only use
    this unit in your projects if you understand that you will likely have to make some modifications
    to your implementation(s) in the very near future!
  }
{$ENDREGION}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  Generics.Collections, LKSL.Generics.Collections,
  LKSL.Common.Types,
  LKSL.Threads.Base,
  LKSL.Streamables.Base,
  LKSL.Events.Base;

  {$I LKSL_RTTI.inc}

type
  { Forward Declarations }
  TLKEventRecorderFile = class;

  {
    TLKEventRecorderFile
      - A TLKEventRecorder which stores all recorded Events in a nominated File.
      - Provides the means to replay the Events contained within the nominated File.
  }
  TLKEventRecorderFile = class(TLKEventRecorder)
  private
    FCreated: TDateTime;
    FFileName: String;
    FFileStream: TFileStream;
    FHeaderEndPosition: Int64;
    FSessionCount: Integer;
    FSessionCountPosition: Int64;
    FSessionBlockSizePosition: Int64;
    FSessionEventCountPosition: Int64;
    // Getters
    function GetSessionCount: Integer;
    // General
    procedure LoadFile(const AFileName: String);
    procedure NewFile(const AFileName: String);
  protected
    procedure RecordEvent(const AEvent: TLKEvent); override;
  public
    constructor Create(const AFileName: String); reintroduce;
    destructor Destroy; override;

    procedure NewSession;
    procedure Replay(const ASession: Integer);

    property Created: TDateTime read FCreated;
    property FileName: String read FFileName;
    property SessionCount: Integer read FSessionCount;
  end;

implementation

uses
  LKSL.Streams.System;

  {$I LKSL_RTTI.inc}

const
  RECORDER_FILE_GUID: TGUID = '{F617BBA0-0BB4-4A08-A937-F184DBC9EB62}';

{ TLKEventRecorderFile }

constructor TLKEventRecorderFile.Create(const AFileName: String);
begin
  inherited Create;
  FFileName := AFileName;
  if FileExists(AFileName) then
    LoadFile(AFileName)
  else
    NewFile(AFileName);
end;

destructor TLKEventRecorderFile.Destroy;
begin
  FFileStream.Free;
  inherited;
end;

function TLKEventRecorderFile.GetSessionCount: Integer;
var
  LPosition: Int64;
begin
  LPosition := FFileStream.Position; // Store a reference to the original Position
  Result := StreamReadInteger(FFileStream, FSessionCountPosition);
  FFileStream.Position := LPosition; // Restore the Position
end;

procedure TLKEventRecorderFile.LoadFile(const AFileName: String);
var
  LSignature: TGUID;
begin
  FFileStream := TFileStream.Create(AFileName, fmOpenReadWrite, fmExclusive);
  LSignature := StreamReadGUID(FFileStream, 0); // Read Signature
  if LSignature = RECORDER_FILE_GUID then
  begin
    FCreated := StreamReadDateTime(FFileStream); // Read Created Timestamp
    FSessionCountPosition := FFileStream.Position; // Store a reference to the Session Count Position
    StreamReadInteger(FFileStream); // Read the Session Count (just to advance the Stream)
    FHeaderEndPosition := FFileStream.Position;
  end else
    raise ELEventRecorderSignatureMismatchException.CreateFmt('Signature Mismatch! Expected "%s", got "%s"', [GUIDToString(RECORDER_FILE_GUID), GUIDToString(LSignature)]);
end;

procedure TLKEventRecorderFile.NewFile(const AFileName: String);
begin
  FFileStream := TFileStream.Create(AFileName, fmCreate);
  StreamWriteGUID(FFileStream, RECORDER_FILE_GUID); // Write Signature
  StreamWriteDateTime(FFileStream, Now); // Write Created Timestamp
  FSessionCountPosition := FFileStream.Position;
  StreamWriteInteger(FFileStream, 0); // Write Session Count (defaults to 0)
  FHeaderEndPosition := FFileStream.Position;
end;

procedure TLKEventRecorderFile.NewSession;
begin
  FSessionCount := GetSessionCount;
  Inc(FSessionCount);

  FSessionBlockSizePosition := FFileStream.Position; // Store a reference to the Session Block Size Position
  StreamWriteInt64(FFileStream, 0); // Write the Session Size (defaults to 0)
  StreamWriteDateTime(FFileStream, Now); // Write the Started Timestamp
  FSessionEventCountPosition := FFileStream.Position; // Store a reference to the current Session's Event Count
  StreamWriteInteger(FFileStream, 0); // Write the Event Count (defaults to 0)

  StreamWriteInteger(FFileStream, FSessionCount, FSessionCountPosition); // Update the Session Count
end;

procedure TLKEventRecorderFile.RecordEvent(const AEvent: TLKEvent);
var
  LPosition: Int64;
  LCount: Int64;
begin
  LPosition := FFileStream.Position;
  AEvent.SaveToStream(FFileStream);
  LCount := StreamReadInt64(FFileStream, FSessionEventCountPosition);
  Inc(LCount);
  StreamWriteInt64(FFileStream, LCount, FSessionEventCountPosition);
  FFileStream.Position := LPosition;
end;

procedure TLKEventRecorderFile.Replay(const ASession: Integer);
var
  I, LEventCount: Integer;
  LSessionSize: Int64;
  LEvent: TLKEvent;
  LStreamableType: TLKStreamableType;
begin
  FFileStream.Position := FHeaderEndPosition;
  for I := 0 to ASession do
  begin
    LSessionSize := StreamReadInt64(FFileStream);
    FFileStream.Seek(-SizeOf(Int64), soCurrent);
    FFileStream.Seek(LSessionSize, soCurrent);
  end;
  StreamReadInt64(FFileStream); // Read the size of the block for the current session
  StreamReadDateTime(FFileStream); // Read the created timestamp
  LEventCount := StreamReadInteger(FFileStream); // Read the number of Events in this session
  for I := 0 to LEventcount - 1 do
  begin
    LStreamableType := Streamables.GetStreamableTypeFromStream(FFileStream);
    if Streamables.StreamableTypeMatch(FFileStream, TLKEvent) then
    begin
      LEvent := TLKEvent(LStreamableType.CreateFromStream(FFileStream));
      LEvent.IsReplay := True;
      case LEvent.DispatchMethod of
        edQueue: LEvent.Queue;
        edStack: LEvent.Stack;
      end;
    end;
  end;
end;

end.
