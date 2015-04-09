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
unit LKSL.Streams.Main;

interface

{$I LKSL.inc}

{$IFDEF FPC}
  {$IFDEF LKSL_MODE_FPC}
    {$mode objfpc}{$H+}
  {$ELSE}
    {$mode delphi}
  {$ENDIF LKSL_MODE_FPC}
{$ENDIF FPC}

{
  About this unit:
    - This unit provides fundamental base functionality for working wtih Streams.
}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs, System.RTLConsts,
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF MSWINDOWS}
  {$ELSE}
    Classes, SysUtils, SyncObjs, RTLConsts,
    {$IFDEF MSWINDOWS}Windows,{$ENDIF MSWINDOWS}
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types,
  LKSL.Generics.Collections;

  {$I LKSL_RTTI.inc}

type
  { Interface Forward Declarations }
  ILKStreamCaret = interface;
  ILKStream = interface;
  ILKHandleStreamCaret = interface;
  ILKHandleStream = interface;
  ILKFileStreamCaret = interface;
  ILKFileStream = interface;
  ILKMemoryStreamCaret = interface;
  ILKMemoryStream  = interface;
  { Class Forward Declarations }
  TLKStreamCaret = class;
  TLKStream = class;
  TLKHandleStreamCaret = class;
  TLKHandleStream = class;
  TLKFileStreamCaret = class;
  TLKFileStream = class;
  TLKMemoryStreamCaret = class;
  TLKMemoryStream = class;

  { Exceptions }
  ELKStreamException = class(ELKException);
    ELKStreamHasGoneAwayException = class(ELKStreamException);

  { Class References }
  TLKStreamCaretClass = class of TLKStreamCaret;

  { Generics Collections }
  TLKStreamCaretList = class(TLKList<TLKStreamCaret>);

  ILKStreamCaret = interface
  ['{D8E849E5-A5A1-4B4F-9AF6-BBD397216C5B}']
    function GetPosition: Int64;
    procedure SetPosition(const APosition: Int64);

    function GetStream: TLKStream;

    ///  <summary><c>Deletes the given number of Bytes from the current Position in the Stream, then compacts the Stream by that number of Bytes (shifting any subsequent Bytes to the left)</c></summary>
    ///  <returns><c>Returns the number of Bytes deleted.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>Automatically shifts the Position of subsequent Carets by the offset of Bytes deleted.</c></para>
    ///  </remarks>
    function Delete(const ALength: Int64): Int64;

    ///  <summary><c>Inserts the given Buffer into the current Position within the Stream (shifting any subsequent Bytes to the right)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>Automatically shifts the Position of subsequent Carets by the offset of Bytes inserted.</c></para>
    ///  </remarks>
    function Insert(const ABuffer; const ALength: Int64): Int64;

    ///  <summary><c>Moves the block of Bytes (from Position to Position + ACount) in the direction of AOffset, then nulls the original Addresses (accounting for any overlap).</c></summary>
    ///  <remarks>
    ///    <para><c>Locks the Stream for the duration of operation</c></para>
    ///  </remarks>
    function MoveBytes(const ACount: Int64; const AOffset: Int64): Int64;

    ///  <summary><c>Reads the specified number of Bytes from the Array into the specified Address</c></summary>
    ///  <returns><c>Returns the number of Bytes actually read.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream only when reading each Byte (releases Lock between Bytes)</c></para>
    ///  </remarks>
    function Read(var ABuffer; const ALength: Int64): Int64;

    ///  <summary><c>Writes the given Buffer into the current Position within the Stream (overwriting any existing data, and expanding the Size of the Stream if required)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>DOES NOT shift the position of any subsequent Carets!</c></para>
    ///  </remarks>
    function Write(const ABuffer; const ALength: Int64): Int64;

    ///  <returns><c>Returns the new </c>Position<c> in the Stream.</c></returns>
    function Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;

    property Position: Int64 read GetPosition write SetPosition;
    property Stream: TLKStream read GetStream;
  end;

  ILKStream = interface
  ['{07F45B12-1DFC-453A-B95C-E00C9F5F4285}']
    function GetSize: Int64;
    procedure SetSize(const ASize: Int64);

    function NewCaret: ILKStreamCaret; overload;
    function NewCaret(const APosition: Int64): ILKStreamCaret; overload;

    property Size: Int64 read GetSize write SetSize;
  end;

  ILKHandleStreamCaret = interface(ILKStreamCaret)
  ['{292415DC-68E3-4317-8EE1-5DF1E36097DB}']

  end;

  ILKHandleStream = interface(ILKStream)
  ['{88BE52C5-0BAD-4E05-A0B3-9DA2C0330F22}']

  end;

  ILKFileStreamCaret = interface(ILKHandleStreamCaret)
  ['{D3D56456-E6A6-49CE-985A-3407009EA2C9}']

  end;

  ILKFileStream = interface(ILKHandleStream)
  ['{62B111EB-938E-4F2D-BA94-30504646F90F}']

  end;

  ILKMemoryStreamCaret = interface(ILKStreamCaret)
  ['{1FB5A4F9-8FFB-4A79-B364-01D6E428A718}']

  end;

  ILKMemoryStream  = interface(ILKStream)
  ['{289F1193-AE69-47D6-B66B-0174070963B5}']
    procedure SaveToFile(const AFileName: String);
    procedure SaveToStream(const AStream: ILKStream);
  end;

  ///  <summary><c>Abstract Base Class for Stream Reading Carets.</c></summary>
  TLKStreamCaret = class abstract(TLKInterfacedObject, ILKStreamCaret)
  private
    FPosition: Int64;
    FStream: TLKStream;
    // Getters
    function GetPosition: Int64;
    function GetStream: TLKStream;
    // Setters
    procedure SetPosition(const APosition: Int64);
  protected
    function DeleteActual(const ALength: Int64): Int64; virtual; abstract;
    function InsertActual(const ABuffer; const ALength: Int64): Int64; virtual; abstract;
    function MoveBytesActual(const ACount: Int64; const AOffset: Int64): Int64; virtual; abstract;
    function ReadActual(var ABuffer; const ALength: Int64): Int64; virtual; abstract;
    function WriteActual(const ABuffer; const ALength: Int64): Int64; virtual; abstract;
    function SeekActual(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64; virtual; abstract;
  public
    constructor Create(const AStream: TLKStream); reintroduce;
    destructor Destroy; override;

    ///  <summary><c>Deletes the given number of Bytes from the current Position in the Stream, then compacts the Stream by that number of Bytes (shifting any subsequent Bytes to the left)</c></summary>
    ///  <returns><c>Returns the new </c>Size<c> of the Stream.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>Automatically shifts the Position of subsequent Carets by the offset of Bytes deleted.</c></para>
    ///  </remarks>
    function Delete(const ALength: Int64): Int64;

    ///  <summary><c>Inserts the given Buffer into the current Position within the Stream (shifting any subsequent Bytes to the right)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>Automatically shifts the Position of subsequent Carets by the offset of Bytes inserted.</c></para>
    ///  </remarks>
    function Insert(const ABuffer; const ALength: Int64): Int64;

    ///  <summary><c>Moves the block of Bytes (from Position to Position + ACount) in the direction of AOffset, then nulls the original Addresses (accounting for any overlap).</c></summary>
    ///  <remarks>
    ///    <para><c>Locks the Stream for the duration of operation</c></para>
    ///  </remarks>
    function MoveBytes(const ACount: Int64; const AOffset: Int64): Int64;

    ///  <summary><c>Reads the specified number of Bytes from the Array into the specified Address</c></summary>
    ///  <returns><c>Returns the number of Bytes actually read.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream only when reading each Byte (releases Lock between Bytes)</c></para>
    ///  </remarks>
    function Read(var ABuffer; const ALength: Int64): Int64;

    ///  <summary><c>Writes the given Buffer into the current Position within the Stream (overwriting any existing data, and expanding the Size of the Stream if required)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>DOES NOT shift the position of any subsequent Carets!</c></para>
    ///  </remarks>
    function Write(const ABuffer; const ALength: Int64): Int64;

    ///  <returns><c>Returns the new </c>Position<c> in the Stream.</c></returns>
    function Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;

    property Position: Int64 read GetPosition write SetPosition;
  end;

  ///  <summary><c>Abstract Base Type for all Multi-Thread Access Streams.</c></summary>
  ///  <remarks>
  ///    <para><c>Each consuming Thread provides its own Caret.</c></para>
  ///    <para><c>Locking only occurs to prevent Write-During-Read and Read-During-Write.</c></para>
  ///  </remarks>
  TLKStream = class abstract(TLKInterfacedObject, ILKStream)
  private
    FDestroying: Boolean;
    //  <summary><c>Event is Set when all Read Operations are complete.</c></summary>
    FReadOpen: TEvent;
    ///  <summary><c>Keeps track of the number of Read Operations in progress (atomic)</c></summary>
    FReads: Integer;
    ///  <summary><c>Write operations MUST be Exclusive not only of Readers, but also of others attempting to Write</c></summary>
    FWriteLock: TCriticalSection;
    ///  <summary><c>Event is Set when all Write Operations are complete.</c></summary>
    FWriteOpen: TEvent;
    ///  <summary><c>Keeps track of the number of Write Operations in progress (atomic)</c></summary>
    FWrites: Integer;
    FCarets: TLKStreamCaretList;
    procedure RegisterCaret(const ACaret: TLKStreamCaret);
    procedure UnregisterCaret(const ACaret: TLKStreamCaret);

    function GetSize: Int64;
    ///  <remarks>
    ///    <para><c>Locks the Stream for the duration of operation</c></para>
    ///    <para><c>Where the new Size is smaller than the previous Size, all Carets no longer in range will have their Position shifted to the new End Of Stream</c></para>
    ///  </remarks>
    procedure SetSize(const ASize: Int64);
  protected
    ///  <summary><c>Override to define the correct Stream Caret Type to use for this Stream</c></summary>
    function GetCaretType: TLKStreamCaretClass; virtual; abstract;

    function GetSizeActual: Int64; virtual; abstract;
    procedure SetSizeActual(const ASize: Int64); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    ///  <summary><c>Waits for all Writes to be completed, then increments the Read Count (locking Write access)</c></summary>
    procedure AcquireRead;
    ///  <summary><c>Waits for all Reads to be completed, then increments the Write Count (locking Read access)</c></summary>
    procedure AcquireWrite;
    ///  <summary><c>Decrements the Read Count (unlocking Write access if that count hits 0)</c></summary>
    procedure ReleaseRead;
    ///  <summary><c>Decrements the Write Count (unlocking Read access if that count hits 0)</c></summary>
    procedure ReleaseWrite;

    function NewCaret: ILKStreamCaret; overload;
    function NewCaret(const APosition: Int64): ILKStreamCaret; overload;

    property Size: Int64 read GetSize write SetSize;
  end;

  TLKHandleStreamCaret = class(TLKStreamCaret, ILKHandleStreamCaret)
  protected
    function DeleteActual(const ALength: Int64): Int64; override;
    function InsertActual(const ABuffer; const ALength: Int64): Int64;  override;
    function MoveBytesActual(const ACount: Int64; const AOffset: Int64): Int64; override;
    function ReadActual(var ABuffer; const ALength: Int64): Int64; override;
    function WriteActual(const ABuffer; const ALength: Int64): Int64; override;
    function SeekActual(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64; override;
  end;

  TLKHandleStream = class(TLKStream, ILKHandleStream)
  private
    FHandle: THandle;
  protected
    function GetCaretType: TLKStreamCaretClass; override;

    function GetSizeActual: Int64; override;
    procedure SetSizeActual(const ASize: Int64); override;
  public
    constructor Create(const AHandle: THandle); reintroduce;
  end;

  TLKFileStreamCaret = class(TLKHandleStreamCaret, ILKFileStreamCaret)
  protected
    function DeleteActual(const ALength: Int64): Int64; override;
    function InsertActual(const ABuffer; const ALength: Int64): Int64; override;
    function MoveBytesActual(const ACount: Int64; const AOffset: Int64): Int64; override;
    function ReadActual(var ABuffer; const ALength: Int64): Int64; override;
    function WriteActual(const ABuffer; const ALength: Int64): Int64; override;
  end;

  ///  <summary><c>Special File Stream built for Multi-Thread Asynchronous (and Random) Access.</c></summary>
  ///  <remarks><c>Multi-Read, Exclusive-Write</c></remarks>
  TLKFileStream = class(TLKStream, ILKFileStream)
  protected
    function GetCaretType: TLKStreamCaretClass; override;
  public
    constructor Create(const AFileName: String; const AMode: Word); reintroduce;
  end;

  ///  <summary><c>Caret specifically set up for Memory Streams.</c></summary>
  TLKMemoryStreamCaret = class(TLKStreamCaret, ILKMemoryStreamCaret)
  protected
    function DeleteActual(const ALength: Int64): Int64; override;
    function InsertActual(const ABuffer; const ALength: Int64): Int64; override;
    function MoveBytesActual(const ACount: Int64; const AOffset: Int64): Int64; override;
    function ReadActual(var ABuffer; const ALength: Int64): Int64; override;
    function WriteActual(const ABuffer; const ALength: Int64): Int64; override;
    function SeekActual(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64; override;
  end;

  ///  <summary><c>Special Memory Stream built for Multi-Thread Asynchronous (and Random) Access.</c></summary>
  ///  <remarks><c>Multi-Read, Exclusive-Write</c></remarks>
  TLKMemoryStream = class(TLKStream, ILKMemoryStream)
  private
    FCapacity: Int64;
    FMemory: Pointer;
    FSize: Int64;
    function Realloc(var ACapacity: Int64): Pointer;
    procedure SetCapacity(ACapacity: Int64);
    procedure SetPointer(const APointer: Pointer; const ASize: Int64);
  protected
    procedure Clear;
    function GetCaretType: TLKStreamCaretClass; override;

    function GetSizeActual: Int64; override;
    procedure SetSizeActual(const ASize: Int64); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SaveToFile(const AFileName: String);
    procedure SaveToStream(const AStream: ILKStream);
  end;

// Utility Methods
procedure StreamClearSpace(const AStream: TStream; const ASize: Int64); overload;
procedure StreamClearSpace(const AStream: TStream; const APosition: Int64; const ASize: Int64); overload;
procedure StreamMakeSpace(const AStream: TStream; const ASize: Int64); overload;
procedure StreamMakeSpace(const AStream: TStream; const APosition: Integer; const ASize: Int64); overload;

implementation

procedure StreamClearSpace(const AStream: TStream; const ASize: Int64);
begin
  StreamClearSpace(AStream, AStream.Position, ASize);
end;

procedure StreamClearSpace(const AStream: TStream; const APosition, ASize: Int64);
var
  I: Int64;
  LValue: Byte;
begin
  I := APosition + (ASize - 1);
  repeat
    AStream.Position := I;
    AStream.Read(LValue, 1);
    AStream.Position := I - ASize;
    AStream.Write(LValue, 1);
    I := I + 1;
  until I = AStream.Size + 1;
  AStream.Size := AStream.Size - ASize;
  AStream.Position := APosition;
end;

procedure StreamMakeSpace(const AStream: TStream; const ASize: Int64);
begin
  StreamMakeSpace(AStream, AStream.Position, ASize);
end;

procedure StreamMakeSpace(const AStream: TStream; const APosition: Integer; const ASize: Int64);
var
  I: Int64;
  LValue: Byte;
begin
  AStream.Size := AStream.Size + ASize;
  AStream.Position := APosition;
  I := AStream.Size - (ASize - 1);
  repeat
    AStream.Position := I;
    AStream.Read(LValue, 1);
    AStream.Position := I + ASize;
    AStream.Write(LValue, 1);
    I := I - 1;
  until I = APosition - 1;
  AStream.Position := APosition;
end;

{ TLKStreamCaret }

constructor TLKStreamCaret.Create(const AStream: TLKStream);
begin
  inherited Create;
  FStream := AStream;
  Seek(0, soBeginning);
  GetStream.RegisterCaret(Self);
end;

function TLKStreamCaret.Delete(const ALength: Int64): Int64;
begin
  GetStream.AcquireWrite;
  try
    Result := DeleteActual(ALength);
  finally
    GetStream.ReleaseWrite;
  end;
end;

destructor TLKStreamCaret.Destroy;
begin
  if FStream <> nil then
    GetStream.UnregisterCaret(Self);
  inherited;
end;

function TLKStreamCaret.GetPosition: Int64;
begin
  Result := Seek(0, soCurrent);
end;

function TLKStreamCaret.GetStream: TLKStream;
begin
  if FStream = nil then
    raise ELKStreamHasGoneAwayException.Create('The Stream to which this Caret belongs has been Destroyed!')
  else
    Result := FStream;
end;

function TLKStreamCaret.MoveBytes(const ACount, AOffset: Int64): Int64;
begin
  GetStream.AcquireWrite;
  try
    Result := MoveBytes(ACount, AOffset);
  finally
    GetStream.ReleaseWrite;
  end;
end;

function TLKStreamCaret.Read(var ABuffer; const ALength: Int64): Int64;
begin
  GetStream.AcquireRead;
  try
    Result := ReadActual(ABuffer, ALength);
  finally
    GetStream.ReleaseRead;
  end;
end;

function TLKStreamCaret.Insert(const ABuffer; const ALength: Int64): Int64;
begin
  GetStream.AcquireWrite;
  try
    Result := InsertActual(ABuffer, ALength);
  finally
    GetStream.ReleaseWrite;
  end;
end;

function TLKStreamCaret.Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
begin
  GetStream.AcquireRead;
  try
    Result := SeekActual(AOffset, AOrigin);
  finally
    GetStream.ReleaseRead;
  end;
  FPosition := Result;
end;

procedure TLKStreamCaret.SetPosition(const APosition: Int64);
begin
  Seek(APosition, soBeginning);
end;

function TLKStreamCaret.Write(const ABuffer; const ALength: Int64): Int64;
begin
  GetStream.AcquireWrite;
  try
    Result := WriteActual(ABuffer, ALength);
  finally
    GetStream.ReleaseWrite;
  end;
end;

{ TLKStream }

procedure TLKStream.AcquireRead;
begin
  FWriteOpen.WaitFor(INFINITE);
  Lock;
  try
    Inc(FReads);
  finally
    Unlock;
  end;
  if FReads = 1 then
    FReadOpen.ResetEvent;
end;

procedure TLKStream.AcquireWrite;
begin
  FReadOpen.WaitFor(INFINITE);
  Lock;
  try
    Inc(FWrites);
  finally
    Unlock;
  end;
  if FWrites = 1 then
    FWriteOpen.ResetEvent;
end;

constructor TLKStream.Create;
begin
  inherited;
  FWriteLock := TCriticalSection.Create;
  FDestroying := False;
  FCarets := TLKStreamCaretList.Create;
  FReadOpen := TEvent.Create(nil, True, True, '');
  FReads := 0;
  FWriteOpen := TEvent.Create(nil, True, True, '');
  FWrites := 0;
  Size := 0;
end;

destructor TLKStream.Destroy;
var
  I: Integer;
begin
  FDestroying := True; // Mark this Stream as "in destruction"
  FReadOpen.SetEvent; // Prevent wait halting
  FWriteOpen.SetEvent; // Prevent wait halting
  FReadOpen.Free;
  FWriteOpen.Free;
  for I := 0 to FCarets.Count - 1 do
    FCarets[I].FStream := nil;
  FCarets.Free;
  FWriteLock.Free;
  inherited;
end;

function TLKStream.GetSize: Int64;
begin
  AcquireRead;
  try
    Result := GetSizeActual;
  finally
    ReleaseRead;
  end;
end;

function TLKStream.NewCaret(const APosition: Int64): ILKStreamCaret;
begin
  Result := NewCaret;
  Result.Position := APosition;
end;

procedure TLKStream.RegisterCaret(const ACaret: TLKStreamCaret);
begin
  FCarets.Lock;
  try
    if (not FCarets.Contains(ACaret)) then
      FCarets.Add(ACaret);
  finally
    FCarets.Unlock;
  end;
end;

procedure TLKStream.ReleaseRead;
begin
  Lock;
  try
    Dec(FReads);
  finally
    Unlock;
  end;
  if FReads = 0 then
    FReadOpen.SetEvent;
end;

procedure TLKStream.ReleaseWrite;
begin
  Lock;
  try
    Dec(FWrites);
  finally
    Unlock;
  end;
  if FWrites = 0 then
    FWriteOpen.SetEvent;
end;

procedure TLKStream.SetSize(const ASize: Int64);
begin
  AcquireWrite;
  try
    SetSizeActual(ASize);
  finally
    ReleaseWrite;
  end;
end;

procedure TLKStream.UnregisterCaret(const ACaret: TLKStreamCaret);
var
  LIndex: Integer;
begin
  FCarets.Lock;
  try
    LIndex := FCarets.IndexOf(ACaret);
    if LIndex > -1 then
      FCarets.Delete(LIndex);
  finally
    FCarets.Unlock;
  end;
end;

function TLKStream.NewCaret: ILKStreamCaret;
begin
  Result := GetCaretType.Create(Self);
end;

{ TLKHandleStreamCaret }

function TLKHandleStreamCaret.DeleteActual(const ALength: Int64): Int64;
var
  LStartPosition, I: Int64;
  LValue: Byte;
begin
  Result := 0;
  GetStream.Lock;
  try
    FileSeek(TLKHandleStream(GetStream).FHandle, Position, Ord(soBeginning));
    LStartPosition := Position;
    I := Position + (ALength - 1);
    repeat
      Position := I;
      ReadActual(LValue, 1);
      FileSeek(TLKHandleStream(GetStream).FHandle, I - ALength, Ord(soBeginning));
      WriteActual(LValue, 1);
      Inc(I);
      Inc(Result);
    until I = GetStream.Size + 1;
    GetStream.Size := GetStream.Size - ALength;
    Position := LStartPosition;
  finally
    GetStream.Unlock;
  end;
end;

function TLKHandleStreamCaret.InsertActual(const ABuffer; const ALength: Int64): Int64;
begin
  Result := 0;
{ TODO -oSJS -cTLKMemoryStream : Implement Insert }
end;

function TLKHandleStreamCaret.MoveBytesActual(const ACount, AOffset: Int64): Int64;
begin
  Result := 0;
{ TODO -oSJS -cTLKMemoryStream : Implement Move Bytes }
end;

function TLKHandleStreamCaret.ReadActual(var ABuffer; const ALength: Int64): Int64;
begin
  GetStream.Lock;
  try
    FileSeek(TLKHandleStream(GetStream).FHandle, Position, Ord(soBeginning));
    Result := FileRead(TLKHandleStream(GetStream).FHandle, ABuffer, ALength);
    if Result = -1 then
      Result := 0;
  finally
    GetStream.Unlock;
  end;
end;

function TLKHandleStreamCaret.SeekActual(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
begin
  Result := FileSeek(TLKHandleStream(GetStream).FHandle, AOffset, Ord(AOrigin));
end;

function TLKHandleStreamCaret.WriteActual(const ABuffer; const ALength: Int64): Int64;
begin
  GetStream.Lock;
  try
    FileSeek(TLKHandleStream(GetStream).FHandle, FPosition, Ord(soBeginning));
    Result := FileWrite(TLKHandleStream(GetStream).FHandle, ABuffer, ALength);
    if Result = -1 then
      Result := 0;
  finally
    GetStream.Unlock;
  end;
end;

{ TLKHandleStream }

constructor TLKHandleStream.Create(const AHandle: THandle);
begin
  inherited Create;;
  FHandle := AHandle;
end;

function TLKHandleStream.GetCaretType: TLKStreamCaretClass;
begin
  Result := TLKHandleStreamCaret;
end;

function TLKHandleStream.GetSizeActual: Int64;
begin
  Result := 0;
  { TODO -oSJS -cTLKHandleStream : Implement GetSizeActual }
end;

procedure TLKHandleStream.SetSizeActual(const ASize: Int64);
begin
{$IF Defined(MSWINDOWS)}
  Win32Check(SetEndOfFile(FHandle));
{$ELSEIF Defined(POSIX)}
  if ftruncate(FHandle, Position) = -1 then
    raise EStreamError(sStreamSetSize);
{$ENDIF POSIX}
end;

{ TLKFileStreamCaret }

function TLKFileStreamCaret.DeleteActual(const ALength: Int64): Int64;
begin

end;

function TLKFileStreamCaret.InsertActual(const ABuffer; const ALength: Int64): Int64;
begin

end;

function TLKFileStreamCaret.MoveBytesActual(const ACount, AOffset: Int64): Int64;
begin

end;

function TLKFileStreamCaret.ReadActual(var ABuffer; const ALength: Int64): Int64;
begin

end;

function TLKFileStreamCaret.WriteActual(const ABuffer; const ALength: Int64): Int64;
begin

end;

{ TLKFileStream }

constructor TLKFileStream.Create(const AFileName: String; const AMode: Word);
begin

end;

function TLKFileStream.GetCaretType: TLKStreamCaretClass;
begin
  Result := TLKFileStreamCaret;
end;

{ TLKMemoryStream }

procedure TLKMemoryStream.Clear;
begin
  SetCapacity(0);
  FSize := 0;
end;

constructor TLKMemoryStream.Create;
begin
  inherited;
  Clear;
end;

destructor TLKMemoryStream.Destroy;
begin
  Clear;
  inherited;
end;

function TLKMemoryStream.GetCaretType: TLKStreamCaretClass;
begin
  Result := TLKMemoryStreamCaret;
end;

function TLKMemoryStream.GetSizeActual: Int64;
begin
  Result := FSize;
end;

function TLKMemoryStream.Realloc(var ACapacity: Int64): Pointer;
const
  MemoryDelta = $2000;
begin
  if (ACapacity > 0) and (ACapacity <> FSize) then
    ACapacity := (ACapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := FMemory;
  if ACapacity <> FCapacity then
  begin
    if ACapacity = 0 then
    begin
      FreeMem(FMemory);
      Result := nil;
    end else
    begin
      if FCapacity = 0 then
        GetMem(Result, ACapacity)
      else
        ReallocMem(Result, ACapacity);
      if Result = nil then raise EStreamError.CreateRes(@SMemoryStreamError);
    end;
  end;
end;

procedure TLKMemoryStream.SaveToFile(const AFileName: String);
var
  LStream: ILKFileStream;
begin
  LStream := TLKFileStream.Create(AFileName, fmCreate);
  SaveToStream(LStream);
end;

procedure TLKMemoryStream.SaveToStream(const AStream: ILKStream);
var
  LCaret: ILKStreamCaret;
begin
  AcquireRead;
  try
    if FSize > 0 then
    begin
      LCaret := AStream.NewCaret;
      LCaret.Write(FMemory^, FSize);
    end;
  finally
    ReleaseRead;
  end;
end;

procedure TLKMemoryStream.SetCapacity(ACapacity: Int64);
begin
  SetPointer(Realloc(ACapacity), FSize);
  FCapacity := ACapacity;
end;

procedure TLKMemoryStream.SetPointer(const APointer: Pointer; const ASize: Int64);
begin
  FMemory := APointer;
  FSize := ASize;
end;

procedure TLKMemoryStream.SetSizeActual(const ASize: Int64);
var
  LOldSize: Longint;
begin
  LOldSize := FSize;
  SetCapacity(ASize);
  FSize := ASize;
  if LOldSize < ASize then
  begin
    { TODO -oSJS -cTLKMemoryStream : Shift all Carets that're off the end of the Stream }
  end;
end;

{ TLKMemoryStreamCaret }

function TLKMemoryStreamCaret.DeleteActual(const ALength: Int64): Int64;
begin
  // Shift elements after Position + Length back to Position
  if FPosition + ALength < TLKMemoryStream(GetStream).FSize then
    System.Move(
                  (PByte(TLKMemoryStream(GetStream).FMemory) + FPosition + ALength)^,
                  (PByte(TLKMemoryStream(GetStream).FMemory) + FPosition)^,
                  TLKMemoryStream(GetStream).FSize - (FPosition + ALength)
               );
  // Dealloc Memory
  TLKMemoryStream(GetStream).Size := TLKMemoryStream(GetStream).FSize - ALength;
  Result := ALength
end;

function TLKMemoryStreamCaret.InsertActual(const ABuffer; const ALength: Int64): Int64;
begin
  Result := 0;
{ TODO -oSJS -cTLKMemoryStream : Implement Insert }
end;

function TLKMemoryStreamCaret.MoveBytesActual(const ACount, AOffset: Int64): Int64;
begin
  Result := 0;
{ TODO -oSJS -cTLKMemoryStream : Implement Move Bytes }
end;

function TLKMemoryStreamCaret.ReadActual(var ABuffer; const ALength: Int64): Int64;
begin
  Result := 0;
  if (FPosition < 0) or (ALength < 0) then
    Exit;

  Result := TLKMemoryStream(GetStream).FSize - FPosition;
  if Result > 0 then
  begin
    if Result > ALength then Result := ALength;
    System.Move((PByte(TLKMemoryStream(GetStream).FMemory) + FPosition)^, ABuffer, Result);
    Inc(FPosition, Result);
  end;
end;

function TLKMemoryStreamCaret.SeekActual(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
begin
  case AOrigin of
    soBeginning: Result := AOffset;
    soCurrent: Result := FPosition + AOffset;
    soEnd: Result := TLKMemoryStream(FStream).FSize - AOffset;
  end;
end;

function TLKMemoryStreamCaret.WriteActual(const ABuffer; const ALength: Int64): Int64;
var
  LPos: Int64;
begin
  Result := 0;
  LPos := FPosition + ALength;

  if (FPosition < 0) or (ALength < 0) or (LPos <= 0) then
    Exit;

  if LPos > TLKMemoryStream(GetStream).FSize then
  begin
    if LPos > TLKMemoryStream(GetStream).FCapacity then
      TLKMemoryStream(GetStream).SetCapacity(LPos);
    TLKMemoryStream(GetStream).FSize := LPos;
  end;
  System.Move(ABuffer, (PByte(TLKMemoryStream(GetStream).FMemory) + FPosition)^, ALength);
  FPosition := LPos;
  Result := ALength;
end;

end.
