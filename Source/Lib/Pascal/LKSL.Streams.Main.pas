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
unit LKSL.Streams.Main;

interface

{$I LKSL.inc}

{
  About this unit:
    - This unit provides fundamental base functionality for working wtih Streams.
}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs, System.RTLConsts,
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF MSWINDOWS}
    {$IFDEF POSIX}Posix.UniStd,{$ENDIF POSIX}
  {$ELSE}
    Classes, SysUtils, SyncObjs, RTLConsts,
    {$IFDEF MSWINDOWS}Windows,{$ENDIF MSWINDOWS}
    {$IFDEF POSIX}Posix,{$ENDIF POSIX}
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
    ELKStreamCaretInvalid = class(ELKStreamException);

  { Class References }
  TLKStreamCaretClass = class of TLKStreamCaret;

  { Generics Collections }
  TLKStreamCaretList = class(TLKList<TLKStreamCaret>);

  ILKStreamCaret = interface(ILKInterface)
  ['{D8E849E5-A5A1-4B4F-9AF6-BBD397216C5B}']
    function GetInvalid: Boolean;
    function GetPosition: Int64;
    procedure SetPosition(const APosition: Int64);

    function GetStream: ILKStream;

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

    property Invalid: Boolean read GetInvalid;
    property Position: Int64 read GetPosition write SetPosition;
    property Stream: ILKStream read GetStream;
  end;

  ILKStream = interface(ILKInterface)
  ['{07F45B12-1DFC-453A-B95C-E00C9F5F4285}']
    function GetSize: Int64;
    procedure SetSize(const ASize: Int64);

    ///  <summary><c>Waits for all Writes to be completed, then increments the Read Count (locking Write access)</c></summary>
    procedure AcquireReadLock;
    ///  <summary><c>Waits for all Reads to be completed, then increments the Write Count (locking Read access)</c></summary>
    procedure AcquireWriteLock;
    ///  <summary><c>Decrements the Read Count (unlocking Write access if that count hits 0)</c></summary>
    procedure ReleaseReadLock;
    ///  <summary><c>Decrements the Write Count (unlocking Read access if that count hits 0)</c></summary>
    procedure ReleaseWriteLock;

    procedure LoadFromFile(const AFileName: String);
    procedure LoadFromStream(const AStream: ILKStream); overload;
    procedure LoadFromStream(const AStream: TStream); overload;

    function NewCaret: ILKStreamCaret; overload;
    function NewCaret(const APosition: Int64): ILKStreamCaret; overload;

    procedure SaveToFile(const AFileName: String);
    procedure SaveToStream(const AStream: ILKStream); overload;
    procedure SaveToStream(const AStream: TStream); overload;

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

  end;

  ///  <summary><c>Abstract Base Class for Stream Reading Carets.</c></summary>
  TLKStreamCaret = class abstract(TLKInterfacedObject, ILKStreamCaret)
  protected
    FPosition: Int64;
    FInvalid: Boolean;
    FStream: TLKStream;
    procedure CheckCaretValid; inline;
    // Getters
    function GetInvalid: Boolean;
    function GetPosition: Int64;
    function GetStream: ILKStream;
    // Setters
    procedure SetPosition(const APosition: Int64);
  public
    constructor Create(const AStream: TLKStream); reintroduce;
    destructor Destroy; override;

    ///  <summary><c>Deletes the given number of Bytes from the current Position in the Stream, then compacts the Stream by that number of Bytes (shifting any subsequent Bytes to the left)</c></summary>
    ///  <returns><c>Returns the new </c>Size<c> of the Stream.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>Automatically shifts the Position of subsequent Carets by the offset of Bytes deleted.</c></para>
    ///  </remarks>
    function Delete(const ALength: Int64): Int64; virtual;

    ///  <summary><c>Inserts the given Buffer into the current Position within the Stream (shifting any subsequent Bytes to the right)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>Automatically shifts the Position of subsequent Carets by the offset of Bytes inserted.</c></para>
    ///  </remarks>
    function Insert(const ABuffer; const ALength: Int64): Int64; virtual;

    ///  <summary><c>Reads the specified number of Bytes from the Array into the specified Address</c></summary>
    ///  <returns><c>Returns the number of Bytes actually read.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream only when reading each Byte (releases Lock between Bytes)</c></para>
    ///  </remarks>
    function Read(var ABuffer; const ALength: Int64): Int64; virtual;

    ///  <summary><c>Writes the given Buffer into the current Position within the Stream (overwriting any existing data, and expanding the Size of the Stream if required)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>DOES NOT shift the position of any subsequent Carets!</c></para>
    ///  </remarks>
    function Write(const ABuffer; const ALength: Int64): Int64; virtual;

    ///  <returns><c>Returns the new </c>Position<c> in the Stream.</c></returns>
    function Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64; virtual;

    property Invalid: Boolean read GetInvalid;
    property Position: Int64 read GetPosition write SetPosition;
  end;

  ///  <summary><c>Abstract Base Type for all Multi-Thread Access Streams.</c></summary>
  ///  <remarks>
  ///    <para><c>Each consumer provides its own Caret.</c></para>
  ///    <para><c>Designed to be 100% Thread-Safe</c></para>
  ///  </remarks>
  TLKStream = class abstract(TLKInterfacedObject, ILKStream)
  private
    FCarets: TLKStreamCaretList;
    procedure RegisterCaret(const ACaret: TLKStreamCaret);
    procedure UnregisterCaret(const ACaret: TLKStreamCaret);

    procedure InvalidateCarets(const AFromPosition, ACount: Int64);

    procedure ShiftCaretsLeft(const AFromPosition, ACount: Int64);
    procedure ShiftCaretsRight(const AFromPosition, ACount: Int64);
  protected
    ///  <summary><c>Override to define the correct Stream Caret Type to use for this Stream</c></summary>
    function GetCaretType: TLKStreamCaretClass; virtual; abstract;

    function GetSize: Int64; virtual; abstract;
    ///  <remarks>
    ///    <para><c>Locks the Stream for the duration of operation</c></para>
    ///    <para><c>Where the new Size is smaller than the previous Size, all Carets no longer in range will have their Position shifted to the new End Of Stream</c></para>
    ///  </remarks>
    procedure SetSize(const ASize: Int64); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromFile(const AFileName: String); virtual; abstract;
    procedure LoadFromStream(const AStream: ILKStream); overload; virtual; abstract;
    procedure LoadFromStream(const AStream: TStream); overload; virtual; abstract;

    function NewCaret: ILKStreamCaret; overload;
    function NewCaret(const APosition: Int64): ILKStreamCaret; overload;

    procedure SaveToFile(const AFileName: String); virtual; abstract;
    procedure SaveToStream(const AStream: ILKStream); overload; virtual; abstract;
    procedure SaveToStream(const AStream: TStream); overload; virtual; abstract;

    property Size: Int64 read GetSize write SetSize;
  end;

  ///  <summary><c>Caret specifically set up for Handle Streams.</c></summary>  
  TLKHandleStreamCaret = class(TLKStreamCaret, ILKHandleStreamCaret)
  public
    ///  <summary><c>Deletes the given number of Bytes from the current Position in the Stream, then compacts the Stream by that number of Bytes (shifting any subsequent Bytes to the left)</c></summary>
    ///  <returns><c>Returns the new </c>Size<c> of the Stream.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>Automatically shifts the Position of subsequent Carets by the offset of Bytes deleted.</c></para>
    ///  </remarks>
    function Delete(const ALength: Int64): Int64; override;

    ///  <summary><c>Inserts the given Buffer into the current Position within the Stream (shifting any subsequent Bytes to the right)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>Automatically shifts the Position of subsequent Carets by the offset of Bytes inserted.</c></para>
    ///  </remarks>
    function Insert(const ABuffer; const ALength: Int64): Int64; override;

    ///  <summary><c>Reads the specified number of Bytes from the Array into the specified Address</c></summary>
    ///  <returns><c>Returns the number of Bytes actually read.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream only when reading each Byte (releases Lock between Bytes)</c></para>
    ///  </remarks>
    function Read(var ABuffer; const ALength: Int64): Int64; override;

    ///  <summary><c>Writes the given Buffer into the current Position within the Stream (overwriting any existing data, and expanding the Size of the Stream if required)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>DOES NOT shift the position of any subsequent Carets!</c></para>
    ///  </remarks>
    function Write(const ABuffer; const ALength: Int64): Int64; override;

    ///  <returns><c>Returns the new </c>Position<c> in the Stream.</c></returns>
    function Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64; override;
  end;

  ///  <summary><c>Special Handle Stream built for Multi-Thread Asynchronous (and Random) Acces.</c></summary>
  ///  <remarks><c>Handle Streams are Exclusive Access (read AND write) using a Transactional Lock</c></remarks>
  TLKHandleStream = class(TLKStream, ILKHandleStream)
  protected
    FHandle: THandle;
    function GetCaretType: TLKStreamCaretClass; override;

    function GetSize: Int64; override;
    procedure SetSize(const ASize: Int64); override;
  public
    constructor Create(const AHandle: THandle); reintroduce; overload;
    constructor Create(const AStream: THandleStream); reintroduce; overload;

    procedure LoadFromFile(const AFileName: String); override;
    procedure LoadFromStream(const AStream: ILKStream); overload; override;
    procedure LoadFromStream(const AStream: TStream); overload; override;

    procedure SaveToFile(const AFileName: String); override;
    procedure SaveToStream(const AStream: ILKStream); overload; override;
    procedure SaveToStream(const AStream: TStream); overload; override;
  end;

  ///  <summary><c>Caret specifically set up for File Streams.</c></summary>
  TLKFileStreamCaret = class(TLKHandleStreamCaret, ILKFileStreamCaret)

  end;

  ///  <summary><c>Special File Stream built for Multi-Thread Asynchronous (and Random) Access.</c></summary>
  ///  <remarks><c>File Streams are Exclusive Access (read AND write) using a Transactional Lock</c></remarks>
  TLKFileStream = class(TLKHandleStream, ILKFileStream)
  private
    FAdoptedHandle: Boolean;
    FFileName: String;
  protected
    function GetCaretType: TLKStreamCaretClass; override;
  public
    constructor Create(const AFileName: String; const AMode: Word); reintroduce; overload;
    constructor Create(const AFileName: String; const AMode: Word; const ARights: Cardinal); reintroduce; overload;
    constructor Create(const AStream: TFileStream); reintroduce; overload;
    destructor Destroy; override;
  end;

  ///  <summary><c>Caret specifically set up for Memory Streams.</c></summary>
  TLKMemoryStreamCaret = class(TLKStreamCaret, ILKMemoryStreamCaret)
  protected
    function DeleteActual(const ALength: Int64): Int64;
    function InsertActual(const ABuffer; const ALength: Int64): Int64;
    function ReadActual(var ABuffer; const ALength: Int64): Int64;
    function WriteActual(const ABuffer; const ALength: Int64): Int64;
    function SeekActual(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
  public
    ///  <summary><c>Deletes the given number of Bytes from the current Position in the Stream, then compacts the Stream by that number of Bytes (shifting any subsequent Bytes to the left)</c></summary>
    ///  <returns><c>Returns the new </c>Size<c> of the Stream.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>Automatically shifts the Position of subsequent Carets by the offset of Bytes deleted.</c></para>
    ///  </remarks>
    function Delete(const ALength: Int64): Int64; override;

    ///  <summary><c>Inserts the given Buffer into the current Position within the Stream (shifting any subsequent Bytes to the right)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>Automatically shifts the Position of subsequent Carets by the offset of Bytes inserted.</c></para>
    ///  </remarks>
    function Insert(const ABuffer; const ALength: Int64): Int64; override;

    ///  <summary><c>Reads the specified number of Bytes from the Array into the specified Address</c></summary>
    ///  <returns><c>Returns the number of Bytes actually read.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream only when reading each Byte (releases Lock between Bytes)</c></para>
    ///  </remarks>
    function Read(var ABuffer; const ALength: Int64): Int64; override;

    ///  <summary><c>Writes the given Buffer into the current Position within the Stream (overwriting any existing data, and expanding the Size of the Stream if required)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    ///  <remarks>
    ///    <para><c>Locks the Stream for duration of operation</c></para>
    ///    <para><c>DOES NOT shift the position of any subsequent Carets!</c></para>
    ///  </remarks>
    function Write(const ABuffer; const ALength: Int64): Int64; override;

    ///  <returns><c>Returns the new </c>Position<c> in the Stream.</c></returns>
    function Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64; override;
  end;

  ///  <summary><c>Special Memory Stream built for Multi-Thread Asynchronous (and Random) Access.</c></summary>
  ///  <remarks><c>Multi-Read, Exclusive-Write</c></remarks>
  TLKMemoryStream = class(TLKStream, ILKMemoryStream)
  private
    FCapacity: Int64;
    FDestroying: Boolean;
    FMemory: Pointer;
    FSize: Int64;
    function Realloc(var ACapacity: Int64): Pointer;
    procedure SetCapacity(ACapacity: Int64);
    procedure SetPointer(const APointer: Pointer; const ASize: Int64);
  protected
    procedure Clear;
    function GetCaretType: TLKStreamCaretClass; override;

    function GetSizeActual: Int64;
    procedure SetSizeActual(const ASize: Int64);

    function GetSize: Int64; override;
    procedure SetSize(const ASize: Int64); override;
  public
    constructor Create; overload; override;
    constructor Create(const AStream: TCustomMemoryStream); reintroduce; overload;
    destructor Destroy; override;

    procedure LoadFromFile(const AFileName: String); override;
    procedure LoadFromStream(const AStream: ILKStream); overload; override;
    procedure LoadFromStream(const AStream: TStream); overload; override;

    procedure SaveToFile(const AFileName: String); override;
    procedure SaveToStream(const AStream: ILKStream); overload; override;
    procedure SaveToStream(const AStream: TStream); overload; override;
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

procedure TLKStreamCaret.CheckCaretValid;
begin
  if FInvalid then
    raise ELKStreamCaretInvalid.Create('The binary data at the position referenced by this Caret has been removed or modified.');
end;

constructor TLKStreamCaret.Create(const AStream: TLKStream);
begin
  inherited Create;
  FInvalid := False;
  FStream := AStream;
  Seek(0, soBeginning);
  FStream.RegisterCaret(Self);
end;

function TLKStreamCaret.Delete(const ALength: Int64): Int64;
begin
  Result := 0;
  CheckCaretValid;
end;

destructor TLKStreamCaret.Destroy;
begin
  if FStream <> nil then
    FStream.UnregisterCaret(Self);
  inherited;
end;

function TLKStreamCaret.GetInvalid: Boolean;
begin
  AcquireReadLock;
  try
    Result := FInvalid;
  finally
    ReleaseReadLock;
  end;
end;

function TLKStreamCaret.GetPosition: Int64;
begin
  Result := Seek(0, soCurrent);
end;

function TLKStreamCaret.GetStream: ILKStream;
begin
  if FStream = nil then
    raise ELKStreamHasGoneAwayException.Create('The Stream to which this Caret belongs has been Destroyed!')
  else
    Result := FStream;
end;

function TLKStreamCaret.Insert(const ABuffer; const ALength: Int64): Int64;
begin
  Result := 0;
  CheckCaretValid;
end;

function TLKStreamCaret.Read(var ABuffer; const ALength: Int64): Int64;
begin
  Result := 0;
  CheckCaretValid;
end;

function TLKStreamCaret.Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
begin
  Result := 0;
  CheckCaretValid;
end;

procedure TLKStreamCaret.SetPosition(const APosition: Int64);
begin
  AcquireWriteLock;
  try
    FPosition := Seek(APosition, soBeginning);
  finally
    ReleaseWriteLock;
  end;
end;

function TLKStreamCaret.Write(const ABuffer; const ALength: Int64): Int64;
begin
  Result := 0;
  CheckCaretValid;
end;

{ TLKStream }

constructor TLKStream.Create;
begin
  inherited;
  FCarets := TLKStreamCaretList.Create;
  Size := 0;
end;

destructor TLKStream.Destroy;
var
  I: Integer;
begin
  for I := 0 to FCarets.Count - 1 do
    FCarets[I].FStream := nil;
  FCarets.Free;
  inherited;
end;

procedure TLKStream.InvalidateCarets(const AFromPosition, ACount: Int64);
var
  I: Integer;
begin
  FCarets.AcquireReadLock;
  try
    for I := 0 to FCarets.Count - 1 do
    begin
      FCarets[I].AcquireWriteLock;
      try
        if (FCarets[I].FPosition >= AFromPosition) and (FCarets[I].FPosition < AFromPosition + ACount) then
          FCarets[I].FInvalid := True;
      finally
        FCarets[I].ReleaseWriteLock;
      end;
    end;
  finally
    FCarets.ReleaseReadLock;
  end;
end;

function TLKStream.NewCaret(const APosition: Int64): ILKStreamCaret;
begin
  Result := NewCaret;
  Result.Position := APosition;
end;

procedure TLKStream.RegisterCaret(const ACaret: TLKStreamCaret);
begin
  FCarets.AcquireWriteLock;
  try
    if (not FCarets.Contains(ACaret)) then
      FCarets.Add(ACaret);
  finally
    FCarets.ReleaseWriteLock;
  end;
end;

procedure TLKStream.ShiftCaretsLeft(const AFromPosition, ACount: Int64);
var
  I: Integer;
begin
  FCarets.AcquireWriteLock;
  try
    for I := 0 to FCarets.Count - 1 do
    begin
      FCarets[I].AcquireWriteLock;
      try
        if (FCarets[I].FPosition >= AFromPosition) and (FCarets[I].FPosition < AFromPosition + ACount) then
          FCarets[I].FPosition := FCarets[I].FPosition - ACount;
      finally
        FCarets[I].ReleaseWriteLock;
      end;
    end;
  finally
    FCarets.ReleaseWriteLock;
  end;
end;

procedure TLKStream.ShiftCaretsRight(const AFromPosition, ACount: Int64);
var
  I: Integer;
begin
  FCarets.AcquireWriteLock;
  try
    for I := 0 to FCarets.Count - 1 do
    begin
      FCarets[I].AcquireWriteLock;
      try
        if (FCarets[I].FPosition >= AFromPosition) and (FCarets[I].FPosition < AFromPosition + ACount) then
          FCarets[I].FPosition := FCarets[I].FPosition + ACount;
      finally
        FCarets[I].ReleaseWriteLock;
      end;
    end;
  finally
    FCarets.ReleaseWriteLock;
  end;
end;

procedure TLKStream.UnregisterCaret(const ACaret: TLKStreamCaret);
var
  LIndex: Integer;
begin
  FCarets.AcquireWriteLock;
  try
    LIndex := FCarets.IndexOf(ACaret);
    if LIndex > -1 then
      FCarets.Delete(LIndex);
  finally
    FCarets.ReleaseWriteLock;
  end;
end;

function TLKStream.NewCaret: ILKStreamCaret;
begin
  Result := GetCaretType.Create(Self);
end;

{ TLKHandleStreamCaret }

function TLKHandleStreamCaret.Delete(const ALength: Int64): Int64;
var
  LStartPosition, LSize: Int64;
  LValue: TBytes;
begin
  inherited;
  FStream.AcquireWriteLock;
  try
    LStartPosition := Position;
    LSize := FStream.Size;
    if FStream.Size > Position + ALength then
    begin
      SetLength(LValue, LSize - ALength);
      Position := Position + ALength;
      Read(LValue[0], LSize - ALength);
      Position := LStartPosition;
      Write(LValue[0], ALength);
    end;
    FStream.Size := LSize - ALength;
    // Invalidate the Carets representing the Bytes we've deleted
    FStream.InvalidateCarets(LStartPosition, ALength);
    // Shift subsequent Carets to the left
    FStream.ShiftCaretsLeft(LStartPosition + ALength, LSize - (LStartPosition + ALength));
    // Set this Caret's position back to where it began
    Position := LStartPosition;
  finally
    FStream.ReleaseWriteLock;
  end;
  Result := ALength;
end;

function TLKHandleStreamCaret.Insert(const ABuffer; const ALength: Int64): Int64;
var
  I, LStartPosition, LNewSize: Int64;
  LByte: Byte;
begin
  inherited;
  Result := 0;
  LStartPosition := FPosition;
  FStream.AcquireWriteLock;
  try
    // Expand the Stream
    LNewSize := FStream.Size + ALength;
    FStream.Size := LNewSize;
    // Move subsequent Bytes to the Right
    I := LStartPosition;
    repeat
      Seek(I, soBeginning); // Navigate to the Byte
      Read(LByte, 1); // Read this byte
      Seek(I + ALength + 1, soBeginning); // Navigate to this Byte's new location
      Write(LByte, 1); // Write this byte
      Inc(I); // On to the next
      Inc(Result);
    until I > LNewSize;
    // Insert the Value
    Position := LStartPosition;
    Write(ABuffer, ALength);
    // Shift overlapping Carets to the Right
    FStream.ShiftCaretsRight(LStartPosition, ALength);
    Position := LStartPosition + ALength;
  finally
    FStream.ReleaseWriteLock;
  end;
end;

function TLKHandleStreamCaret.Read(var ABuffer; const ALength: Int64): Int64;
begin
  inherited;
  FStream.AcquireWriteLock;
  try
    Seek(FPosition, soBeginning);
    Result := FileRead(TLKHandleStream(GetStream).FHandle, ABuffer, ALength);
    if Result = -1 then
      Result := 0
    else
      Inc(FPosition, ALength);
  finally
    FStream.ReleaseWriteLock;
  end;
end;

function TLKHandleStreamCaret.Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
begin
  inherited;
  FStream.AcquireWriteLock;
  try
    Result := FileSeek(TLKHandleStream(GetStream).FHandle, AOffset, Ord(AOrigin));
  finally
    FStream.ReleaseWriteLock;
  end;
end;

function TLKHandleStreamCaret.Write(const ABuffer; const ALength: Int64): Int64;
var
  LStartPosition: Int64;
begin
  inherited;
  FStream.AcquireWriteLock;
  try
    LStartPosition := FPosition;
    Seek(FPosition, soBeginning);
    Result := FileWrite(TLKHandleStream(GetStream).FHandle, ABuffer, ALength);
    if Result = -1 then
      Result := 0
    else begin
      Inc(FPosition, ALength);
      FStream.InvalidateCarets(LStartPosition, ALength);
    end;
  finally
    FStream.ReleaseWriteLock;
  end;
end;

{ TLKHandleStream }

constructor TLKHandleStream.Create(const AStream: THandleStream);
begin
  Create(AStream.Handle);
end;

constructor TLKHandleStream.Create(const AHandle: THandle);
begin
  FHandle := AHandle;
  inherited Create;
end;

function TLKHandleStream.GetCaretType: TLKStreamCaretClass;
begin
  Result := TLKHandleStreamCaret;
end;

function TLKHandleStream.GetSize: Int64;
var
  LPos: Int64;
begin
  AcquireWriteLock;
  try
    LPos := FileSeek(FHandle, 0, Ord(soCurrent));
    Result := FileSeek(FHandle, 0, Ord(soEnd));
    FileSeek(FHandle, LPos, Ord(soBeginning));
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKHandleStream.LoadFromFile(const AFileName: String);
var
  LStream: ILKFileStream;
begin
  LStream := TLKFileStream.Create(AFileName, fmOpenRead);
  LoadFromStream(LStream);
end;

procedure TLKHandleStream.LoadFromStream(const AStream: ILKStream);
var
  LReadCaret, LWriteCaret: ILKStreamCaret;
  I: Int64;
  LValue: Byte;
begin
  AcquireWriteLock;
  try
    Size := AStream.Size;
    LReadCaret := AStream.NewCaret;
    LWriteCaret := NewCaret;
    I := 0;
    repeat
      LReadCaret.Read(LValue, 1);
      LWriteCaret.Write(LValue, 1);
      Inc(I);
    until I > AStream.Size;;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKHandleStream.LoadFromStream(const AStream: TStream);
var
  LWriteCaret: ILKStreamCaret;
  I: Int64;
  LValue: Byte;
begin
  AcquireWriteLock;
  try
    AStream.Position := 0;
    Size := AStream.Size;
    LWriteCaret := NewCaret;
    I := 0;
    repeat
      AStream.Read(LValue, 1);
      LWriteCaret.Write(LValue, 1);
    until I > AStream.Size;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKHandleStream.SaveToFile(const AFileName: String);
var
  LStream: ILKFileStream;
begin
  LStream := TLKFileStream.Create(AFileName, fmCreate);
  SaveToStream(LStream);
end;

procedure TLKHandleStream.SaveToStream(const AStream: ILKStream);
var
  LReadCaret, LWriteCaret: ILKStreamCaret;
  I: Int64;
  LValue: Byte;
begin
  AcquireWriteLock;
  try
    AStream.Size := 0;
    LReadCaret := NewCaret;
    LWriteCaret := AStream.NewCaret;
    I := 0;
    repeat
      LReadCaret.Read(LValue, 1);
      LWriteCaret.Write(LValue, 1);
      Inc(I);
    until I > Size;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKHandleStream.SaveToStream(const AStream: TStream);
var
  LReadCaret: ILKStreamCaret;
  I: Int64;
  LValue: Byte;
begin
  AcquireWriteLock;
  try
    AStream.Size := 0;
    I := 0;
    repeat
      LReadCaret.Read(LValue, 1);
      AStream.Write(LValue, 1);
      Inc(I);
    until I > Size;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKHandleStream.SetSize(const ASize: Int64);
var
  {$IFDEF POSIX}LPosition,{$ENDIF POSIX} LOldSize: Int64;
begin
  AcquireWriteLock;
  try
    LOldSize := GetSize;
    {$IFDEF POSIX}LPosition := {$ENDIF POSIX}FileSeek(FHandle, ASize, Ord(soBeginning));
    {$WARNINGS OFF} // We're handling platform-specifics here... we don't NEED platform warnings!
    {$IF Defined(MSWINDOWS)}
      Win32Check(SetEndOfFile(FHandle));
    {$ELSEIF Defined(POSIX)}
    if ftruncate(FHandle, LPosition) = -1 then
      raise EStreamError(sStreamSetSize);
    {$ELSE}
      {$FATAL 'No implementation for this platform! Please report this issue on https://github.com/LaKraven/LKSL'}
    {$ENDIF}
    {$WARNINGS ON}
    if ASize < LOldSize then
      InvalidateCarets(LOldSize, ASize - LOldSize);
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKFileStream }

constructor TLKFileStream.Create(const AFileName: String; const AMode: Word);
begin
  Create(AFilename, AMode, 0);
end;

constructor TLKFileStream.Create(const AFileName: String; const AMode: Word; const ARights: Cardinal);
var
  LShareMode: Word;
begin
  FAdoptedHandle := False;
  if (AMode and fmCreate = fmCreate) then
  begin
    LShareMode := AMode and $FF;
    if LShareMode = $FF then
      LShareMode := fmShareExclusive;
    inherited Create(FileCreate(AFileName, LShareMode, ARights));
    if FHandle = INVALID_HANDLE_VALUE then
      raise EFCreateError.CreateResFmt(@SFCreateErrorEx, [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end
  else
  begin
    inherited Create(FileOpen(AFileName, AMode));
    if FHandle = INVALID_HANDLE_VALUE then
      raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end;
  FFileName := AFileName;
end;

constructor TLKFileStream.Create(const AStream: TFileStream);
begin
  inherited Create(AStream.Handle);
  FAdoptedHandle := True;
end;

destructor TLKFileStream.Destroy;
begin
  if (FHandle <> INVALID_HANDLE_VALUE) and (not FAdoptedHandle) then
    FileClose(FHandle);
  inherited;
end;

function TLKFileStream.GetCaretType: TLKStreamCaretClass;
begin
  Result := TLKFileStreamCaret;
end;

{ TLKMemoryStreamCaret }

function TLKMemoryStreamCaret.Delete(const ALength: Int64): Int64;
begin
  inherited;
  Result := DeleteActual(ALength);
end;

function TLKMemoryStreamCaret.DeleteActual(const ALength: Int64): Int64;
begin
  AcquireReadLock;
  try
    // Shift elements after Position + Length back to Position
    if FPosition + ALength < TLKMemoryStream(GetStream).FSize then
      System.Move(
                    (PByte(TLKMemoryStream(GetStream).FMemory) + FPosition + ALength)^,
                    (PByte(TLKMemoryStream(GetStream).FMemory) + FPosition)^,
                    TLKMemoryStream(GetStream).FSize - (FPosition + ALength)
                 );
    // Dealloc Memory
    TLKMemoryStream(GetStream).Size := TLKMemoryStream(GetStream).FSize - ALength;
    // Invalidate the Carets representing the Bytes we've deleted
    FStream.InvalidateCarets(FPosition, ALength);
    // Shift subsequent Carets to the left
    FStream.ShiftCaretsLeft(FPosition + ALength, ALength - (FPosition + ALength));
    Result := ALength;
  finally
    ReleaseReadLock;
  end;
end;

function TLKMemoryStreamCaret.Insert(const ABuffer; const ALength: Int64): Int64;
begin
  inherited;
  GetStream.AcquireWriteLock;
  try
    Result := InsertActual(ABuffer, ALength);
  finally
    GetStream.ReleaseWriteLock;
  end;
end;

function TLKMemoryStreamCaret.InsertActual(const ABuffer; const ALength: Int64): Int64;
var
  LStartPosition: Int64;
begin
  AcquireReadLock;
  try
    Result := 0;
    LStartPosition := FPosition;
    FStream.Size := FStream.Size + ALength;
    // Shift subsequent Bytes to the Right
    System.Move(
                  (PByte(TLKMemoryStream(GetStream).FMemory) + FPosition)^,
                  (PByte(TLKMemoryStream(GetStream).FMemory) + FPosition + ALength)^,
                  TLKMemoryStream(GetStream).FSize - (FPosition + ALength)
               );
    // Write the Data
    WriteActual(ABuffer, ALength);
    // Shift subsequent Carets to the Right
    FStream.ShiftCaretsRight(LStartPosition, (FStream.Size - ALength) - LStartPosition);
    Result := ALength;
  finally
    ReleaseReadLock;
  end;
end;

function TLKMemoryStreamCaret.Read(var ABuffer; const ALength: Int64): Int64;
begin
  inherited;
  GetStream.AcquireReadLock;
  try
    Result := ReadActual(ABuffer, ALength);
  finally
    GetStream.ReleaseReadLock;
  end;
end;

function TLKMemoryStreamCaret.ReadActual(var ABuffer; const ALength: Int64): Int64;
begin
  AcquireWriteLock;
  try
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
  finally
    ReleaseWriteLock;
  end;
end;

function TLKMemoryStreamCaret.Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
begin
  inherited;
  GetStream.AcquireReadLock;
  try
    Result := SeekActual(AOffset, AOrigin);
  finally
    GetStream.ReleaseReadLock;
  end;
end;

function TLKMemoryStreamCaret.SeekActual(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
begin
  AcquireWriteLock;
  try
    case AOrigin of
      soBeginning: Result := AOffset;
      soCurrent: Result := FPosition + AOffset;
      soEnd: Result := TLKMemoryStream(FStream).FSize - AOffset;
    else
      Result := FPosition;
    end;
    FPosition := Result;
  finally
    ReleaseWriteLock;
  end;
end;

function TLKMemoryStreamCaret.Write(const ABuffer; const ALength: Int64): Int64;
begin
  inherited;
  GetStream.AcquireWriteLock;
  try
    Result := WriteActual(ABuffer, ALength);
  finally
    GetStream.ReleaseWriteLock;
  end;
end;

function TLKMemoryStreamCaret.WriteActual(const ABuffer; const ALength: Int64): Int64;
var
  LPos: Int64;
begin
  Result := 0;
  AcquireWriteLock;
  try
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
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKMemoryStream }

procedure TLKMemoryStream.Clear;
begin
  SetCapacity(0);
  FSize := 0;
end;

constructor TLKMemoryStream.Create(const AStream: TCustomMemoryStream);
begin
  Create;
  FMemory := AStream.Memory;
end;

constructor TLKMemoryStream.Create;
begin
  FDestroying := False;
  inherited;
  Clear;
end;

destructor TLKMemoryStream.Destroy;
begin
  Clear;
  FDestroying := True; // Mark this Stream as "in destruction"
  inherited;
end;

function TLKMemoryStream.GetCaretType: TLKStreamCaretClass;
begin
  Result := TLKMemoryStreamCaret;
end;

function TLKMemoryStream.GetSize: Int64;
begin
  AcquireReadLock;
  try
    Result := GetSizeActual;
  finally
    ReleaseReadLock;
  end;
end;

function TLKMemoryStream.GetSizeActual: Int64;
begin
  Result := FSize;
end;

procedure TLKMemoryStream.LoadFromFile(const AFileName: String);
var
  LStream: ILKStream;
begin
  LStream := TLKFileStream.Create(AFileName, fmOpenRead);
  LoadFromStream(LStream);
end;

procedure TLKMemoryStream.LoadFromStream(const AStream: ILKStream);
var
  LReadCaret: ILKStreamCaret;
begin
  AcquireWriteLock;
  try
    Size := AStream.Size;
    LReadCaret := AStream.NewCaret;
    LReadCaret.Read(FMemory^, AStream.Size);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKMemoryStream.LoadFromStream(const AStream: TStream);
begin
  AcquireWriteLock;
  try
    AStream.Position := 0;
    Size := AStream.Size;
    AStream.Read(FMemory^, AStream.Size);
  finally
    ReleaseWriteLock;
  end;
end;

function TLKMemoryStream.Realloc(var ACapacity: Int64): Pointer;
const
  MemoryDelta = $2000;
begin
  Result := nil;
  AcquireWriteLock;
  try
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
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKMemoryStream.SaveToFile(const AFileName: String);
var
  LStream: ILKFileStream;
begin
  LStream := TLKFileStream.Create(AFileName, fmCreate);
  SaveToStream(LStream);
end;

procedure TLKMemoryStream.SaveToStream(const AStream: TStream);
begin
  AcquireReadLock;
  try
    if FSize > 0 then
      AStream.WriteBuffer(FMemory^, FSize);
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKMemoryStream.SaveToStream(const AStream: ILKStream);
var
  LCaret: ILKStreamCaret;
begin
  AcquireReadLock;
  try
    if FSize > 0 then
    begin
      LCaret := AStream.NewCaret;
      LCaret.Write(FMemory^, FSize);
    end;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKMemoryStream.SetCapacity(ACapacity: Int64);
begin
  AcquireWriteLock;
  try
    SetPointer(Realloc(ACapacity), FSize);
    FCapacity := ACapacity;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKMemoryStream.SetPointer(const APointer: Pointer; const ASize: Int64);
begin
  AcquireWriteLock;
  try
    FMemory := APointer;
    FSize := ASize;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKMemoryStream.SetSize(const ASize: Int64);
begin
  AcquireWriteLock;
  try
    SetSizeActual(ASize);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKMemoryStream.SetSizeActual(const ASize: Int64);
var
  LOldSize: Longint;
begin
  LOldSize := FSize;
  SetCapacity(ASize);
  FSize := ASize;
  if ASize < LOldSize then
    InvalidateCarets(LOldSize, ASize - LOldSize);
end;

end.
