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
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types,
  LKSL.Generics.Collections;

  {$I LKSL_RTTI.inc}

type
  { Interface Forward Declarations }
  ILKStreamCaret = interface;
  ILKStream = interface;
  ILKMemoryStreamCaret = interface;
  ILKMemoryStream  = interface;
  { Class Forward Declarations }
  TLKStreamCaret = class;
  TLKStream = class;
  TLKMemoryStreamCaret = class;
  TLKMemoryStream = class;

  { Class References }
  TLKStreamCaretClass = class of TLKStreamCaret;

  { Generics Collections }
  TLKStreamCaretList = class(TLKList<TLKStreamCaret>);

  ILKStreamCaret = interface
  ['{D8E849E5-A5A1-4B4F-9AF6-BBD397216C5B}']
    function GetPosition: Int64;
    procedure SetPosition(const APosition: Int64);

    ///  <summary><c>Deletes the given number of Bytes from the current Position in the Stream, then compacts the Stream by that number of Bytes (shifting any subsequent Bytes to the left)</c></summary>
    ///  <returns><c>Returns the new </c>Size<c> of the Stream.</c></returns>
    function Delete(const ALength: Int64): Int64;

    ///  <summary><c>Inserts the given Buffer into the current Position within the Stream (shifting any subsequent Bytes to the right)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    function Insert(const ABuffer; const ALength: Int64): Int64; overload;
    ///  <summary><c>Inserts the given Buffer into the current Position within the Stream (shifting any subsequent Bytes to the right)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    function Insert(const ABuffer: TBytes; const ALength: Int64): Int64; overload;

    ///  <summary><c>Reads the specified number of Bytes from the Array into the specified Address</c></summary>
    ///  <returns><c>Returns the number of Bytes actually read.</c></returns>
    function Read(var ABuffer; const ALength: Int64): Int64; overload;
    ///  <summary><c>Reads the specified number of Bytes from the Array into the specified Address</c></summary>
    ///  <returns><c>Returns the number of Bytes actually read.</c></returns>
    function Read(const ABuffer: TBytes; const AOffset, ACount: Int64): Int64; overload;

    ///  <summary><c>Writes the given Buffer into the current Position within the Stream (overwriting any existing data, and expanding the Size of the Stream if required)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    function Write(const ABuffer; const ALength: Int64): Int64; overload;
    ///  <summary><c>Writes the given Buffer into the current Position within the Stream (overwriting any existing data, and expanding the Size of the Stream if required)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    function Write(const ABuffer: TBytes; const ALength: Int64): Int64; overload;

    ///  <returns><c>Returns the new </c>Position<c> in the Stream.</c></returns>
    function Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;

    property Position: Int64 read GetPosition write SetPosition;
  end;

  ILKStream = interface
  ['{07F45B12-1DFC-453A-B95C-E00C9F5F4285}']
    function GetSize: Int64;
    procedure SetSize(const ASize: Int64);

    function NewCaret: ILKStreamCaret; overload;
    function NewCaret(const APosition: Int64): ILKStreamCaret; overload;

    property Size: Int64 read GetSize write SetSize;
  end;

  ILKMemoryStreamCaret = interface
  ['{1FB5A4F9-8FFB-4A79-B364-01D6E428A718}']

  end;

  ILKMemoryStream  = interface
  ['{289F1193-AE69-47D6-B66B-0174070963B5}']

  end;

  ///  <summary><c>Abstract Base Class for Stream Reading Carets.</c></summary>
  TLKStreamCaret = class abstract(TLKInterfacedObject, ILKStreamCaret)
  private
    FPosition: Int64;
    FStream: TLKStream;
    // Getters
    function GetPosition: Int64;
    // Setters
    procedure SetPosition(const APosition: Int64);
  protected
    function SeekBeginning(const AOffset: Int64): Int64; virtual; abstract;
    function SeekCurrent(const AOffset: Int64): Int64; virtual; abstract;
    function SeekEnd(const AOffset: Int64): Int64; virtual; abstract;
  public
    constructor Create(const AStream: TLKStream); reintroduce;
    destructor Destroy; override;

    ///  <summary><c>Deletes the given number of Bytes from the current Position in the Stream, then compacts the Stream by that number of Bytes (shifting any subsequent Bytes to the left)</c></summary>
    ///  <returns><c>Returns the new </c>Size<c> of the Stream.</c></returns>
    ///  <remarks>
    ///    <para><c></c></para>
    ///  </remarks>
    function Delete(const ALength: Int64): Int64; virtual; abstract;

    ///  <summary><c>Inserts the given Buffer into the current Position within the Stream (shifting any subsequent Bytes to the right)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    function Insert(const ABuffer; const ALength: Int64): Int64; overload; virtual; abstract;
    ///  <summary><c>Inserts the given Buffer into the current Position within the Stream (shifting any subsequent Bytes to the right)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    function Insert(const ABuffer: TBytes; const ALength: Int64): Int64; overload; virtual; abstract;

    ///  <summary><c>Reads the specified number of Bytes from the Array into the specified Address</c></summary>
    ///  <returns><c>Returns the number of Bytes actually read.</c></returns>
    function Read(var ABuffer; const ALength: Int64): Int64; overload; virtual; abstract;
    ///  <summary><c>Reads the specified number of Bytes from the Array into the specified Address</c></summary>
    ///  <returns><c>Returns the number of Bytes actually read.</c></returns>
    function Read(const ABuffer: TBytes; const AOffset, ACount: Int64): Int64; overload; virtual; abstract;

    ///  <summary><c>Writes the given Buffer into the current Position within the Stream (overwriting any existing data, and expanding the Size of the Stream if required)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    function Write(const ABuffer; const ALength: Int64): Int64; overload; virtual; abstract;
    ///  <summary><c>Writes the given Buffer into the current Position within the Stream (overwriting any existing data, and expanding the Size of the Stream if required)</c></summary>
    ///  <returns><c>Returns the number of Bytes actually written.</c></returns>
    function Write(const ABuffer: TBytes; const ALength: Int64): Int64; overload; virtual; abstract;

    ///  <returns><c>Returns the new </c>Position<c> in the Stream.</c></returns>
    function Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64; inline;

    property Position: Int64 read GetPosition write SetPosition;
  end;

  ///  <summary><c>Abstract Base Type for all Multi-Thread Access Streams.</c></summary>
  ///  <remarks>
  ///    <para><c>Each consuming Thread provides its own Caret.</c></para>
  ///    <para><c>Locking only occurs to prevent Write-During-Read and Read-During-Write.</c></para>
  ///  </remarks>
  TLKStream = class abstract(TLKInterfacedObject, ILKStream)
  private
    FCarets: TLKStreamCaretList;
    procedure RegisterCaret(const ACaret: TLKStreamCaret);
    procedure UnregisterCaret(const ACaret: TLKStreamCaret);
  protected
    ///  <summary>Override to define the correct Stream Caret Type to use for this Stream</summary>
    function GetCaretType: TLKStreamCaretClass; virtual; abstract;

    function GetSize: Int64; virtual; abstract;
    procedure SetSize(const ASize: Int64); virtual; abstract;
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    function NewCaret: ILKStreamCaret; overload;
    function NewCaret(const APosition: Int64): ILKStreamCaret; overload;

    property Size: Int64 read GetSize write SetSize;
  end;

  ///  <summary><c>Caret specifically set up for Memory Streams.</c></summary>
  TLKMemoryStreamCaret = class(TLKStreamCaret, ILKMemoryStreamCaret)

  end;

  ///  <summary><c>Special Memory Stream built for Multi-Thread Asynchronous (and Random) Access</c></summary>
  TLKMemoryStream = class(TLKStream, ILKMemoryStream)
  private
    FBuffer: TBytes;
  protected
    function GetCaretType: TLKStreamCaretClass; override;
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
  FPosition := 0;
  FStream.RegisterCaret(Self);
end;

destructor TLKStreamCaret.Destroy;
begin
  FStream.UnregisterCaret(Self);
  inherited;
end;

function TLKStreamCaret.GetPosition: Int64;
begin
  Lock;
  try
    Result := FPosition;
  finally
    Unlock;
  end;
end;

function TLKStreamCaret.Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
begin
  case AOrigin of
    soBeginning: Result := SeekBeginning(AOffset);
    soCurrent: Result := SeekCurrent(AOffset);
    soEnd: Result := SeekEnd(AOffset);
    else
      Result := 0;
  end;
end;

procedure TLKStreamCaret.SetPosition(const APosition: Int64);
begin
  SeekBeginning(APosition);
end;

{ TLKStream }

constructor TLKStream.Create;
begin
  inherited;
  FCarets := TLKStreamCaretList.Create;
end;

destructor TLKStream.Destroy;
begin
  FCarets.Free;
  inherited;
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

{ TLKMemoryStream }

function TLKMemoryStream.GetCaretType: TLKStreamCaretClass;
begin
  Result := TLKMemoryStreamCaret;
end;

end.
