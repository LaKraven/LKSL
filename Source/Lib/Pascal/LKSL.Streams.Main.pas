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

type
  { Forward Declarations }
  TLKStreamCaret = class;
  TLKStream = class;

  { Class References }
  TLKStreamCaretClass = class of TLKStreamCaret;

  TLKStreamCaretList = class(TLKObjectList<TLKStreamCaret>);

  ///  <summary><c>Abstract Base Class for Stream Reading Carets.</c></summary>
  TLKStreamCaret = class abstract(TLKPersistent)
  private
    FPosition: Int64;
    FStream: TLKStream;
    function GetPosition: Int64;

    procedure SetPosition(const APosition: Int64);
  public
    constructor Create(const AStream: TLKStream); reintroduce;
    destructor Destroy; override;

    property Position: Int64 read GetPosition write SetPosition;

    function Read(var ABuffer; const ALength: Int64): Int64; overload; virtual;
    function Read(const ABuffer: TBytes; const AOffset, ACount: Int64): Int64; overload; virtual;

    function Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
  end;

  ///  <summary><c>Abstract Base Type for all Multi-Thread Access Streams.</c></summary>
  ///  <remarks>
  ///    <para><c>Each consuming Thread provides its own Caret.</c></para>
  ///    <para><c>Locking only occurs to prevent Write-During-Read and Read-During-Write.</c></para>
  ///  </remarks>
  TLKStream = class abstract(TLKPersistent)
  protected
    function GetCaretType: TLKStreamCaretClass; virtual; abstract;
  private
    FCarets: TLKStreamCaretList;

    procedure RegisterCaret(const ACaret: TLKStreamCaret);
    procedure UnregisterCaret(const ACaret: TLKStreamCaret);
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    function NewCaret: TLKStreamCaret;
  end;

  {$IFNDEF FPC}
    StreamManager = class abstract
    public
      class procedure CustomDelete<T>(const AStream: TStream; const ASizeOf: Int64); overload; inline;
      class procedure CustomDelete<T>(const AStream: TStream; const APosition: Int64; const ASizeOf: Int64); overload; inline;
      class procedure CustomInsert<T>(const AStream: TStream; const AValue: T; const ASizeOf: Int64); overload; inline;
      class procedure CustomInsert<T>(const AStream: TStream; const AValue: T; const APosition: Int64; const ASizeOf: Int64); overload; inline;
      class function CustomRead<T>(const AStream: TStream; const ASizeOf: Int64): T; overload; inline;
      class function CustomRead<T>(const AStream: TStream; const APosition: Int64; const ASizeOf: Int64): T; overload; inline;
      class procedure CustomWrite<T>(const AStream: TStream; const AValue: T; const ASizeOf: Int64); overload; inline;
      class procedure CustomWrite<T>(const AStream: TStream; const AValue: T; const APosition: Int64; const ASizeOf: Int64); overload; inline;

      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure Delete<T>(const AStream: TStream); overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure Delete<T>(const AStream: TStream; const APosition: Int64); overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure DeleteArray<T>(const AStream: TStream); overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure DeleteArray<T>(const AStream: TStream; const APosition: Int64); overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure Insert<T>(const AStream: TStream; const AValue: T); overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure Insert<T>(const AStream: TStream; const AValue: T; const APosition: Int64); overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure InsertArray<T>(const AStream: TStream; const AValues: Array of T); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure InsertArray<T>(const AStream: TStream; const AValues: Array of T; const APosition: Int64); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class function Read<T>(const AStream: TStream): T; overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class function Read<T>(const AStream: TStream; const APosition: Int64): T; overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class function ReadArray<T>(const AStream: TStream): TArray<T>; overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class function ReadArray<T>(const AStream: TStream; const APosition: Int64): TArray<T>; overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure Write<T>(const AStream: TStream; const AValue: T); overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure Write<T>(const AStream: TStream; const AValue: T; const APosition: Int64); overload; inline;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure WriteArray<T>(const AStream: TStream; const AValues: TArray<T>); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      class procedure WriteArray<T>(const AStream: TStream; const AValues: TArray<T>; const APosition: Int64); overload;
    end;
  {$ENDIF FPC}

  {$IFDEF LKSL_USE_HELPERS}
    TLKStreamHelper = class helper for TStream
    public
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure DeleteValue<T>; overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure DeleteValue<T>(const APosition: Int64); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure DeleteArray<T>; overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure DeleteArray<T>(const APosition: Int64); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure InsertValue<T>(const AValue: T); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure InsertValue<T>(const AValue: T; const APosition: Int64); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure InsertArray<T>(const AValues: Array of T); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure InsertArray<T>(const AValues: Array of T; const APosition: Int64); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      function ReadValue<T>: T; overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      function ReadValue<T>(const APosition: Int64): T; overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      function ReadArray<T>: TArray<T>; overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      function ReadArray<T>(const APosition: Int64): TArray<T>; overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure WriteValue<T>(const AValue: T); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure WriteValue<T>(const AValue: T; const APosition: Int64); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure WriteArray<T>(const AValues: TArray<T>); overload;
      ///  <remarks><c>Do not use this for Dynamic Length Types such as </c>String</remarks>
      procedure WriteArray<T>(const AValues: TArray<T>; const APosition: Int64); overload;
    end;
  {$ENDIF}

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

function TLKStreamCaret.Read(const ABuffer: TBytes; const AOffset, ACount: Int64): Int64;
begin
  { TODO -oSJS -cTLKStreamCaret: Implement Read from Buffer}
end;

function TLKStreamCaret.Read(var ABuffer; const ALength: Int64): Int64;
begin
  Result := 0;
end;

function TLKStreamCaret.Seek(const AOffset: Int64; const AOrigin: TSeekOrigin): Int64;
begin
  { TODO -oSJS -cTLKStreamCaret: Implement Seek (Relative)}
end;

procedure TLKStreamCaret.SetPosition(const APosition: Int64);
begin
  Seek(APosition, soBeginning);
end;

{ TLKStream }

constructor TLKStream.Create;
begin
  inherited;
  FCarets := TLKStreamCaretList.Create(True);
end;

destructor TLKStream.Destroy;
begin
  FCarets.Free;
  inherited;
end;

function TLKStream.NewCaret: TLKStreamCaret;
begin
  Result := GetCaretType.Create(Self);
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

{$IFNDEF FPC}
  { StreamManager }
  class procedure StreamManager.Delete<T>(const AStream: TStream);
  begin
    Delete<T>(AStream, AStream.Position);
  end;

  class procedure StreamManager.CustomDelete<T>(const AStream: TStream; const ASizeOf: Int64);
  begin
    StreamClearSpace(AStream, ASizeOf);
  end;

  class procedure StreamManager.CustomDelete<T>(const AStream: TStream; const APosition, ASizeOf: Int64);
  begin
    AStream.Position := APosition;
    CustomDelete<T>(AStream, ASizeOf);
  end;

  class procedure StreamManager.CustomInsert<T>(const AStream: TStream; const AValue: T; const ASizeOf: Int64);
  begin
    StreamMakeSpace(AStream, ASizeOf);
    AStream.Write(AValue, ASizeOf);
  end;

  class procedure StreamManager.CustomInsert<T>(const AStream: TStream; const AValue: T; const APosition, ASizeOf: Int64);
  begin
    AStream.Position := APosition;
    CustomInsert<T>(AStream, AValue, ASizeOf);
  end;

  class function StreamManager.CustomRead<T>(const AStream: TStream; const ASizeOf: Int64): T;
  begin
    AStream.Read(Result, ASizeOf);
  end;

  class function StreamManager.CustomRead<T>(const AStream: TStream; const APosition, ASizeOf: Int64): T;
  begin
    AStream.Position := APosition;
    CustomRead<T>(AStream, ASizeOf);
  end;

  class procedure StreamManager.CustomWrite<T>(const AStream: TStream; const AValue: T; const ASizeOf: Int64);
  begin
    CustomWrite<T>(AStream, AValue, AStream.Size);
  end;

  class procedure StreamManager.CustomWrite<T>(const AStream: TStream; const AValue: T; const APosition, ASizeOf: Int64);
  begin
    AStream.Position := APosition;
    AStream.Write(AValue, ASizeOf);
  end;

  class procedure StreamManager.Delete<T>(const AStream: TStream; const APosition: Int64);
  begin
    StreamClearSpace(AStream, APosition, SizeOf(T));
  end;

  class procedure StreamManager.DeleteArray<T>(const AStream: TStream);
  var
    LStartPos: Int64;
    LLength, I: Integer;
  begin
    LStartPos := AStream.Position;
    LLength := Read<Integer>(AStream);
    AStream.Position := LStartPos;
    Delete<Integer>(AStream);
    for I := 0 to LLength - 1 do
      Delete<T>(AStream);
  end;

  class procedure StreamManager.DeleteArray<T>(const AStream: TStream; const APosition: Int64);
  begin
    AStream.Position := APosition;
    DeleteArray<T>(AStream);
  end;

  class procedure StreamManager.Insert<T>(const AStream: TStream; const AValue: T);
  begin
    StreamMakeSpace(AStream, SizeOf(T));
    AStream.Write(AValue, SizeOf(T));
  end;

  class procedure StreamManager.Insert<T>(const AStream: TStream; const AValue: T; const APosition: Int64);
  begin
    AStream.Position := APosition;
    Insert<T>(AStream, AValue);
  end;

  class procedure StreamManager.InsertArray<T>(const AStream: TStream; const AValues: array of T; const APosition: Int64);
  begin
    AStream.Position := APosition;
    InsertArray<T>(AStream, AValues);
  end;

  class procedure StreamManager.InsertArray<T>(const AStream: TStream; const AValues: array of T);
  var
    I: Integer;
  begin
    Insert<Integer>(AStream, Length(AValues)); // Prefix Array Length
    for I := Low(AValues) to High(AValues) do // Insert Values in Sequence
      Insert<T>(AStream, AValues[I]);
  end;

  class function StreamManager.Read<T>(const AStream: TStream): T;
  begin
    AStream.Read(Result, SizeOf(T));
  end;

  class function StreamManager.Read<T>(const AStream: TStream; const APosition: Int64): T;
  begin
    AStream.Position := APosition;
    Result := Read<T>(AStream);
  end;

  class function StreamManager.ReadArray<T>(const AStream: TStream; const APosition: Int64): TArray<T>;
  begin
    AStream.Position := APosition;
    Result := ReadArray<T>(AStream);
  end;

  class function StreamManager.ReadArray<T>(const AStream: TStream): TArray<T>;
  var
    LLength, I: Integer;
  begin
    LLength := StreamManager.Read<Integer>(AStream);
    SetLength(Result, LLength);
    for I := 0 to LLength - 1 do
      Result[I] := StreamManager.Read<T>(AStream);
  end;

  class procedure StreamManager.Write<T>(const AStream: TStream; const AValue: T);
  begin
    Write<T>(AStream, AValue, AStream.Size);
  end;

  class procedure StreamManager.Write<T>(const AStream: TStream; const AValue: T; const APosition: Int64);
  begin
    AStream.Position := APosition;
    AStream.Write(AValue, SizeOf(T));
  end;

  class procedure StreamManager.WriteArray<T>(const AStream: TStream; const AValues: TArray<T>);
  var
    I: Integer;
  begin
    Write<Integer>(AStream, Length(AValues)); // Prefix Array Length
    for I := Low(AValues) to High(AValues) do // Write Values in Sequence
      Write<T>(AStream, AValues[I]);
  end;

  class procedure StreamManager.WriteArray<T>(const AStream: TStream; const AValues: TArray<T>; const APosition: Int64);
  begin
    AStream.Position := APosition;
    WriteArray<T>(AStream, AValues);
  end;
{$ENDIF FPC}

{$IFDEF LKSL_USE_HELPERS}

  { TLKStreamHelper }
  procedure TLKStreamHelper.DeleteValue<T>;
  begin
    StreamManager.Delete<T>(Self);
  end;

  procedure TLKStreamHelper.DeleteValue<T>(const APosition: Int64);
  begin
    StreamManager.Delete<T>(Self, APosition);
  end;

  procedure TLKStreamHelper.DeleteArray<T>;
  begin
    StreamManager.DeleteArray<T>(Self);
  end;

  procedure TLKStreamHelper.DeleteArray<T>(const APosition: Int64);
  begin
    StreamManager.DeleteArray<T>(Self, APosition);
  end;

  procedure TLKStreamHelper.InsertValue<T>(const AValue: T);
  begin
    StreamManager.Insert<T>(Self, AValue);
  end;

  procedure TLKStreamHelper.InsertValue<T>(const AValue: T; const APosition: Int64);
  begin
    StreamManager.Insert<T>(Self, AValue, APosition);
  end;

  procedure TLKStreamHelper.InsertArray<T>(const AValues: Array of T);
  begin
    StreamManager.InsertArray<T>(Self, AValues);
  end;

  procedure TLKStreamHelper.InsertArray<T>(const AValues: Array of T; const APosition: Int64);
  begin
    StreamManager.InsertArray<T>(Self, AValues, APosition);
  end;

  function TLKStreamHelper.ReadValue<T>: T;
  begin
    Result := StreamManager.Read<T>(Self);
  end;

  function TLKStreamHelper.ReadValue<T>(const APosition: Int64): T;
  begin
    Result := StreamManager.Read<T>(Self, APosition);
  end;

  function TLKStreamHelper.ReadArray<T>: TArray<T>;
  begin
    Result := StreamManager.ReadArray<T>(Self);
  end;

  function TLKStreamHelper.ReadArray<T>(const APosition: Int64): TArray<T>;
  begin
    Result := StreamManager.ReadArray<T>(Self, APosition);
  end;

  procedure TLKStreamHelper.WriteValue<T>(const AValue: T);
  begin
    StreamManager.Write<T>(Self, AValue);
  end;

  procedure TLKStreamHelper.WriteValue<T>(const AValue: T; const APosition: Int64);
  begin
    StreamManager.Write<T>(Self, AValue, APosition);
  end;

  procedure TLKStreamHelper.WriteArray<T>(const AValues: TArray<T>);
  begin
    StreamManager.WriteArray<T>(Self, AValues);
  end;

  procedure TLKStreamHelper.WriteArray<T>(const AValues: TArray<T>; const APosition: Int64);
  begin
    StreamManager.WriteArray<T>(Self, AValues, APosition);
  end;

{$ENDIF LKSL_USE_HELPERS}

end.
