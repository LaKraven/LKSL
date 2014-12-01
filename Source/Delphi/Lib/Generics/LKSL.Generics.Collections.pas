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
unit LKSL.Generics.Collections;

interface

{$I LKSL.inc}

{
  About this unit:
    - This unit provides useful enhancements for Generics types used in the LKSL.

    30th November 2014:
      - Prepared for Release
}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  Generics.Collections,
  LKSL.Common.Types;

type
  { Forward Declaration }
  TLKArray<T> = class;
  TLKDictionary<TKey, TValue> = class;
  TLKList<T> = class;
  TLKObjectList<T: class> = class;

  {
    TLKArray<T>
      - Provides a Thread-Safe Lock (TCriticalSection)
      - A very simple "Managed Array" for items of the nominated type.
      - Items are UNSORTED
      - The Array expands by 1 each time a new item is added
      - The Array collapses by 1 each time an item is removed
      - If referencing the "ArrayRaw" property, don't forget to call "Lock" and "Unlock" manually!
  }
  TLKArray<T> = class(TLKPersistent)
  type
    TLKArrayType = Array of T;
  private
    FArray: TLKArrayType;
    FLock: TCriticalSection;

    // Getters
    function GetItem(const AIndex: Integer): T;

    // Setters
    procedure SetItem(const AIndex: Integer; const AItem: T);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;

    function Add(const AItem: T): Integer;
    procedure Clear; inline;
    procedure Delete(const AIndex: Integer); virtual;

    property ArrayRaw: TLKArrayType read FArray;
    property Items[const AIndex: Integer]: T read GetItem write SetItem;
  end;

  {
    TLKDictionary<TKey, TValue>
      - Provides a Thread-Safe Lock (TCriticalSection)
  }
  TLKDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
  private
    FLock: TCriticalSection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
  end;

  {
    TLKList<T>
      - Provides a Thread-Safe Lock (TCriticalSection)
  }
  TLKList<T> = class(TList<T>)
  private
    FLock: TCriticalSection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
  end;

  {
    TLKObjectList<T: class>
      - Provides a Thread-Safe Lock (TCriticalSection)
  }
  TLKObjectList<T: class> = class(TObjectList<T>)
  private
    FLock: TCriticalSection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
  end;

implementation

{ TLKArray<T> }

function TLKArray<T>.Add(const AItem: T): Integer;
begin
  Result := Length(FArray);
  SetLength(FArray, Result + 1);
  FArray[Result] := AItem;
end;

procedure TLKArray<T>.Clear;
begin
  SetLength(FArray, 0);
end;

constructor TLKArray<T>.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
end;

procedure TLKArray<T>.Delete(const AIndex: Integer);
var
  I: Integer;
begin
  Lock;
  try
    for I := AIndex to Length(FArray) - 2 do
      FArray[I] := FArray[I + 1];

    SetLength(FArray, Length(FArray) - 1);
  finally
    Unlock;
  end;
end;

destructor TLKArray<T>.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TLKArray<T>.GetItem(const AIndex: Integer): T;
begin
  Lock;
  try
    Result := FArray[AIndex];
  finally
    Unlock;
  end;
end;

procedure TLKArray<T>.Lock;
begin
  FLock.Acquire;
end;

procedure TLKArray<T>.SetItem(const AIndex: Integer; const AItem: T);
begin
  Lock;
  try
    FArray[AIndex] := AItem;
  finally
    Unlock;
  end;
end;

procedure TLKArray<T>.Unlock;
begin
  FLock.Release;
end;

{ TLKDictionary<TKey, TValue> }

constructor TLKDictionary<TKey, TValue>.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
end;

destructor TLKDictionary<TKey, TValue>.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKDictionary<TKey, TValue>.Lock;
begin
  FLock.Acquire;
end;

procedure TLKDictionary<TKey, TValue>.Unlock;
begin
  FLock.Release;
end;

{ TLKList<T> }

constructor TLKList<T>.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TLKList<T>.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKList<T>.Lock;
begin
  FLock.Acquire;
end;

procedure TLKList<T>.Unlock;
begin
  FLock.Release;
end;

{ TLKObjectList<T> }

constructor TLKObjectList<T>.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TLKObjectList<T>.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKObjectList<T>.Lock;
begin
  FLock.Acquire;
end;

procedure TLKObjectList<T>.Unlock;
begin
  FLock.Release;
end;

end.
