{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014-2015, Simon J Stuart, All Rights Reserved

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
unit LKSL.Streamables.Main;

interface

{$I LKSL.inc}

{$IFDEF FPC}
  {$IFDEF LKSL_MODE_FPC}
    {$mode objfpc}{$H+}
  {$ELSE}
    {$mode delphi}
  {$ENDIF LKSL_MODE_FPC}
{$ENDIF FPC}

{$REGION 'Unit About'}
  {
    About this unit:
      - This unit is the heart and soul of the "Streamables Engine".
      - This unit MUST be referenced in the "Uses" section of any units intending to use Streamable Types.
      - DON'T FORGET TO CALL "Streamables.Register(<type>);" OR "Streamables.Register([<types]);"
        BEFORE USING ANY OF YOUR STREAMABLE TYPES! THIS CAN BE EASILY DONE IN THE "INITIALIZATION" SECTION.
        YOU CAN THEN CALL "Streamables.Unregister(<type);" OR "Streamables.Unregister([<types]);"
        IN THE "FINALIZATION" SECTION OF YOUR DEFINING UNITS!
  }
{$ENDREGION}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  {$IFNDEF FPC}Generics.Defaults,{$ENDIF FPC}
  LKSL.Generics.Collections,
  LKSL.Common.Types,
  LKSL.Streams.Main, LKSL.Streams.System;

  {$I LKSL_RTTI.inc}

type
  { Forward Declarations }
  TLKStreamable = class;
  TLKStreamableNamed = class;
  TLKStreamables = class;

  { Exceptions }
  // "ELKStreamableException" is the Base Exception Type for ALL Streamables Exceptions
  ELKStreamableException = class(ELKException);
  // "ELKStreamableSignatureMismatch" occurs when a Streamable is being Read from a Stream if the
  // Signatures don't match!
  ELKStreamableSignatureMismatch = class(ELKStreamableException);
  // "ELKStreamableTypesException" is the Base Exception Type for ALL Streamables Types Exceptions
  ELKStreamableTypesException = class(ELKException);
  // "ELKStreamableTypeAlreadyRegistered" occurs when you attempt to register a Streamable Type where
  // a Streamable Type with that GUID has already been registered.
  ELKStreamableTypeAlreadyRegistered = class(ELKStreamableTypesException);
  // "ELKStreamableTypeNotRegistered" occurs when you attempt to UNregister a Streamable Type which has
  // not yet been registered with the Streamable Types Manager.
  ELKStreamableTypeNotRegistered = class(ELKStreamableTypesException);

  { Class References }
  TLKStreamableType = class of TLKStreamable;

  { Array Types }
  TLKStreamableArray = TArray<TLKStreamable>;

  { Generics Collections }
  {$IFDEF LKSL_EVENTENGINE_USEBINARYLISTS}
    TLKStreamableTypesSortedList = class(TLKSortedList<TLKStreamableType>)
    protected
      function AEqualToB(const A, B: TLKStreamableType): Boolean; override;
      function AGreaterThanB(const A, B: TLKStreamableType): Boolean; override;
      function AGreaterThanOrEqualB(const A, B: TLKStreamableType): Boolean; override;
      function ALessThanB(const A, B: TLKStreamableType): Boolean; override;
      function ALessThanOrEqualB(const A, B: TLKStreamableType): Boolean; override;
    end;
  {$ELSE}
    TLKStreamableTypesDictionary = class(TLKDictionary<TGUID, TLKStreamableType>);
  {$ENDIF LKSL_EVENTENGINE_USEBINARYLISTS}

  {
    TLKStreamable
      - Abstract Base Type for all Streamable Types
  }
  TLKStreamable = class abstract(TLKPersistent)
  private
    FBlockSize: Int64;
    FVersion: Double;
  protected
    class procedure OnRegistration; virtual;
    class procedure OnUnregistration; virtual;
    // "ReadFromStream" reads an instance of your Streamable Type from the given Stream.
    // NOTE: Don't forget to set the starting Position of the Streamable Instance within your Stream first!
    // NOTE: The object's thread-safe LOCK is engaged for this call! No need to call "Lock" in your implementation!
    procedure ReadFromStream(const ACaret: ILKStreamCaret); virtual; abstract;
    // "InjectIntoStream" inserts an instance of your Streamable Type into the given Stream.
    // NOTE: The object's thread-safe LOCK is engaged for this call! No need to call "Lock" in your implementation!
    procedure InsertIntoStream(const ACaret: ILKStreamCaret); virtual; abstract;
    // "WriteToStream" writes an instance of your Streamable Type to the END of the given Stream.
    // NOTE: The object's thread-safe LOCK is engaged for this call! No need to call "Lock" in your implementation!
    procedure WriteToStream(const ACaret: ILKStreamCaret); virtual; abstract;
  public
    // Creates a BLANK instance of your Streamable
    constructor Create; override;
    // Creates a POPULATED instance of your Streamable from a Stream
    constructor CreateFromStream(const ACaret: ILKStreamCaret); overload; virtual;
    constructor CreateFromStream(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
    constructor CreateFromStream(const AStream: ILKStream); overload;
    constructor CreateFromStream(const AStream: ILKStream; const APosition: Int64); overload;
    // Creates a POPULATED instance of your Streamable from a File
    constructor CreateFromFile(const AFileName: String); virtual;
    // "GetBlockSize" returns the Block Size for a given Streamable from within a given Stream.
    // It does this without
    // You MUST provide a UNIQUE GUID String identifier for ALL Streamable Types
    // This GUID is used to uniquely identify a Streamable Type when Reading back Streamables from a Stream.
    // Override "GetTypeGUID" and return a GUID String.
    // TIP: Use Ctrl+Shift+G to generate a GUID, then delete the opening "[" and closing "]".
    class function GetTypeGUID: TGUID; virtual; abstract;
    // "GetTypeVersion" returns a floating point number used to identify a Version of a Streamable Type.
    // By default, it returns a "0", to assume that your type is the FIRST version of its Type.
    // Version numbers can be used to ensure backward-compatibility with earlier serializations of your
    // Streamable Type. You can use an "IF" statement on new additions or changes to ensure that those
    // changed valuesets are only considered if the serialization matches a given version.
    class function GetTypeVersion: Double; virtual;
    class function CheckTypeInStream(const ACaret: ILKStreamCaret): Boolean; overload;
    class function CheckTypeInStream(const ACaret: ILKStreamCaret; const APosition: Int64): Boolean; overload;
    // "Register" will register this Streamable Type with the "Streamable Types Manager"
    // This means you can request the appropriate Streamable Type using its Type GUID String from a
    // Stream.
    // NOTE: You should ALWAYS Register TLKStreamable descendants (but NOT Abstract descendants).
    //       You can call "Streamables.Register" and pass an ARRAY of TLKStreamable descendants too!
    class procedure Register;
    class procedure Unregister;
    // "DeleteFromStream" removes an instance of your Streamable Type from the given Stream.
    class procedure DeleteFromStream(const ACaret: ILKStreamCaret); overload;
    class procedure DeleteFromStream(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
    // "LoadFromFile" sets the Member Data of your Streamable Instance from a saved File
    procedure LoadFromFile(const AFileName: String);
    // "LoadFromStream" sets the Member Data of your Streamable Instance from a Stream
    // NOTE: Don't forget to set the starting Position of the Streamable Instance within your Stream first!
    procedure LoadFromStream(const ACaret: ILKStreamCaret); overload;
    procedure LoadFromStream(const ACaret: ILKStreamCaret; const APosition: Int64); overload; inline;
    // "SaveToFile" saves the Member Data of your Streamable Instance to a File
    procedure SaveToFile(const AFileName: String);
    // "SaveToStream" WRITES the Member Data of your Streamable Instance to the END of the given Stream.
    procedure SaveToStream(const ACaret: ILKStreamCaret); overload;
    // "SaveToStream" WRITES the Member Data of your Streamable Instance over the data from the nominated
    // Position in the given Stream.
    procedure SaveToStream(const ACaret: ILKStreamCaret; const APosition: Int64); overload;

    property TypeGUID: TGUID read GetTypeGUID;
    property Version: Double read FVersion;
  end;

  {
    TLKStreamableNamed
      - Abstract Type for all NAMED Streamable Types
      - Basically it adds a "Name" property and handles the Deleting, Inserting, Reading and Writing of
        that name to/from Streams.
  }
  TLKStreamableNamed = class abstract(TLKStreamable)
  private
    FName: String;
    function GetName: String;
    procedure SetName(const AName: String);
  protected
    procedure ReadFromStream(const ACaret: ILKStreamCaret); override;
    procedure InsertIntoStream(const ACaret: ILKStreamCaret); override;
    procedure WriteToStream(const ACaret: ILKStreamCaret); override;
  public
    property Name: String read GetName write SetName;
  end;

  ///  <summary><c>Specialized Streamable Type List</c></summary>
  ///  <remarks>
  ///    <para><c>Provides methods to Load and Save to/from Files and Streams (respectively)</c></para>
  ///  </remarks>
  TLKStreamableList = class(TLKObjectList<TLKStreamable>)
  private type
    TLKStreamListHeader = class(TLKStreamable)
    private
      FCount: Integer;
      function GetCount: Integer;
      procedure SetCount(const ACount: Integer);
    protected
      procedure ReadFromStream(const ACaret: ILKStreamCaret); override;
      procedure InsertIntoStream(const ACaret: ILKStreamCaret); override;
      procedure WriteToStream(const ACaret: ILKStreamCaret); override;
    public
      class function GetTypeGUID: TGUID; override;

      property Count: Integer read GetCount write SetCount;
    end;
  public
    procedure LoadFromFile(const AFileName: String); inline;
    procedure LoadFromStream(const AStream: ILKStream); overload; inline;
    procedure LoadFromStream(const ACaret: ILKStreamCaret); overload;

    procedure SaveToFile(const AFileName: String); inline;
    procedure SaveToStream(const AStream: ILKStream); overload; inline;
    procedure SaveToStream(const ACaret: ILKStreamCaret); overload;
  end;

  {
    TLKStreamables
      - The "Streamable Types Manager"
      - All Streamable Types should be Registered against the "Streamables" Manager so that they can be
        requested when reading a serialization of a Streamable Instance from a Stream.
      - This Manager allows you to request the Class Type Reference of a Streamable from within a Stream
        so that it can be constructed dynamically at runtime.
        This means that you can store arbitrary Streamable Type Instances within a Stream, and the correct
        Class Instance for that Streamable Type will be constructed when you're loading from a Stream.

      THIS STREAMABLES MANAGER IS 100% THREAD-SAFE!
  }
  TLKStreamables =  class(TLKPersistent)
  private
    {$IFDEF LKSL_EVENTENGINE_USEBINARYLISTS}
      FStreamableTypes: TLKStreamableTypesSortedList;
    {$ELSE}
      FStreamableTypes: TLKStreamableTypesDictionary;
    {$ENDIF LKSL_EVENTENGINE_USEBINARYLISTS}

    procedure Clear;
    function GetCount: Integer;
//    function GetStreamableTypeByIndex(const AIndex: Integer): TLKStreamableType;
    function GetStreamableTypeByGUID(const AGUID: TGUID): TLKStreamableType; overload;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Register(const AStreamableType: TLKStreamableType); overload;
    procedure Register(const AStreamableTypes: Array of TLKStreamableType); overload;
    procedure Unregister(const AStreamableType: TLKStreamableType); overload;
    procedure Unregister(const AStreamableTypes: Array of TLKStreamableType); overload;

    function CreateStreamableFromStream(const ACaret: ILKStreamCaret): TLKStreamable; overload;
    function CreateStreamableFromStream(const ACaret: ILKStreamCaret; const APosition: Int64): TLKStreamable; overload;
    function GetStreamableTypeFromStream(const ACaret: ILKStreamCaret): TLKStreamableType;
    function StreamableTypeMatch(const ACaret: ILKStreamCaret; const AStreamableType: TLKStreamableType): Boolean; overload;
    function StreamableTypeMatch(const ACaret: ILKStreamCaret; const AStreamableType: TLKStreamableType; const APosition: Int64): Boolean; overload;

    procedure DeleteArrayOfStreamables(const ACaret: ILKStreamCaret);
    procedure InsertArrayOfStreamables(const ACaret: ILKStreamCaret; const AStreamables: TLKStreamableArray);
    procedure WriteArrayOfStreamables(const ACaret: ILKStreamCaret; const AStreamables: TLKStreamableArray);

    property Count: Integer read GetCount;
//    property StreamableTypeByIndex[const AIndex: Integer]: TLKStreamableType read GetStreamableTypeByIndex;
    property StreamableTypeByGUID[const AGUID: TGUID]: TLKStreamableType read GetStreamableTypeByGUID;
  end;

var
  /// <summary><c>Global Streamable Types Manager</c></summary>
  /// <description><c>For Registering Streamable Types, and retreiving the correct Streamable Type from a given Stream or GUID.</c></description>
  Streamables: TLKStreamables = nil;

implementation

const
  // Used internally to ensure that older files work with the newer engine
  // This is only any good for Streamable Files produced after 2nd December 2014
  STREAMABLES_ENGINE_VERSION: Byte = 2;

{$IFDEF LKSL_EVENTENGINE_USEBINARYLISTS}
  { TLKStreamableTypesSortedList }

  function TLKStreamableTypesSortedList.AEqualToB(const A, B: TLKStreamableType): Boolean;
  begin
    Result := A.GetTypeGUID = B.GetTypeGUID;
  end;

  function TLKStreamableTypesSortedList.AGreaterThanB(const A, B: TLKStreamableType): Boolean;
  begin
    {$IFNDEF FPC}
      //BinaryCompare(@A, @B, SizeOf(TGUID));
      //Result := TComparer<TGUID>.Compare(A, B)
      Result := GUIDToString(A.GetTypeGUID) > GUIDToString(B.GetTypeGUID);
    {$ELSE}
      Result := A.GetTypeGUID > B.GetTypeGUID;
    {$ENDIF FPC}
  end;

  function TLKStreamableTypesSortedList.AGreaterThanOrEqualB(const A, B: TLKStreamableType): Boolean;
  begin
    {$IFNDEF FPC}
      //BinaryCompare(@A, @B, SizeOf(TGUID));
      //Result := TComparer<TGUID>.Compare(A, B)
      Result := GUIDToString(A.GetTypeGUID) >= GUIDToString(B.GetTypeGUID);
    {$ELSE}
      Result := A.GetTypeGUID >= B.GetTypeGUID;
    {$ENDIF FPC}
  end;

  function TLKStreamableTypesSortedList.ALessThanB(const A, B: TLKStreamableType): Boolean;
  begin
    {$IFNDEF FPC}
      //BinaryCompare(@A, @B, SizeOf(TGUID));
      //Result := TComparer<TGUID>.Compare(A, B)
      Result := GUIDToString(A.GetTypeGUID) < GUIDToString(B.GetTypeGUID);
    {$ELSE}
      Result := A.GetTypeGUID < B.GetTypeGUID;
    {$ENDIF FPC}
  end;

  function TLKStreamableTypesSortedList.ALessThanOrEqualB(const A, B: TLKStreamableType): Boolean;
  begin
    {$IFNDEF FPC}
      //BinaryCompare(@A, @B, SizeOf(TGUID));
      //Result := TComparer<TGUID>.Compare(A, B)
      Result := GUIDToString(A.GetTypeGUID) <= GUIDToString(B.GetTypeGUID);
    {$ELSE}
      Result := A.GetTypeGUID <= B.GetTypeGUID;
    {$ENDIF FPC}
  end;
{$ENDIF LKSL_EVENTENGINE_USEBINARYLISTS}

{ TLKStreamable }

class function TLKStreamable.CheckTypeInStream(const ACaret: ILKStreamCaret): Boolean;
begin
  Result := Streamables.StreamableTypeMatch(ACaret, Self);
end;

class function TLKStreamable.CheckTypeInStream(const ACaret: ILKStreamCaret; const APosition: Int64): Boolean;
begin
  Result := Streamables.StreamableTypeMatch(ACaret, Self, APosition);
end;

constructor TLKStreamable.Create;
begin
  inherited;
  FVersion := GetTypeVersion;
end;

constructor TLKStreamable.CreateFromStream(const ACaret: ILKStreamCaret);
begin
  Create;
  LoadFromStream(ACaret);
end;

constructor TLKStreamable.CreateFromFile(const AFileName: String);
begin
  inherited Create;
  LoadFromFile(AFileName);
end;

constructor TLKStreamable.CreateFromStream(const AStream: ILKStream);
begin
  CreateFromStream(AStream.NewCaret);
end;

constructor TLKStreamable.CreateFromStream(const AStream: ILKStream; const APosition: Int64);
begin
  CreateFromStream(AStream.NewCaret(APosition));
end;

constructor TLKStreamable.CreateFromStream(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  CreateFromStream(ACaret);
end;

class procedure TLKStreamable.DeleteFromStream(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  DeleteFromStream(ACaret);
end;

class procedure TLKStreamable.DeleteFromStream(const ACaret: ILKStreamCaret);
var
  LBlockStart, LBlockSize: Int64;
  LSignature: TGUID;
//  LEngineVersion: Byte; // FOR FUTURE USE
begin
  LBlockStart := ACaret.Position;
  LSignature := StreamReadGUID(ACaret); // Read the GUID
  if IsEqualGUID(LSignature, GetTypeGUID) then // Check if the Signature matches the expected GUID
  begin
    {LEngineVersion := }StreamReadByte(ACaret); // Need to advance the Stream past the Engine Version byte
    LBlockSize := StreamReadInt64(ACaret);

    ACaret.Position := LBlockStart;
    ACaret.Delete(LBlockSize);
  end else
    raise ELKStreamableSignatureMismatch.CreateFmt('Stream Signature Mismatch! Expected "%s", got "%s', [GUIDToString(GetTypeGUID), GUIDToString(LSignature)]);
end;

class function TLKStreamable.GetTypeVersion: Double;
begin
  Result := 0.00; // We assume that this is the FRIST version of this Streamable Type
end;

procedure TLKStreamable.LoadFromFile(const AFileName: String);
var
  LStream: ILKStream;
begin
  LStream := TLKFileStream.Create(AFileName, fmOpenRead);
  LoadFromStream(LStream.NewCaret);
end;

procedure TLKStreamable.LoadFromStream(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Stream.AcquireReadLock;
  try
    ACaret.Position := APosition;
    LoadFromStream(ACaret);
  finally
    ACaret.Stream.ReleaseReadLock;
  end;
end;

class procedure TLKStreamable.OnRegistration;
begin
  // Do Nothing!
  // Your descendant Streamable Type may need to do something when it's being Registered
  // This is particularly true of "TLKEventStreamable" in LKSL.Events.Main.pas
end;

class procedure TLKStreamable.OnUnregistration;
begin
  // Do Nothing!
  // Your descendant Streamable Type may need to do something when it's being Unregistered
  // This is particularly true of "TLKEventStreamable" in LKSL.Events.Main.pas
end;

procedure TLKStreamable.LoadFromStream(const ACaret: ILKStreamCaret);
var
  LSignature: TGUID;
//  LEngineVersion: Byte; // FOR FUTURE USE
begin
  AcquireWriteLock;
  try
    ACaret.Stream.AcquireReadLock;
    try
      LSignature := StreamReadGUID(ACaret); // Read the GUID
      if IsEqualGUID(LSignature, GetTypeGUID) then // Check if the Signature matches the expected GUID
      begin
        {LEngineVersion := }StreamReadByte(ACaret); // Need to advance the Stream past the Engine Version byte
        FBlockSize := StreamReadInt64(ACaret); // Read the Block Size
        FVersion := StreamReadDouble(ACaret); // Read the Version
        ReadFromStream(ACaret); // Read this type's specific values
      end else
        raise ELKStreamableSignatureMismatch.CreateFmt('Stream Signature Mismatch! Expected "%s", got "%s', [GUIDToString(GetTypeGUID), GUIDToString(LSignature)]);
    finally
      ACaret.Stream.ReleaseReadLock;
    end;
  finally
    ReleaseWriteLock;
  end;
end;

class procedure TLKStreamable.Register;
begin
  if Streamables = nil then
    Streamables := TLKStreamables.Create;
  Streamables.Register(Self);
end;

procedure TLKStreamable.SaveToFile(const AFileName: String);
var
  LStream: ILKStream;
begin
  LStream := TLKFileStream.Create(AFileName, fmCreate);
  SaveToStream(LStream.NewCaret);
end;

procedure TLKStreamable.SaveToStream(const ACaret: ILKStreamCaret; const APosition: Int64);
var
  LBlockSizePos: Int64;
  LBlockEnd: Int64;
begin
  if APosition = ACaret.Stream.Size then
    SaveToStream(ACaret)
  else
  begin
    AcquireReadLock;
    try
      ACaret.Position := APosition; // Set the position

      StreamInsertGUID(ACaret, GetTypeGUID); // Insert the GUID
      StreamInsertByte(ACaret, STREAMABLES_ENGINE_VERSION); // Insert the Engine Version

      LBlockSizePos := ACaret.Position; // Hold the current Position for the Block Size

      StreamInsertInt64(ACaret, 0); // Insert the default Block Size (0)
      StreamInsertDouble(ACaret, GetTypeVersion); // Insert the Type Version

      InsertIntoStream(ACaret); // Insert the descendant's custom data

      LBlockEnd := ACaret.Position; // Hold the current Position for the End of the Block
      FBlockSize := ACaret.Position - APosition; // Calculate the Block Size

      StreamWriteInt64(ACaret, FBlockSize, LBlockSizePos); // Overwrite the Block Size data with the ACTUAL Block Size

      ACaret.Position := LBlockEnd; // Reset the caret to the end of the Block
    finally
      ReleaseReadLock;
    end;
  end;
end;

class procedure TLKStreamable.Unregister;
begin
  Streamables.Unregister(Self);
end;

procedure TLKStreamable.SaveToStream(const ACaret: ILKStreamCaret);
var
  LBlockStart: Int64;
  LBlockSizePos: Int64;
  LBlockEnd: Int64;
begin
  AcquireReadLock;
  try
    ACaret.Stream.AcquireWriteLock;
    try
      LBlockStart := ACaret.Position;

      StreamWriteGUID(ACaret, GetTypeGUID); // Write the GUID
      StreamWriteByte(ACaret, STREAMABLES_ENGINE_VERSION); // Write the Engine Version

      LBlockSizePos := ACaret.Position;

      StreamWriteInt64(ACaret, 0); // Write the default Block Size (0)
      StreamWriteDouble(ACaret, GetTypeVersion); // Write the Version

      WriteToStream(ACaret);

      LBlockEnd := ACaret.Position;
      FBlockSize := ACaret.Position - LBlockStart;

      StreamWriteInt64(ACaret, FBlockSize, LBlockSizePos); // Update the Block Size

      ACaret.Position := LBlockEnd;
    finally
      ACaret.Stream.ReleaseWriteLock;
    end;
  finally
    ReleaseReadLock;
  end;
end;

{ TLKStreamableList.TLKStreamListHeader }

function TLKStreamableList.TLKStreamListHeader.GetCount: Integer;
begin
  AcquireReadLock;
  try
    Result := FCount;
  finally
    ReleaseReadLock;
  end;
end;

class function TLKStreamableList.TLKStreamListHeader.GetTypeGUID: TGUID;
const
  TYPE_GUID: TGUID = '{CBA750C3-8914-4D38-87F1-0BA460D52C85}';
begin
  Result := TYPE_GUID;
end;

procedure TLKStreamableList.TLKStreamListHeader.InsertIntoStream(const ACaret: ILKStreamCaret);
begin
  StreamInsertInteger(ACaret, FCount);
end;

procedure TLKStreamableList.TLKStreamListHeader.ReadFromStream(const ACaret: ILKStreamCaret);
begin
  FCount := StreamReadInteger(ACaret);
end;

procedure TLKStreamableList.TLKStreamListHeader.SetCount(const ACount: Integer);
begin
  AcquireWriteLock;
  try
    FCount := ACount;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKStreamableList.TLKStreamListHeader.WriteToStream(const ACaret: ILKStreamCaret);
begin
  StreamWriteInteger(ACaret, FCount);
end;

{ TLKStreamableList }

procedure TLKStreamableList.LoadFromFile(const AFileName: String);
var
  LStream: ILKStream;
begin
  LStream := TLKFileStream.Create(AFileName, fmOpenRead);
  LoadFromStream(LStream);
end;

procedure TLKStreamableList.LoadFromStream(const AStream: ILKStream);
var
  LCaret: ILKStreamCaret;
begin
  LCaret := AStream.NewCaret;
  LoadFromStream(LCaret);
end;

procedure TLKStreamableList.LoadFromStream(const ACaret: ILKStreamCaret);
var
  LHeader: TLKStreamListHeader;
  I: Integer;
begin
  ACaret.Stream.AcquireReadLock;
  try
    LHeader := TLKStreamListHeader(Streamables.CreateStreamableFromStream(ACaret));
    for I := 0 to LHeader.Count - 1 do
      Add(Streamables.CreateStreamableFromStream(ACaret));
  finally
    ACaret.Stream.ReleaseReadLock;
  end;
end;

procedure TLKStreamableList.SaveToFile(const AFileName: String);
var
  LStream: ILKStream;
begin
  LStream := TLKFileStream.Create(AFileName, fmCreate);
  SaveToStream(LStream);
end;

procedure TLKStreamableList.SaveToStream(const AStream: ILKStream);
var
  LCaret: ILKStreamCaret;
begin
  LCaret := AStream.NewCaret;
  SaveToStream(LCaret);
end;

procedure TLKStreamableList.SaveToStream(const ACaret: ILKStreamCaret);
var
  LHeader: TLKStreamListHeader;
  I: Integer;
begin
  ACaret.Stream.AcquireWriteLock;
  try
    LHeader := TLKStreamListHeader.Create;
    LHeader.Count := Count;
    LHeader.WriteToStream(ACaret);
    for I := 0 to Count - 1 do
      Items[I].WriteToStream(ACaret);
  finally
    ACaret.Stream.ReleaseWriteLock;
  end;
end;

{ TLKStreamableNamed }

function TLKStreamableNamed.GetName: String;
begin
  AcquireReadLock;
  try
    Result := FName;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKStreamableNamed.InsertIntoStream(const ACaret: ILKStreamCaret);
begin
  inherited;
  StreamInsertString(ACaret, FName); // Insert Name
end;

procedure TLKStreamableNamed.ReadFromStream(const ACaret: ILKStreamCaret);
begin
  inherited;
  FName := StreamReadString(ACaret);
end;

procedure TLKStreamableNamed.SetName(const AName: String);
begin
  AcquireWriteLock;
  try
    FName := AName;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKStreamableNamed.WriteToStream(const ACaret: ILKStreamCaret);
begin
  inherited;
  StreamWriteString(ACaret, FName);
end;

{ TLKStreamables }

procedure TLKStreamables.Clear;
begin
  {$IFDEF LKSL_EVENTENGINE_USEBINARYLISTS}

  {$ELSE}
  //TODO -oSJS -cTLKStreamables: Remove Locking once TLKDictionary is rewritten
    AcquireWriteLock;
    try
      FStreamableTypes.Clear;
    finally
      ReleaseWriteLock;
    end;
  {$ENDIF LKSL_EVENTENGINE_USEBINARYLISTS}
end;

constructor TLKStreamables.Create;
begin
  inherited;
  {$IFDEF LKSL_EVENTENGINE_USEBINARYLISTS}
    FStreamableTypes := TLKStreamableTypesSortedList.Create;
  {$ELSE}
    FStreamableTypes := TLKStreamableTypesDictionary.Create;
  {$ENDIF LKSL_EVENTENGINE_USEBINARYLISTS}
end;

function TLKStreamables.CreateStreamableFromStream(const ACaret: ILKStreamCaret): TLKStreamable;
begin
  Result := CreateStreamableFromStream(ACaret, ACaret.Position);
end;

function TLKStreamables.CreateStreamableFromStream(const ACaret: ILKStreamCaret; const APosition: Int64): TLKStreamable;
var
  LStreamableType: TLKStreamableType;
begin
  ACaret.Position := APosition;
  LStreamableType := GetStreamableTypeFromStream(ACaret);
  if LStreamableType = nil then
    Result := nil
  else
    Result := LStreamableType.CreateFromStream(ACaret);
end;

procedure TLKStreamables.DeleteArrayOfStreamables(const ACaret: ILKStreamCaret);
var
  I, LCount: Integer;
begin
  I := ACaret.Position; // Store the initial Position locally
  LCount := StreamReadInteger(ACaret); // Read the Array Count from the Stream
  ACaret.Position := I; // Reposition the Stream back before the Array Count
  StreamDeleteInteger(ACaret); // Delete the Array Count from the Stream
  for I := 0 to LCount - 1 do // Iterate the Array
    GetStreamableTypeFromStream(ACaret).DeleteFromStream(ACaret); // Delete Array Item from the Stream
end;

destructor TLKStreamables.Destroy;
begin
  Clear;
  FStreamableTypes.Free;
  inherited;
end;

function TLKStreamables.GetCount: Integer;
begin
  AcquireReadLock;
  try
    Result := FStreamableTypes.Count;
  finally
    ReleaseReadLock;
  end;
end;

function TLKStreamables.GetStreamableTypeByGUID(const AGUID: TGUID): TLKStreamableType;
{$IFDEF LKSL_EVENTENGINE_USEBINARYLISTS}
  var
    LIndex: Integer;
{$ENDIF LKSL_EVENTENGINE_USEBINARYLISTS}
begin
  {$IFDEF LKSL_EVENTENGINE_USEBINARYLISTS}
    FStreamableTypes.AcquireReadLock;
    try
//      LIndex := FStreamableTypes.IndexOf()
    finally
      FStreamableTypes.ReleaseReadLock;
    end;
  {$ELSE}
    if not (FStreamableTypes.TryGetValue(AGUID, Result)) then
      Result := nil;
  {$ENDIF LKSL_EVENTENGINE_USEBINARYLISTS}
end;
{
function TLKStreamables.GetStreamableTypeByIndex(const AIndex: Integer): TLKStreamableType;
begin
  Lock;
  try
    Result := FStreamableTypes.Values.ToArray[AIndex];
  finally
    Unlock;
  end;
end;
}
function TLKStreamables.GetStreamableTypeFromStream(const ACaret: ILKStreamCaret): TLKStreamableType;
var
  LPosition: Int64;
  LSignature: TGUID;
begin
  LPosition := ACaret.Position;
  LSignature := StreamReadGUID(ACaret);
  Result := GetStreamableTypeByGUID(LSignature);
  ACaret.Position := LPosition;
end;

procedure TLKStreamables.InsertArrayOfStreamables(const ACaret: ILKStreamCaret; const AStreamables: TLKStreamableArray);
var
  I: Integer;
begin
  StreamInsertInteger(ACaret, Length(AStreamables)); // Insert the Array Count into the Stream
  for I := Low(AStreamables) to High(AStreamables) do
    AStreamables[I].InsertInToStream(ACaret); // Insert the Item into the Stream
end;

procedure TLKStreamables.Register(const AStreamableTypes: array of TLKStreamableType);
var
  I: Integer;
begin
  for I := Low(AStreamableTypes) to High(AStreamableTypes) do
    Register(AStreamableTypes[I]);
end;

function TLKStreamables.StreamableTypeMatch(const ACaret: ILKStreamCaret; const AStreamableType: TLKStreamableType; const APosition: Int64): Boolean;
begin
  ACaret.Position := APosition;
  Result := StreamableTypeMatch(ACaret, AStreamableType);
end;

function TLKStreamables.StreamableTypeMatch(const ACaret: ILKStreamCaret; const AStreamableType: TLKStreamableType): Boolean;
var
  LStreamableType: TLKStreamableType;
begin
  LStreamableType := GetStreamableTypeFromStream(ACaret);
  Result := (((LStreamableType <> nil)) and (LStreamableType.InheritsFrom(AStreamableType)));
end;

procedure TLKStreamables.Register(const AStreamableType: TLKStreamableType);
begin
  AcquireWriteLock;
  try
    if not (FStreamableTypes.ContainsKey(AStreamableType.GetTypeGUID)) then
    begin
      FStreamableTypes.Add(AStreamableType.GetTypeGUID, AStreamableType);
      AStreamableType.OnRegistration;
    end else
      raise ELKStreamableTypeAlreadyRegistered.CreateFmt('A Streamable Type with the GUID "%s" is already registered', [GUIDToString(AStreamableType.GetTypeGUID)]);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKStreamables.Unregister(const AStreamableTypes: array of TLKStreamableType);
var
  I: Integer;
begin
  for I := Low(AStreamableTypes) to High(AStreamableTypes) do
    Unregister(AStreamableTypes[I]);
end;

procedure TLKStreamables.WriteArrayOfStreamables(const ACaret: ILKStreamCaret; const AStreamables: TLKStreamableArray);
var
  I: Integer;
begin
  StreamWriteInteger(ACaret, Length(AStreamables)); // Write the Array Count into the Stream
  for I := Low(AStreamables) to High(AStreamables) do
    AStreamables[I].SaveToStream(ACaret); // Write the Item into the Stream
end;

procedure TLKStreamables.Unregister(const AStreamableType: TLKStreamableType);
begin
  AcquireWriteLock;
  try
    if FStreamableTypes.ContainsKey(AStreamableType.GetTypeGUID) then
    begin
      FStreamableTypes.Remove(AStreamableType.GetTypeGUID);
      AStreamableType.OnUnregistration;
    end else
      raise ELKStreamableTypeNotRegistered.CreateFmt('A Streamable Type with the GUID "%s" has not yet been registered, therefore cannot be unregistered!', [GUIDToString(AStreamableType.GetTypeGUID)]);
  finally
    ReleaseWriteLock;
  end;
end;

initialization
  Streamables := TLKStreamables.Create;
  TLKStreamableList.TLKStreamListHeader.Register;
finalization
  Streamables.Free;

end.
