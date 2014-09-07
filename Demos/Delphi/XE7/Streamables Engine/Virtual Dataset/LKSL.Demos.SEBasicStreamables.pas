unit LKSL.Demos.SEBasicStreamables;

interface

uses
  System.Classes, System.SysUtils,
  FMX.Graphics,
  LKSL.Streamables.Base;

type
  { Forward Declarations }
  TStreamableDummyTable = class;
  TStreamableDummyRecord = class;

  { Array Types }
  TStreamableDummyRecordArray = Array of TStreamableDummyRecord;

  {
    TStreamableDummyTable
      - It's basically a class pretending to be a Table.
      - No, it isn't an ACTUAL table, it just contains an Array of "dummy records"
      - Oh, and it's a NAMED Streamable, because... why not, right?
  }
  TStreamableDummyTable = class(TLKStreamableNamed)
  private
    FRecords: TStreamableDummyRecordArray;
    procedure AddRecord(const ARecord: TStreamableDummyRecord);
    procedure DeleteRecord(const ARecord: TStreamableDummyRecord);
    function GetRecord(const AIndex: Integer): TStreamableDummyRecord;
    function GetRecordCount: Integer;
  public
    destructor Destroy; override;
    { TLKStreamable Overrides Begin }
    class function GetTypeGUID: String; override;
    class procedure DeleteFromStream(const AStream: TStream); override;
    procedure ReadFromStream(const AStream: TStream); override;
    procedure InsertIntoStream(const AStream: TStream); override;
    procedure WriteToStream(const AStream: TStream); override;
    { TLKStreamable Overrides End }
    property RecordCount: Integer read GetRecordCount;
    property Records[const AIndex: Integer]: TStreamableDummyRecord read GetRecord;
  end;

  {
    TStreamableDummyRecord
      - A set of data crudely mascarading as a "Record"
      - The point is that this stuff all gets nested into the Dummy Table's Stream Data.
  }
  TStreamableDummyRecord = class(TLKStreamable)
  private
    FDummyTable: TStreamableDummyTable;
    FField1: String;
    FField2: Integer;
    FField3: Double;
    FField4: Boolean;
    FField5: TBitmap;
    FIndex: Integer;
    function GetField1: String;
    function GetField2: Integer;
    function GetField3: Double;
    function GetField4: Boolean;
    function GetField5: TBitmap;
    function GetIndex: Integer;
    procedure SetField1(const AValue: String);
    procedure SetField2(const AValue: Integer);
    procedure SetField3(const AValue: Double);
    procedure SetField4(const AValue: Boolean);
  public
    constructor Create(const ADummyTable: TStreamableDummyTable); reintroduce;
    destructor Destroy; override;
    { TLKStreamable Overrides Begin }
    class function GetTypeGUID: String; override;
    class procedure DeleteFromStream(const AStream: TStream); override;
    procedure ReadFromStream(const AStream: TStream); override;
    procedure InsertIntoStream(const AStream: TStream); override;
    procedure WriteToStream(const AStream: TStream); override;
    { TLKStreamable Overrides End }
    property Field1: String read GetField1 write SetField1;
    property Field2: Integer read GetField2 write SetField2;
    property Field3: Double read GetField3 write SetField3;
    property Field4: Boolean read GetField4 write SetField4;
    property Field5: TBitmap read GetField5;
    property Index: Integer read GetIndex;
  end;

implementation

uses
  LKSL.Streams.System;

{ TStreamableDummyTable }

procedure TStreamableDummyTable.AddRecord(const ARecord: TStreamableDummyRecord);
var
  LIndex: Integer;
begin
  Lock;
  LIndex := Length(FRecords);
  SetLength(FRecords, LIndex + 1);
  FRecords[LIndex] := ARecord;
  ARecord.FIndex := LIndex;
  Unlock;
end;

class procedure TStreamableDummyTable.DeleteFromStream(const AStream: TStream);
begin
  inherited;
  Streamables.DeleteArrayOfStreamables(AStream); // Deletes the entire Record Array from the Stream
end;

procedure TStreamableDummyTable.DeleteRecord(const ARecord: TStreamableDummyRecord);
var
  I: Integer;
begin
  for I := ARecord.FIndex to High(FRecords) - 1 do
  begin
    FRecords[I] := FRecords[I + 1];
    FRecords[I].FIndex := I;
  end;
  SetLength(FRecords, Length(FRecords) - 1);
end;

destructor TStreamableDummyTable.Destroy;
var
  I: Integer;
begin
  for I := High(FRecords) downto Low(FRecords) do
    FRecords[I].Free;
  inherited;
end;

function TStreamableDummyTable.GetRecord(const AIndex: Integer): TStreamableDummyRecord;
begin
  Lock;
  Result := FRecords[AIndex];
  Unlock;
end;

function TStreamableDummyTable.GetRecordCount: Integer;
begin
  Lock;
  Result := Length(FRecords);
  Unlock;
end;

class function TStreamableDummyTable.GetTypeGUID: String;
begin
  Result := '{70F261C1-4317-4E38-A781-0985E45D6821}';
end;

procedure TStreamableDummyTable.InsertIntoStream(const AStream: TStream);
begin
  Lock;
  inherited;
  Streamables.InsertArrayOfStreamables(AStream, TLKStreamableArray(FRecords));
  Unlock;
end;

procedure TStreamableDummyTable.ReadFromStream(const AStream: TStream);
var
  I, LCount: Integer;
  LDummyRecord: TStreamableDummyRecord;
begin
  Lock;
  inherited;
  LCount := StreamReadInteger(AStream); // Retrieve the Record Count
  for I := 0 to LCount - 1 do // Iterate the Records
  begin
    if Streamables.StreamableTypeMatch(AStream, TStreamableDummyRecord) then // Make sure the Types match
    begin
      LDummyRecord := TStreamableDummyRecord.Create(Self); // Create a Record
      LDummyRecord.ReadFromStream(AStream); // Populate the Record from the Stream
    end;
  end;
  Unlock;
end;

procedure TStreamableDummyTable.WriteToStream(const AStream: TStream);
begin
  Lock;
  inherited;
  Streamables.WriteArrayOfStreamables(AStream, TLKStreamableArray(FRecords));
  Unlock;
end;

{ TStreamableDummyRecord }

constructor TStreamableDummyRecord.Create(const ADummyTable: TStreamableDummyTable);
begin
  inherited Create;
  FDummyTable := ADummyTable;
  FField5 := TBitmap.Create;
  FDummyTable.AddRecord(Self);
end;

class procedure TStreamableDummyRecord.DeleteFromStream(const AStream: TStream);
begin
  inherited;
  StreamDeleteString(AStream); // Delete FField1
  StreamDeleteInteger(AStream); // Delete FField2
  StreamDeleteDouble(AStream); // Delete FField3
  StreamDeleteBoolean(AStream); // Delete FField4
  StreamDeleteStream(AStream); // Delete FField5
end;

destructor TStreamableDummyRecord.Destroy;
begin
  FField5.Free;
  FDummyTable.DeleteRecord(Self);
  inherited;
end;

function TStreamableDummyRecord.GetField1: String;
begin
  Lock;
  Result := FField1;
  Unlock;
end;

function TStreamableDummyRecord.GetField2: Integer;
begin
  Lock;
  Result := FField2;
  Unlock;
end;

function TStreamableDummyRecord.GetField3: Double;
begin
  Lock;
  Result := FField3;
  Unlock;
end;

function TStreamableDummyRecord.GetField4: Boolean;
begin
  Lock;
  Result := FField4;
  Unlock;
end;

function TStreamableDummyRecord.GetField5: TBitmap;
begin
  Lock;
  Result := FField5;
  Unlock;
end;

function TStreamableDummyRecord.GetIndex: Integer;
begin
  Lock;
  Result := FIndex;
  Unlock;
end;

class function TStreamableDummyRecord.GetTypeGUID: String;
begin
  Result := '{814CA57C-8106-4423-A8CB-ACA26926C38B}';
end;

procedure TStreamableDummyRecord.InsertIntoStream(const AStream: TStream);
var
  LStream: TStream;
begin
  Lock;
  inherited;
  StreamInsertString(AStream, FField1); // Insert FField1
  StreamInsertInteger(AStream, FField2); // Insert FField2
  StreamInsertDouble(AStream, FField3); // Insert FField3
  StreamInsertBoolean(AStream, FField4); // Insert FField4
  LStream := TMemoryStream.Create;
  try
    FField5.SaveToStream(LStream);
    StreamInsertStream(AStream, LStream); // Insert FField5
  finally
    LStream.Free;
  end;
  Unlock;
end;

procedure TStreamableDummyRecord.ReadFromStream(const AStream: TStream);
var
  LStream: TStream;
begin
  Lock;
  inherited;
  FField1 := StreamReadString(AStream); // Read FField1
  FField2 := StreamReadInteger(AStream); // Read FField2
  FField3 := StreamReadDouble(AStream); // Read FField3
  FField4 := StreamReadBoolean(AStream); // Read FField4
  LStream := TMemoryStream.Create;
  try
    StreamReadStream(AStream, LStream); // Read FField5
    if LStream.Size > 12 then // If there is no actual Image, the Stream will be 12 bytes in size
      FField5.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
  Unlock;
end;

procedure TStreamableDummyRecord.SetField1(const AValue: String);
begin
  Lock;
  FField1 := AValue;
  Unlock;
end;

procedure TStreamableDummyRecord.SetField2(const AValue: Integer);
begin
  Lock;
  FField2 := AValue;
  Unlock;
end;

procedure TStreamableDummyRecord.SetField3(const AValue: Double);
begin
  Lock;
  FField3 := AValue;
  Unlock;
end;

procedure TStreamableDummyRecord.SetField4(const AValue: Boolean);
begin
  Lock;
  FField4 := AValue;
  Unlock;
end;

procedure TStreamableDummyRecord.WriteToStream(const AStream: TStream);
var
  LStream: TStream;
begin
  Lock;
  inherited;
  StreamWriteString(AStream, FField1); // Write FField1
  StreamWriteInteger(AStream, FField2); // Write FField2
  StreamWriteDouble(AStream, FField3); // Write FField3
  StreamWriteBoolean(AStream, FField4); // Write FField4
  LStream := TMemoryStream.Create;
  try
    FField5.SaveToStream(LStream);
    StreamWriteStream(AStream, LStream); // Write FField5
  finally
    LStream.Free;
  end;
  Unlock;
end;

initialization
  Streamables.Register([TStreamableDummyTable, TStreamableDummyRecord]);
finalization
  Streamables.Unregister([TStreamableDummyTable, TStreamableDummyRecord]);
end.
