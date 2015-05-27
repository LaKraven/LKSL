unit LKSL.DUTXGenerics.Collections;

interface

uses
  DUnitX.TestFramework, System.SysUtils,
  LKSL.Generics.CollectionsRedux;

type
  TFoo = class(TObject)
  private
    FFoo: String;
  public
    constructor Create(const AFoo: String);
    property Foo: String read FFoo;
  end;

  [TestFixture]
  TLKArrayTests = class(TObject)
  private
    FArray: TLKArray<String>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure ArrayIntegrity;
    [Test]
    [TestCase('In Range at 1', '1,True')]
    [TestCase('Out Of Range at 11', '11,False')]
    [TestCase('In Range at 2', '2,True')]
    [TestCase('Out Of Range at 1337', '1337,False')]
    [TestCase('In Range at 9', '9,True')]
    [TestCase('Out Of Range at 10', '10,False')]
    procedure ItemInRange(const AIndex: Integer; const AExpectInRange: Boolean);
  end;

  [TestFixture]
  TLKListTests = class(TObject)
  private
    FList: TLKList<String>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure ListIntegrity;
    [Test]
    [TestCase('In Range at 1', '1,True')]
    [TestCase('Out Of Range at 11', '11,False')]
    [TestCase('In Range at 2', '2,True')]
    [TestCase('Out Of Range at 1337', '1337,False')]
    [TestCase('In Range at 9', '9,True')]
    [TestCase('Out Of Range at 10', '10,False')]
    procedure ItemInRange(const AIndex: Integer; const AExpectInRange: Boolean);
    [Test]
    procedure DeletingItems;
  end;

  [TestFixture]
  TLKObjectListTests = class(TObject)
  private
    FList: TLKObjectList<TFoo>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure ListIntegrity;
    [Test]
    [TestCase('In Range at 1', '1,True')]
    [TestCase('Out Of Range at 11', '11,False')]
    [TestCase('In Range at 2', '2,True')]
    [TestCase('Out Of Range at 1337', '1337,False')]
    [TestCase('In Range at 9', '9,True')]
    [TestCase('Out Of Range at 10', '10,False')]
    procedure ItemInRange(const AIndex: Integer; const AExpectInRange: Boolean);
  end;

  [TestFixture]
  TLKCircularListTests = class(TObject)
  private
    FCircularList: TLKCircularList<String>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure ListIntegrity;
    [Test]
    [TestCase('In Range at 1', '1,True')]
    [TestCase('Out Of Range at 11', '11,False')]
    [TestCase('In Range at 2', '2,True')]
    [TestCase('Out Of Range at 1337', '1337,False')]
    [TestCase('In Range at 9', '9,True')]
    [TestCase('Out Of Range at 10', '10,False')]
    procedure ItemInRange(const AIndex: Integer; const AExpectInRange: Boolean);
    [Test]
    procedure ReplacingOldestItems;
    [Test]
    procedure ListFromArrayIntegrity;
    [Test]
    procedure DeletingItems;
  end;

  [TestFixture]
  TLKCircularObjectListTests = class(TObject)
  private
    FCircularObjectList: TLKCircularObjectList<TFoo>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure ListIntegrity;
    [Test]
    [TestCase('In Range at 1', '1,True')]
    [TestCase('Out Of Range at 11', '11,False')]
    [TestCase('In Range at 2', '2,True')]
    [TestCase('Out Of Range at 1337', '1337,False')]
    [TestCase('In Range at 9', '9,True')]
    [TestCase('Out Of Range at 10', '10,False')]
    procedure ItemInRange(const AIndex: Integer; const AExpectInRange: Boolean);
    [Test]
    procedure ReplacingOldestItems;
    [Test]
    procedure DeletingItems;
  end;

implementation

{ TFoo }

constructor TFoo.Create(const AFoo: String);
begin
  FFoo := AFoo;
end;

{ TLKArrayTests }

procedure TLKArrayTests.ArrayIntegrity;
const
  ITEMS: Array[0..9] of String = (
                                  'Bob',
                                  'Terry',
                                  'Andy',
                                  'Rick',
                                  'Sarah',
                                  'Ellen',
                                  'Hugh',
                                  'Jack',
                                  'Marie',
                                  'Ninette'
                                 );
var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FArray.Items[I] := ITEMS[I];
  for I := 0 to FArray.Capacity - 1 do
    Assert.IsTrue(FArray.Items[I] = ITEMS[I], Format('Item at Index %d does not match. Expected "%s" but got "%s"', [I, FArray.Items[I], ITEMS[I]]));
end;

procedure TLKArrayTests.ItemInRange(const AIndex: Integer; const AExpectInRange: Boolean);
const
  ITEMS: Array[0..9] of String = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J');
var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FArray.Items[I] := ITEMS[I];

  if not (AExpectInRange) then
    Assert.WillRaise(procedure
                     begin
                       FArray.Items[AIndex]
                     end,
                     ELKGenericCollectionsRangeException,
                     Format('Item %d SHOULD be out of range!', [AIndex]))
  else
    Assert.IsTrue(FArray.Items[AIndex] = ITEMS[AIndex], Format('Item %d did not match. Expected "%s" but got "%s"', [AIndex, ITEMS[AIndex], FArray.Items[AIndex]]))
end;

procedure TLKArrayTests.Setup;
begin
  FArray := TLKArray<String>.Create(10);
end;

procedure TLKArrayTests.TearDown;
begin
  FArray.Free;
end;

{ TLKListTests }

procedure TLKListTests.DeletingItems;
const
  ITEMS: Array[0..9] of String = (
                                  'Bob',
                                  'Terry',
                                  'Andy',
                                  'Rick',
                                  'Sarah',
                                  'Ellen',
                                  'Hugh',
                                  'Jack',
                                  'Marie',
                                  'Ninette'
                                 );
var
  I: Integer;
begin
  FList.AddItems(Items);
  FList.Delete(5);

  for I := 0 to FList.Count - 1 do
    if I < 5 then
      Assert.IsTrue(FList.Items[I] = ITEMS[I], Format('Item %d Expected "%s" got "%s"', [I, ITEMS[I], FList.Items[I]]))
    else
      Assert.IsTrue(FList.Items[I] = ITEMS[I + 1], Format('Item %d Expected "%s" got "%s"', [I, ITEMS[I], FList.Items[I]]))
end;

procedure TLKListTests.ItemInRange(const AIndex: Integer; const AExpectInRange: Boolean);
const
  ITEMS: Array[0..9] of String = (
                                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
                                 );
begin
  FList.AddItems(ITEMS);
  if not (AExpectInRange) then
    Assert.WillRaise(procedure
                     begin
                       FList.Items[AIndex]
                     end,
                     ELKGenericCollectionsRangeException,
                     Format('Item %d SHOULD be out of range!', [AIndex]))
  else
    Assert.IsTrue(FList.Items[AIndex] = ITEMS[AIndex], Format('Item %d did not match. Expected "%s" but got "%s"', [AIndex, ITEMS[AIndex], FList.Items[AIndex]]))
end;

procedure TLKListTests.ListIntegrity;
const
  ITEMS: Array[0..9] of String = (
                                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
                                 );
var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FList.Add(ITEMS[I]);
  Assert.IsTrue(FList.Count = Length(ITEMS), Format('Item Count Mismatch. Expected %d but got %d.', [Length(ITEMS), FList.Count]));
  Assert.IsTrue(FList.Capacity = Length(ITEMS), Format('List Capacity Mismatch. Expected %d but got %d.', [Length(ITEMS), FList.Capacity]));
  for I := 0 to FList.Count - 1 do
    Assert.IsTrue(FList.Items[I] = ITEMS[I], Format('Item %d Expected "%s" got "%s"', [I, ITEMS[I], FList.Items[I]]))
end;

procedure TLKListTests.Setup;
begin
  FList := TLKList<String>.Create;
end;

procedure TLKListTests.TearDown;
begin
  FList.Free;
end;

{ TLKObjectListTests }

procedure TLKObjectListTests.ItemInRange(const AIndex: Integer; const AExpectInRange: Boolean);
const
  ITEMS: Array[0..9] of String = (
                                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
                                 );
var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FList.Add(TFoo.Create(ITEMS[I]));
  if not (AExpectInRange) then
    Assert.WillRaise(procedure
                     begin
                       FList.Items[AIndex]
                     end,
                     ELKGenericCollectionsRangeException,
                     Format('Item %d SHOULD be out of range!', [AIndex]))
  else
    Assert.IsTrue(FList.Items[AIndex].Foo = ITEMS[AIndex], Format('Item %d did not match. Expected "%s" but got "%s"', [AIndex, ITEMS[AIndex], FList.Items[AIndex].Foo]))
end;

procedure TLKObjectListTests.ListIntegrity;
const
  ITEMS: Array[0..9] of String = (
                                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
                                 );
var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FList.Add(TFoo.Create(ITEMS[I]));
  Assert.IsTrue(FList.Count = Length(ITEMS), Format('Item Count Mismatch. Expected %d but got %d', [Length(ITEMS), FList.Count]));
  Assert.IsTrue(FList.Capacity = Length(ITEMS), Format('List Capacity Mismatch. Expected %d but got %d.', [Length(ITEMS), FList.Capacity]));
  for I := 0 to FList.Count - 1 do
    Assert.IsTrue(FList.Items[I].Foo = ITEMS[I], Format('Item %d Expected "%s" got "%s"', [I, ITEMS[I], FList.Items[I].Foo]))
end;

procedure TLKObjectListTests.Setup;
begin
  FList := TLKObjectList<TFoo>.Create;
end;

procedure TLKObjectListTests.TearDown;
begin
  FList.Free;
end;

{ TLKCircularListTests}

procedure TLKCircularListTests.DeletingItems;
const
  ITEMS: Array[0..9] of String = (
                                  'Bob',
                                  'Terry',
                                  'Andy',
                                  'Rick',
                                  'Sarah',
                                  'Ellen',
                                  'Hugh',
                                  'Jack',
                                  'Marie',
                                  'Ninette'
                                 );
var
  I: Integer;
begin
  FCircularList.AddItems(Items);
  FCircularList.Delete(5);

  for I := 0 to FCircularList.Count - 1 do
    if I < 5 then
      Assert.IsTrue(FCircularList.Items[I] = ITEMS[I], Format('Item %d Expected "%s" got "%s"', [I, ITEMS[I], FCircularList.Items[I]]))
    else
      Assert.IsTrue(FCircularList.Items[I] = ITEMS[I + 1], Format('Item %d Expected "%s" got "%s"', [I, ITEMS[I], FCircularList.Items[I]]))
end;

procedure TLKCircularListTests.ItemInRange(const AIndex: Integer; const AExpectInRange: Boolean);
const
  ITEMS: Array[0..9] of String = (
                                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
                                 );
begin
  FCircularList.AddItems(ITEMS);
  if not (AExpectInRange) then
    Assert.WillRaise(procedure
                     begin
                       FCircularList.Items[AIndex]
                     end,
                     ELKGenericCollectionsRangeException,
                     Format('Item %d SHOULD be out of range!', [AIndex]))
  else
    Assert.IsTrue(FCircularList.Items[AIndex] = ITEMS[AIndex], Format('Item %d did not match. Expected "%s" but got "%s"', [AIndex, ITEMS[AIndex], FCircularList.Items[AIndex]]))
end;

procedure TLKCircularListTests.ListFromArrayIntegrity;
const
  ITEMS: Array[0..9] of String = (
                                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
                                 );
var
  I: Integer;
begin
  FCircularList.AddItems(ITEMS);
  for I := 0 to FCircularList.Count - 1 do
    Assert.IsTrue(FCircularList.Items[I] = ITEMS[I], 'Items do not match... List has no Integirty!');
end;

procedure TLKCircularListTests.ListIntegrity;
const
  ITEMS: Array[0..9] of String = (
                                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
                                 );
var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FCircularList.Add(ITEMS[I]);
  for I := 0 to FCircularList.Count - 1 do
    Assert.IsTrue(FCircularList.Items[I] = ITEMS[I], Format('Item %d Expected "%s" got "%s"', [I, ITEMS[I], FCircularList.Items[I]]))
end;

procedure TLKCircularListTests.ReplacingOldestItems;
const
  ITEMS: Array[0..9] of String = (
                                    'Apple',
                                    'Banana',
                                    'Pineapple',
                                    'Orange',
                                    'Clementine',
                                    'Tangerine',
                                    'Melon',
                                    'Lemon',
                                    'Starfruit',
                                    'Pear'
                                 );
  OVERWRITES: Array[0..4] of String = (
                                       'Kiwi',
                                       'Mango',
                                       'Coconut',
                                       'Sprouts',
                                       'Beans'
                                      );

var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FCircularList.Add(ITEMS[I]);
  for I := Low(OVERWRITES) to High(OVERWRITES) do
    FCircularList.Add(OVERWRITES[I]);

  for I := 0 to FCircularList.Count - 1 do
    if I <= High(OVERWRITES) then
      Assert.IsTrue(FCircularList.Items[I] = OVERWRITES[I], Format('Did not replace item at Index %d. Expected "%s" but got "%s"', [I, OVERWRITES[I], FCircularList.Items[I]]))
    else
      Assert.IsTrue(FCircularList.Items[I] = ITEMS[I], Format('Did not replace item at Index %d. Expected "%s" but got "%s"', [I, ITEMS[I], FCircularList.Items[I]]))
end;

procedure TLKCircularListTests.Setup;
begin
  FCircularList := TLKCircularList<String>.Create(10);
end;

procedure TLKCircularListTests.TearDown;
begin
  FCircularList.Free;
end;

{ TLKCircularObjectListTests }

procedure TLKCircularObjectListTests.DeletingItems;
const
  ITEMS: Array[0..9] of String = (
                                  'Bob',
                                  'Terry',
                                  'Andy',
                                  'Rick',
                                  'Sarah',
                                  'Ellen',
                                  'Hugh',
                                  'Jack',
                                  'Marie',
                                  'Ninette'
                                 );
var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FCircularObjectList.Add(TFoo.Create(ITEMS[I]));

  FCircularObjectList.Delete(5);

  for I := 0 to FCircularObjectList.Count - 1 do
    if I < 5 then
      Assert.IsTrue(FCircularObjectList.Items[I].Foo = ITEMS[I], Format('Item %d Expected "%s" got "%s"', [I, ITEMS[I], FCircularObjectList.Items[I].Foo]))
    else
      Assert.IsTrue(FCircularObjectList.Items[I].Foo = ITEMS[I + 1], Format('Item %d Expected "%s" got "%s"', [I, ITEMS[I], FCircularObjectList.Items[I].Foo]))
end;

procedure TLKCircularObjectListTests.ItemInRange(const AIndex: Integer; const AExpectInRange: Boolean);
const
  ITEMS: Array[0..9] of String = (
                                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
                                 );
var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FCircularObjectList.Add(TFoo.Create(ITEMS[I]));
  if not (AExpectInRange) then
    Assert.WillRaise(procedure
                     begin
                       FCircularObjectList.Items[AIndex]
                     end,
                     ELKGenericCollectionsRangeException,
                     Format('Item %d SHOULD be out of range!', [AIndex]))
  else
    Assert.IsTrue(FCircularObjectList.Items[AIndex].Foo = ITEMS[AIndex], Format('Item %d did not match. Expected "%s" but got "%s"', [AIndex, ITEMS[AIndex], FCircularObjectList.Items[AIndex].Foo]))
end;

procedure TLKCircularObjectListTests.ListIntegrity;
const
  ITEMS: Array[0..9] of String = (
                                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
                                 );
var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FCircularObjectList.Add(TFoo.Create(ITEMS[I]));
  for I := 0 to FCircularObjectList.Count - 1 do
    Assert.IsTrue(FCircularObjectList.Items[I].Foo = ITEMS[I], Format('Item %d Expected "%s" got "%s"', [I, ITEMS[I], FCircularObjectList.Items[I].Foo]))
end;

procedure TLKCircularObjectListTests.ReplacingOldestItems;
const
  ITEMS: Array[0..9] of String = (
                                    'Apple',
                                    'Banana',
                                    'Pineapple',
                                    'Orange',
                                    'Clementine',
                                    'Tangerine',
                                    'Melon',
                                    'Lemon',
                                    'Starfruit',
                                    'Pear'
                                 );
  OVERWRITES: Array[0..4] of String = (
                                       'Kiwi',
                                       'Mango',
                                       'Coconut',
                                       'Sprouts',
                                       'Beans'
                                      );

var
  I: Integer;
begin
  for I := Low(ITEMS) to High(ITEMS) do
    FCircularObjectList.Add(TFoo.Create(ITEMS[I]));
  for I := Low(OVERWRITES) to High(OVERWRITES) do
    FCircularObjectList.Add(TFoo.Create(OVERWRITES[I]));

  for I := 0 to FCircularObjectList.Count - 1 do
    if I <= High(OVERWRITES) then
      Assert.IsTrue(FCircularObjectList.Items[I].Foo = OVERWRITES[I], Format('Did not replace item at Index %d. Expected "%s" but got "%s"', [I, OVERWRITES[I], FCircularObjectList.Items[I].Foo]))
    else
      Assert.IsTrue(FCircularObjectList.Items[I].Foo = ITEMS[I], Format('Did not replace item at Index %d. Expected "%s" but got "%s"', [I, ITEMS[I], FCircularObjectList.Items[I].Foo]))
end;

procedure TLKCircularObjectListTests.Setup;
begin
  FCircularObjectList := TLKCircularObjectList<TFoo>.Create(10);
end;

procedure TLKCircularObjectListTests.TearDown;
begin
  FCircularObjectList.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TLKArrayTests, 'TLKArray Tests');
  TDUnitX.RegisterTestFixture(TLKListTests, 'TLKList Tests');
  TDUnitX.RegisterTestFixture(TLKCircularListTests, 'TLKCircularList Tests');
  TDUnitX.RegisterTestFixture(TLKCircularObjectListTests, 'TLKCircularObjectList Tests');
end.
