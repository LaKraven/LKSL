unit LKSL.DUTXGenerics.Collections;

interface

uses
  DUnitX.TestFramework, System.SysUtils,
  LKSL.Generics.CollectionsRedux;

type

  [TestFixture]
  TLKCircularListTest = class(TObject)
  private
    FCircularList: TLKCircularList<String>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure AddingItems;
    [Test]
    procedure ReplacingOldestItems;
  end;

implementation

procedure TLKCircularListTest.AddingItems;
const
  ITEMS: Array[0..9] of String = (
                                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
                                 );
var
  I: Integer;
begin
  TDUnitX.CurrentRunner.Status('Testing simple List Integrity');
  for I := Low(ITEMS) to High(ITEMS) do
    FCircularList.Add(ITEMS[I]);
  TDUnitX.CurrentRunner.Status('Added 10 items to Circular List');
  TDUnitX.CurrentRunner.Status('Verifying that Items match...');
  for I := 0 to FCircularList.Count - 1 do
    Assert.IsTrue(FCircularList.Items[I] = ITEMS[I], 'Items do not match... List has no Integirty!');
end;

procedure TLKCircularListTest.ReplacingOldestItems;
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

procedure TLKCircularListTest.Setup;
begin
  FCircularList := TLKCircularList<String>.Create(10);
end;

procedure TLKCircularListTest.TearDown;
begin
  FCircularList.Free;
end;


initialization
  TDUnitX.RegisterTestFixture(TLKCircularListTest);
end.
