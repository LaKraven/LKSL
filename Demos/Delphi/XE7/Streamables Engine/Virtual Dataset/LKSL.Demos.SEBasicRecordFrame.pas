unit LKSL.Demos.SEBasicRecordFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  LKSL.Demos.SEBasicStreamables, FMX.Layouts, FMX.Controls.Presentation, FMX.Edit, FMX.EditBox, FMX.NumberBox;

type
  TDeleteCallback = procedure() of object;

  TframeRecord = class(TFrame)
    btnDelete: TSpeedButton;
    edField1: TEdit;
    Layout1: TLayout;
    Layout2: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    nbField2: TNumberBox;
    nbField3: TNumberBox;
    Label3: TLabel;
    Label4: TLabel;
    cbField4: TCheckBox;
    Label5: TLabel;
    btnImageView: TSpeedButton;
    btnLoadImage: TSpeedButton;
    odLoadImage: TOpenDialog;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnImageViewClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure edField1Change(Sender: TObject);
    procedure nbField2Change(Sender: TObject);
    procedure nbField3Change(Sender: TObject);
    procedure cbField4Change(Sender: TObject);
  private
    FRecord: TStreamableDummyRecord;
    FOnDelete: TDeleteCallback;
  public
    procedure SetRecord(const ARecord: TStreamableDummyRecord);
    property OnDelete: TDeleteCallback read FOnDelete write FOnDelete;
  end;

implementation

uses
  LKSL.Demos.SEBasicViewerForm;

{$R *.fmx}

{ TFrame1 }

procedure TframeRecord.btnDeleteClick(Sender: TObject);
begin
  FRecord.Free;
  if Assigned(FOnDelete) then
    FOnDelete;
end;

procedure TframeRecord.btnImageViewClick(Sender: TObject);
begin
  frmImageViewer.SetImage(FRecord.Field5);
  frmImageViewer.ShowModal;
end;

procedure TframeRecord.SetRecord(const ARecord: TStreamableDummyRecord);
begin
  FRecord := ARecord;
  edField1.Text := FRecord.Field1;
  nbField2.Value := FRecord.Field2;
  nbField3.Value := FRecord.Field3;
  cbField4.IsChecked := FRecord.Field4;
  btnImageView.Enabled := FRecord.Field5.HandleAllocated;
end;

procedure TframeRecord.btnLoadImageClick(Sender: TObject);
begin
  if odLoadImage.Execute then
  begin
    FRecord.Field5.LoadFromFile(odLoadImage.FileName);
    btnImageView.Enabled := True;
  end;
end;

procedure TframeRecord.cbField4Change(Sender: TObject);
begin
  FRecord.Field4 := cbField4.IsChecked;
end;

procedure TframeRecord.edField1Change(Sender: TObject);
begin
  FRecord.Field1 := edField1.Text;
end;

procedure TframeRecord.nbField2Change(Sender: TObject);
begin
  FRecord.Field2 := Round(nbField2.Value);
end;

procedure TframeRecord.nbField3Change(Sender: TObject);
begin
  FRecord.Field3 := nbField3.Value;
end;

end.
