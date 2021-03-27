unit Unit2;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  LocalCache4D, Vcl.Grids, Vcl.ValEdit;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    ValueListEditor1: TValueListEditor;
    aListItens: TButton;
    Button5: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure aListItensClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ValueListEditor1Click(Sender: TObject);
  private
    { Private declarations }
    procedure ListItems;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.aListItensClick(Sender: TObject);
begin
  ListItems;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  LocalCache.SetItem(LabeledEdit2.Text, LabeledEdit3.Text);
  ListItems();
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  LabeledEdit3.Text := LocalCache.GetItem(LabeledEdit2.Text);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  LocalCache.LoadDatabase;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  LocalCache.SaveToStorage;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  LocalCache.RemoveItem(LabeledEdit2.Text);
  ListItems();
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  LocalCache.LoadDatabase();
  ListItems();
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  LocalCache.SaveToStorage();
end;

procedure TForm2.ListItems;
begin
  ValueListEditor1.Strings.Clear;
  for var Item in LocalCache.ListItens do
      ValueListEditor1.InsertRow(Item.Key, Item.Value, True);
end;

procedure TForm2.ValueListEditor1Click(Sender: TObject);
begin
  LabeledEdit2.Text := ValueListEditor1.Cells[0, ValueListEditor1.Row];
  LabeledEdit3.Text := ValueListEditor1.Cells[1, ValueListEditor1.Row]
end;

end.
