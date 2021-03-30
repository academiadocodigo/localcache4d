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
  LocalCache4D,
  Vcl.Grids,
  Vcl.ValEdit;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    ValueListEditor1: TValueListEditor;
    Button5: TButton;
    ListBox1: TListBox;
    LabeledEdit1: TLabeledEdit;
    Button6: TButton;
    Button7: TButton;
    OpenDialog1: TOpenDialog;
    LabeledEdit4: TLabeledEdit;
    Button8: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure aListItensClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ValueListEditor1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }
    procedure ListItems;
    procedure ListInstances;
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
  LocalCache.Instance(LabeledEdit1.Text).SetItem(LabeledEdit2.Text, LabeledEdit3.Text);
  ListItems();
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  LabeledEdit3.Text := LocalCache.GetItem(LabeledEdit2.Text);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  LocalCache.LoadDatabase(LabeledEdit4.Text);
  ListInstances;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  LocalCache.SaveToStorage(LabeledEdit4.Text);
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  LocalCache.Instance(LabeledEdit1.Text).RemoveItem(LabeledEdit2.Text);
  ListItems();
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  LocalCache.Instance(LabeledEdit1.Text);
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  LocalCache.RemoveInstance(LabeledEdit1.Text);
  ListInstances;
end;

procedure TForm2.Button8Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LabeledEdit4.Text := OpenDialog1.FileName;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  //LocalCache.LoadDatabase();
  //ListInstances;
  //ListItems();
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  //LocalCache.SaveToStorage();
end;

procedure TForm2.ListBox1Click(Sender: TObject);
begin
  ListItems;
  LabeledEdit1.Text := ListBox1.Items[ListBox1.ItemIndex];
end;

procedure TForm2.ListInstances;
begin
  ListBox1.Clear;
  for var Instances in LocalCache.ListInstances do
    ListBox1.AddItem(Instances.Key, Instances.Value);

  if LabeledEdit1.Text <> '' then
    ListBox1.Items.IndexOf(LabeledEdit1.Text);
end;

procedure TForm2.ListItems;
begin
  //ListInstances;
  LocalCache.Instance(ListBox1.Items[ListBox1.ItemIndex]);
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
