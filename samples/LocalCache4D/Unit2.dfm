object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'LocalCacheAdmin'
  ClientHeight = 639
  ClientWidth = 1086
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Button1: TButton
    Left = 903
    Top = 279
    Width = 82
    Height = 25
    Caption = 'Set'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 8
    Top = 54
    Width = 204
    Height = 37
    Caption = 'LoadDataBase'
    TabOrder = 1
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 218
    Top = 54
    Width = 207
    Height = 37
    Caption = 'SaveStorage'
    TabOrder = 2
    OnClick = Button4Click
  end
  object LabeledEdit2: TLabeledEdit
    Left = 903
    Top = 203
    Width = 175
    Height = 24
    EditLabel.Width = 20
    EditLabel.Height = 16
    EditLabel.Caption = 'Key'
    TabOrder = 3
  end
  object LabeledEdit3: TLabeledEdit
    Left = 903
    Top = 249
    Width = 175
    Height = 24
    EditLabel.Width = 32
    EditLabel.Height = 16
    EditLabel.Caption = 'Value'
    TabOrder = 4
  end
  object ValueListEditor1: TValueListEditor
    Left = 218
    Top = 97
    Width = 679
    Height = 512
    TabOrder = 5
    OnClick = ValueListEditor1Click
    ColWidths = (
      150
      523)
  end
  object Button5: TButton
    Left = 991
    Top = 279
    Width = 87
    Height = 25
    Caption = 'Remove'
    TabOrder = 6
    OnClick = Button5Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 97
    Width = 204
    Height = 512
    TabOrder = 7
    OnClick = ListBox1Click
  end
  object LabeledEdit1: TLabeledEdit
    Left = 903
    Top = 115
    Width = 175
    Height = 24
    EditLabel.Width = 48
    EditLabel.Height = 16
    EditLabel.Caption = 'Instance'
    TabOrder = 8
  end
  object Button6: TButton
    Left = 903
    Top = 145
    Width = 94
    Height = 25
    Caption = 'Set'
    TabOrder = 9
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 1003
    Top = 145
    Width = 75
    Height = 25
    Caption = 'Remove'
    TabOrder = 10
    OnClick = Button7Click
  end
  object LabeledEdit4: TLabeledEdit
    Left = 8
    Top = 24
    Width = 989
    Height = 24
    EditLabel.Width = 77
    EditLabel.Height = 16
    EditLabel.Caption = 'Database File'
    TabOrder = 11
  end
  object Button8: TButton
    Left = 1003
    Top = 24
    Width = 75
    Height = 25
    Caption = '...'
    TabOrder = 12
    OnClick = Button8Click
  end
  object OpenDialog1: TOpenDialog
    Left = 752
    Top = 120
  end
end
