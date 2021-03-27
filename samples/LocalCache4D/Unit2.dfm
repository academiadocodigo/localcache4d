object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 427
  ClientWidth = 327
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 382
    Width = 75
    Height = 25
    Caption = 'Set'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 129
    Top = 382
    Width = 75
    Height = 25
    Caption = 'Get'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 19
    Width = 154
    Height = 25
    Caption = 'LoadDataBase'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 168
    Top = 19
    Width = 146
    Height = 25
    Caption = 'SaveStorage'
    TabOrder = 3
    OnClick = Button4Click
  end
  object LabeledEdit2: TLabeledEdit
    Left = 8
    Top = 355
    Width = 75
    Height = 21
    EditLabel.Width = 18
    EditLabel.Height = 13
    EditLabel.Caption = 'Key'
    TabOrder = 4
  end
  object LabeledEdit3: TLabeledEdit
    Left = 89
    Top = 355
    Width = 225
    Height = 21
    EditLabel.Width = 26
    EditLabel.Height = 13
    EditLabel.Caption = 'Value'
    TabOrder = 5
  end
  object ValueListEditor1: TValueListEditor
    Left = 8
    Top = 81
    Width = 306
    Height = 252
    TabOrder = 6
    OnClick = ValueListEditor1Click
  end
  object aListItens: TButton
    Left = 8
    Top = 50
    Width = 306
    Height = 25
    Caption = 'List Itens'
    TabOrder = 7
    OnClick = aListItensClick
  end
  object Button5: TButton
    Left = 239
    Top = 382
    Width = 75
    Height = 25
    Caption = 'Remove'
    TabOrder = 8
    OnClick = Button5Click
  end
end
