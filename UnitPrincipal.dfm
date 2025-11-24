object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 113
    Height = 25
    Caption = 'Ler arquivos'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 72
    Width = 601
    Height = 120
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object Memo1: TMemo
    Left = 8
    Top = 208
    Width = 601
    Height = 161
    TabOrder = 2
  end
  object btnAtualizar: TButton
    Left = 136
    Top = 16
    Width = 121
    Height = 25
    Caption = 'Atualizar Arquivos'
    TabOrder = 3
    OnClick = btnAtualizarClick
  end
  object Button2: TButton
    Left = 272
    Top = 16
    Width = 113
    Height = 25
    Caption = 'Comparar pastas'
    TabOrder = 4
    OnClick = Button2Click
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 40
    Top = 376
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 144
    Top = 376
  end
end
