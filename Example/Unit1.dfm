object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 208
    Top = 128
    Width = 28
    Height = 15
    Caption = 'Input'
  end
  object Label2: TLabel
    Left = 368
    Top = 128
    Width = 38
    Height = 15
    Caption = 'Output'
  end
  object Edit1: TEdit
    Left = 160
    Top = 160
    Width = 121
    Height = 23
    TabOrder = 0
    Text = 'in.rtf'
  end
  object Edit2: TEdit
    Left = 328
    Top = 160
    Width = 121
    Height = 23
    TabOrder = 1
    Text = 'out.pdf'
  end
  object Button1: TButton
    Left = 264
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 2
    OnClick = Button1Click
  end
end
