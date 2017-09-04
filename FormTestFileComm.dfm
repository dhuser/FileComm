object Form1: TForm1
  Left = 457
  Top = 267
  Width = 554
  Height = 408
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 121
    Height = 97
    Caption = 'FileComm-Objekte Init'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 22
      Height = 13
      Caption = 'Nein'
    end
    object Label2: TLabel
      Left = 16
      Top = 40
      Width = 22
      Height = 13
      Caption = 'Nein'
    end
    object Label3: TLabel
      Left = 16
      Top = 56
      Width = 22
      Height = 13
      Caption = 'Nein'
    end
    object Label4: TLabel
      Left = 16
      Top = 72
      Width = 9
      Height = 13
      Caption = '---'
    end
  end
  object butSendeN1: TButton
    Left = 8
    Top = 112
    Width = 121
    Height = 25
    Caption = 'Sende  N1'
    TabOrder = 1
    OnClick = butSendeN1Click
  end
  object Memo1: TMemo
    Left = 136
    Top = 13
    Width = 401
    Height = 305
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object butLoeschen: TButton
    Left = 408
    Top = 328
    Width = 129
    Height = 25
    Caption = 'L'#246'schen'
    TabOrder = 3
    OnClick = butLoeschenClick
  end
  object butSende1N10: TButton
    Left = 8
    Top = 144
    Width = 121
    Height = 25
    Caption = 'Sende N1-10x'
    TabOrder = 4
    OnClick = butSende1N10Click
  end
  object Button1: TButton
    Left = 8
    Top = 216
    Width = 121
    Height = 25
    Caption = 'Sende async N1'
    TabOrder = 5
    OnClick = Button1Click
  end
end
