object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 514
  ClientWidth = 594
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    594
    514)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 40
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = #33258#21160#28378#21160
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object ListView1: TListView
    Left = 8
    Top = 59
    Width = 576
    Height = 444
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = #26085#24535#20869#23481
      end>
    OwnerData = True
    TabOrder = 1
    ViewStyle = vsReport
    OnData = ListView1Data
  end
  object Button1: TButton
    Left = 192
    Top = 6
    Width = 75
    Height = 25
    Caption = #24182#21457#27979#35797
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 97
    Top = 6
    Width = 75
    Height = 25
    Caption = #28165#31354#26085#24535
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 439
    Top = 6
    Width = 96
    Height = 25
    Caption = #20889#19968#34892#26085#24535
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 273
    Top = 6
    Width = 90
    Height = 25
    Caption = #20572#27490#24182#21457#27979#35797
    TabOrder = 5
    OnClick = Button4Click
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 296
    Top = 240
  end
  object Timer2: TTimer
    Interval = 100
    OnTimer = Timer2Timer
    Left = 336
    Top = 240
  end
end
