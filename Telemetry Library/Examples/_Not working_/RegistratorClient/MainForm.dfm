object frmMainForm: TfrmMainForm
  Left = 493
  Top = 123
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Registrator Client'
  ClientHeight = 587
  ClientWidth = 776
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblEvents: TLabel
    Left = 264
    Top = 8
    Width = 37
    Height = 13
    Caption = 'Events:'
  end
  object lblActionsLog: TLabel
    Left = 8
    Top = 384
    Width = 56
    Height = 13
    Caption = 'Actions log:'
  end
  object lblPort: TLabel
    Left = 168
    Top = 8
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object lblChannels: TLabel
    Left = 8
    Top = 176
    Width = 48
    Height = 13
    Caption = 'Channels:'
  end
  object bvlSplit: TBevel
    Left = 256
    Top = 8
    Width = 9
    Height = 161
    Shape = bsLeftLine
  end
  object shpItemNormal: TShape
    Left = 8
    Top = 92
    Width = 241
    Height = 17
    Pen.Style = psClear
  end
  object shpItemRegistered: TShape
    Left = 8
    Top = 116
    Width = 241
    Height = 17
    Brush.Color = 11141074
    Pen.Style = psClear
  end
  object shpItemUnknown: TShape
    Left = 8
    Top = 140
    Width = 241
    Height = 17
    Brush.Color = 8978431
    Pen.Style = psClear
  end
  object lblItemNormal: TLabel
    Left = 8
    Top = 92
    Width = 241
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Known item, not registered'
    Transparent = True
    Layout = tlCenter
  end
  object lblItemRegistered: TLabel
    Left = 8
    Top = 116
    Width = 241
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Known item, registered'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
    Layout = tlCenter
  end
  object lblItemUnknown: TLabel
    Left = 8
    Top = 140
    Width = 241
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Unknown item, registered'
    Transparent = True
    Layout = tlCenter
  end
  object lbEvents: TListBox
    Left = 264
    Top = 24
    Width = 505
    Height = 124
    Style = lbOwnerDrawFixed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 20
    ParentFont = False
    TabOrder = 3
    OnDblClick = lbEventsDblClick
    OnDrawItem = lbEventsDrawItem
  end
  object meActionsLog: TMemo
    Left = 8
    Top = 400
    Width = 761
    Height = 161
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 9
    WordWrap = False
  end
  object leAddress: TLabeledEdit
    Left = 8
    Top = 24
    Width = 153
    Height = 21
    EditLabel.Width = 43
    EditLabel.Height = 13
    EditLabel.Caption = 'Address:'
    TabOrder = 0
  end
  object sePort: TSpinEdit
    Left = 168
    Top = 24
    Width = 81
    Height = 22
    MaxValue = 65535
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object btnConnect: TButton
    Left = 8
    Top = 56
    Width = 241
    Height = 25
    Caption = 'Connect to server'
    TabOrder = 2
    OnClick = btnConnectClick
  end
  object lbChannels: TListBox
    Left = 8
    Top = 192
    Width = 761
    Height = 164
    Style = lbOwnerDrawFixed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 20
    ParentFont = False
    TabOrder = 6
    OnDblClick = lbChannelsDblClick
    OnDrawItem = lbChannelsDrawItem
  end
  object btnEventRegister: TButton
    Left = 536
    Top = 152
    Width = 113
    Height = 25
    Caption = 'Register selected'
    TabOrder = 4
    OnClick = btnEventRegisterClick
  end
  object btnEventUnregister: TButton
    Left = 656
    Top = 152
    Width = 113
    Height = 25
    Caption = 'Unregister selected'
    TabOrder = 5
    OnClick = btnEventUnregisterClick
  end
  object stbStatusBar: TStatusBar
    Left = 0
    Top = 568
    Width = 776
    Height = 19
    Panels = <
      item
        Text = 'Not connected'
        Width = 400
      end>
  end
  object btnChannelRegister: TButton
    Left = 536
    Top = 360
    Width = 113
    Height = 25
    Caption = 'Register selected'
    TabOrder = 7
    OnClick = btnChannelRegisterClick
  end
  object btnChannelUnregister: TButton
    Left = 656
    Top = 360
    Width = 113
    Height = 25
    Caption = 'Unregister selected'
    TabOrder = 8
    OnClick = btnChannelUnregisterClick
  end
  object oXPManifest: TXPManifest
    Left = 736
  end
end
