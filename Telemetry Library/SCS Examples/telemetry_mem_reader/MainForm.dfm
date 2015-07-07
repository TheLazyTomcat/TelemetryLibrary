object fMainForm: TfMainForm
  Left = 458
  Top = 121
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Telemetry Mem Reader'
  ClientHeight = 570
  ClientWidth = 812
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object imgMainImage: TImage
    Left = 0
    Top = 0
    Width = 812
    Height = 551
    Align = alClient
  end
  object stbStatusBar: TStatusBar
    Left = 0
    Top = 551
    Width = 812
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Panels = <
      item
        Alignment = taRightJustify
        Text = 
          'Telemetry Library - Telemetry Mem Reader Example, '#169' Franti'#353'ek Mi' +
          'lt 2014'
        Width = 150
      end>
    UseSystemFont = False
  end
  object tmrReadTimer: TTimer
    Interval = 50
    OnTimer = tmrReadTimerTimer
    Left = 8
    Top = 8
  end
end
