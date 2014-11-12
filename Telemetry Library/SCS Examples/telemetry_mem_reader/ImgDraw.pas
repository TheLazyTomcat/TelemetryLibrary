// ToDo: needs some serious code inspection ;)
unit ImgDraw;

interface

{$INCLUDE '..\..\Source\Telemetry_defs.inc'}

uses
  SysUtils,
  Graphics,
  TelemetrySCS_Examples_telemetry_mem,
  TelemetryStrings,
{$IFDEF UseCondensedHeader}
  SCS_Telemetry_Condensed;
{$ELSE}
  scssdk_value;
{$ENDIF}

type
{==============================================================================}
{------------------------------------------------------------------------------}
{                                  TImgDrawer                                  }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{    TImgDrawer // Class declaration                                           }
{==============================================================================}
  TImgDrawer = class(TObject)
  private
    fFormatSettings:  TFormatSettings;
    fImgNoData:       TBitmap;
    fImgOutput:       TBitmap;
    fDefaultFont:     TFont;
    fStoredState:     TSCSExm_TelemetryMemState;
  protected
    procedure InitImages; virtual;
    procedure DrawOutput; virtual;
  public
    constructor Create(Width, Height: Integer; DefaultFont: TFont);
    destructor Destroy; override;
    procedure Update(State: TSCSExm_TelemetryMemState); virtual;
  published
    property ImgNoData: TBitmap read fImgNoData;
    property ImgOutput: TBitmap read fImgOutput;
  end;

implementation

uses
  Windows, Types, Math;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                  TImgDrawer                                  }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{    TImgDrawer // Class implementation                                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{    Constants, types, variables, etc...                                       }
{------------------------------------------------------------------------------}
const
  ndt_FontName  = 'Courier New';
  ndt_FontSize  = 10;
  ndt_FontColor = clRed;
  ndt_FontStyle = [fsBold];
  ndt_Text      = 'No data available.';

  img_BackgroundColor   = clWhite;
  img_Spacing           = 10;
  img_TextSpacing       = 5;
  img_ThrottleColor1    = $0000D941;
  img_ThrottleColor2    = $00008A29;
  img_BreakColor1       = $003535FF;
  img_BreakColor2       = $000000B3;
  img_ClutchColor1      = $005BFFFF;
  img_ClutchColor2      = $0000A8A8;
  img_InactiveBoxWidth  = 100;
  img_InactiveBoxHeight = 30;
  img_InactiveBoxColor  = $000000AE;
  img_InactiveTextColor = clWhite;
  img_InactiveText      = 'Paused';

  txt_FontName     = 'Courier New';
  txt_CapSpeedPosX = img_Spacing;
  txt_CapRPMPosX   = txt_CapSpeedPosX + 150;
  txt_CapGearPosX  = txt_CapRPMPosX + 150;

  spl_SplitterColor = clSilver;

  sbar_BarHeight         = 15;
  sbar_RightWindowWidth  = 45;
  sbar_FontName          = 'Courier New';
  sbar_FontSize          = 8;
  sbar_CapFontColor      = clWindowText;
  sbar_OutlineColor      = clBlack;
  sbar_FillColor         = $00F0F0F0;
  sbar_BarColor1         = $00FF2424;
  sbar_BarColor2         = $00660000;
  sbar_PosLineHalfLength = 3;
  sbar_PercFontColor     = clWhite;
  sbar_Caption           = 'Steering position:';

  bar_OutlineColor      = clBlack;
  bar_FillColor         = $00F0F0F0;
  bar_RightWindowWidth  = 45;
  bar_BarHeight         = 15;
  bar_FontName          = 'Courier New';
  bar_FontSize          = 8;
  bar_CapFontColor      = clWindowText;
  bar_PercFontColor     = clWhite;
  bar_PosLineHalfLength = 3;

  wbar_OutlineColor      = clBlack;
  wbar_FillColor         = $00F0F0F0;
  wbar_BarWidth          = 90;
  wbar_BarHeight         = 90;
  wbar_ValWindowHeight   = 15;
  wbar_FontName          = 'Courier New';
  wbar_FontSize          = 8;
  wbar_CapFontColor      = clWindowText;
  wbar_ValFontColor      = clWhite;
  wbar_PosLineHalfLength = 3;
  wbar_BarColor1         = $00AEAEAE;
  wbar_BarColor2         = $00444444;
  wbar_InactiveColor     = clRed;

{------------------------------------------------------------------------------}
{    TImgDrawer // Protected methods                                           }
{------------------------------------------------------------------------------}

procedure TImgDrawer.InitImages;
var
  TempPt: TPoint;
begin
// prepare no data image
fImgNoData.Canvas.Pen.Style := psSolid;
fImgNoData.Canvas.Pen.Color := img_BackgroundColor;
fImgNoData.Canvas.Brush.Style := bsSolid;
fImgNoData.Canvas.Brush.Color := img_BackgroundColor;
fImgNoData.Canvas.Font := fDefaultFont;
fImgNoData.Canvas.Font.Name := ndt_FontName;
fImgNoData.Canvas.Font.Size := ndt_FontSize;
fImgNoData.Canvas.Font.Color := ndt_FontColor;
fImgNoData.Canvas.Font.Style := ndt_FontStyle;

// fill background
fImgNoData.Canvas.FillRect(Rect(0,0,fImgNoData.Width,fImgNoData.Height));

// draw text
TempPt.X := (fImgNoData.Width - fImgNoData.Canvas.TextWidth(ndt_Text)) div 2;
TempPt.Y := (fImgNoData.Height - fImgNoData.Canvas.TextHeight(ndt_Text)) div 2;
fImgNoData.Canvas.TextOut(TempPt.X,TempPt.Y,ndt_Text);
end;

procedure TImgDrawer.DrawOutput;
var
  TempInt:  Integer;
  TempPt:   TPoint;
  i,j:      Integer;
  TempStr:  String;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Function DrawSplitter(Origin: Integer): Integer;
  const
    spl_SplitterColor = clSilver;
  begin
    // draw splitter line
    fImgOutput.Canvas.Pen.Color := spl_SplitterColor;
    fImgOutput.Canvas.MoveTo(img_Spacing,Origin);
    fImgOutput.Canvas.LineTo(fImgOutput.Width - img_Spacing,Origin);
    Result := Origin;
  end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Function DrawSteeringBar(Origin: Integer): Integer;
  var
    _TempPt:  TPoint;
    _TempStr: String;    
    _Center:  Integer;
  begin
    // draw caption
    fImgOutput.Canvas.Font.Name := sbar_FontName;
    fImgOutput.Canvas.Font.Size := sbar_FontSize;
    fImgOutput.Canvas.Font.Style := [];
    fImgOutput.Canvas.Font.Color := sbar_CapFontColor;
    fImgOutput.Canvas.Brush.Style := bsClear;
    fImgOutput.Canvas.TextOut(img_Spacing,Origin,sbar_Caption);
    _TempPt.Y := fImgOutput.Canvas.TextHeight(sbar_Caption) + Origin + img_TextSpacing;
    Result := _TempPt.Y + + sbar_BarHeight;

    // draw main rectangle
    fImgOutput.Canvas.Pen.Style := psSolid;
    fImgOutput.Canvas.Pen.Color := sbar_OutlineColor;
    fImgOutput.Canvas.Brush.Style := bsSolid;
    fImgOutput.Canvas.Brush.Color := sbar_FillColor;
    _TempPt.X := fImgOutput.Width - img_Spacing;
    fImgOutput.Canvas.Rectangle(img_Spacing,_TempPt.Y,_TempPt.X,_TempPt.Y + sbar_BarHeight);

    // draw right window
    fImgOutput.Canvas.Brush.Color := sbar_BarColor2;
    fImgOutput.Canvas.Rectangle(_TempPt.X - sbar_RightWindowWidth,_TempPt.Y,_TempPt.X,_TempPt.Y + sbar_BarHeight);

    // get center
    _Center := (fImgOutput.Width - 2 * img_Spacing - sbar_RightWindowWidth) div 2;

    // draw position bar
    fImgOutput.Canvas.Pen.Color := sbar_BarColor1;
    fImgOutput.Canvas.Brush.Color := sbar_BarColor1;
    If fStoredState.Steering < 0 then
      begin
        _TempPt.X := _Center + Trunc(_Center * Abs(fStoredState.Steering)) + 1;
        fImgOutput.Canvas.Rectangle(img_Spacing + _Center,_TempPt.Y + 1,img_Spacing + _TempPt.X,_TempPt.Y + sbar_BarHeight);
      end
    else
      begin
        _TempPt.X := _Center - Trunc(_Center * Abs(fStoredState.Steering)) + 1;
        fImgOutput.Canvas.Rectangle(img_Spacing + _TempPt.X,_TempPt.Y + 1,img_Spacing + _Center,_TempPt.Y + sbar_BarHeight - 1);
      end;

    // draw center line
    fImgOutput.Canvas.Pen.Color := sbar_OutlineColor;
    fImgOutput.Canvas.MoveTo(img_Spacing + _Center,_TempPt.Y - 2);
    fImgOutput.Canvas.LineTo(img_Spacing + _Center,_TempPt.Y + sbar_BarHeight + 2);

    // draw percentage
    fImgOutput.Canvas.Font.Color := sbar_PercFontColor;
    fImgOutput.Canvas.Brush.Style := bsClear;
    _TempStr := IntToStr(Trunc(fStoredState.Steering * 100)) + '%';
    _TempPt.X := fImgOutput.Width - img_Spacing - img_TextSpacing - fImgOutput.Canvas.TextWidth(_TempStr);
    _TempPt.Y := _TempPt.Y + (sbar_BarHeight - fImgOutput.Canvas.TextHeight(_TempStr)) div 2;
    fImgOutput.Canvas.TextOut(_TempPt.X,_TempPt.Y,_TempStr);
  end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Function DrawBar(Origin: Integer; Color1, Color2: TColor; Caption: String; Value: Single): Integer;
  var
    _TempStr: String;
    _TempPt:  TPoint;
  begin
    // draw caption
    fImgOutput.Canvas.Font.Name := bar_FontName;
    fImgOutput.Canvas.Font.Size := bar_FontSize;
    fImgOutput.Canvas.Font.Style := [];
    fImgOutput.Canvas.Font.Color := bar_CapFontColor;
    fImgOutput.Canvas.Brush.Style := bsClear;
    fImgOutput.Canvas.TextOut(img_Spacing,Origin,Caption);
    _TempPt.Y := fImgOutput.Canvas.TextHeight(Caption) + Origin + img_TextSpacing;
    Result := _TempPt.Y + + bar_BarHeight;

    // draw main rectangle
    fImgOutput.Canvas.Pen.Style := psSolid;
    fImgOutput.Canvas.Pen.Color := bar_OutlineColor;
    fImgOutput.Canvas.Brush.Style := bsSolid;
    fImgOutput.Canvas.Brush.Color := bar_FillColor;
    _TempPt.X := fImgOutput.Width - img_Spacing;
    fImgOutput.Canvas.Rectangle(img_Spacing,_TempPt.Y,_TempPt.X,_TempPt.Y + bar_BarHeight);

    // draw right window
    fImgOutput.Canvas.Brush.Color := Color2;
    fImgOutput.Canvas.Rectangle(_TempPt.X - bar_RightWindowWidth,_TempPt.Y,_TempPt.X,_TempPt.Y + bar_BarHeight);

    // draw bar
    fImgOutput.Canvas.Pen.Color := Color1;
    fImgOutput.Canvas.Brush.Color := Color1;
    _TempPt.X := img_Spacing + Trunc((_TempPt.X - bar_RightWindowWidth - img_Spacing - 1) * Value) + 1;
    fImgOutput.Canvas.Rectangle(img_Spacing + 1,_TempPt.Y + 1,_TempPt.X,_TempPt.Y + bar_BarHeight - 1);

    // draw percentage
    fImgOutput.Canvas.Font.Color := bar_PercFontColor;
    fImgOutput.Canvas.Brush.Style := bsClear;
    _TempStr := IntToStr(Trunc(Value * 100)) + '%';
    _TempPt.X := fImgOutput.Width - img_Spacing - img_TextSpacing - fImgOutput.Canvas.TextWidth(_TempStr);
    _TempPt.Y := _TempPt.Y + (bar_BarHeight - fImgOutput.Canvas.TextHeight(_TempStr)) div 2;
    fImgOutput.Canvas.TextOut(_TempPt.X,_TempPt.Y,_TempStr);
  end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

  Function DrawWheelDeflection(Origin,Index: Integer; Active: Boolean): Integer;
  var
    _TempStr: String;
    _TempPt:  TPoint;
    _Scale:   Integer;
    _TempInt: Integer;
    _TempVal: Single;
    _Center:  Integer;
  begin
    // draw caption
    fImgOutput.Canvas.Font.Name := wbar_FontName;
    fImgOutput.Canvas.Font.Size := wbar_FontSize;
    fImgOutput.Canvas.Font.Style := [];
    fImgOutput.Canvas.Font.Color := wbar_CapFontColor;
    fImgOutput.Canvas.Brush.Style := bsClear;
    _TempStr := 'Wheel #' + IntToStr(Index);
    _TempPt.X := img_Spacing + Index * (wbar_BarWidth + img_Spacing);
    _TempPt.Y := fImgOutput.Canvas.TextHeight(_TempStr) + Origin + img_TextSpacing;
    fImgOutput.Canvas.TextOut(_TempPt.X,Origin,_TempStr);
    Result := _TempPt.Y + wbar_ValWindowHeight + wbar_BarHeight;

    // draw value window
    fImgOutput.Canvas.Pen.Style := psSolid;
    fImgOutput.Canvas.Pen.Color := wbar_OutlineColor;
    fImgOutput.Canvas.Brush.Style := bsSolid;
    fImgOutput.Canvas.Brush.Color := wbar_BarColor2;
    fImgOutput.Canvas.Rectangle(_TempPt.X,_TempPt.Y,_TempPt.X + wbar_BarWidth,_TempPt.Y + wbar_ValWindowHeight);

    // draw main rectangle
    fImgOutput.Canvas.Brush.Color := wbar_FillColor;
    fImgOutput.Canvas.Rectangle(_TempPt.X,_TempPt.Y + wbar_ValWindowHeight - 1,_TempPt.X + wbar_BarWidth,_TempPt.Y + wbar_ValWindowHeight + wbar_BarHeight);

    If Active then
      begin
        // get variable scale
        If fStoredState.WheelDeflections[Index] <> 0 then
          _Scale := Ceil(Log10(Abs(fStoredState.WheelDeflections[Index] * 1000)))
        else
          _Scale := 0;

        // get center
        _Center := _TempPt.Y + wbar_ValWindowHeight + wbar_BarHeight div 2;

        // draw position bar
        _TempVal := Abs(fStoredState.WheelDeflections[Index] * Power(10,-_Scale) * 1000);
        fImgOutput.Canvas.Pen.Color := wbar_BarColor1;
        fImgOutput.Canvas.Brush.Color := wbar_BarColor1;
        If fStoredState.WheelDeflections[Index] > 0 then
          begin
            _TempInt := Trunc((wbar_BarHeight div 2) * _TempVal);
            fImgOutput.Canvas.Rectangle(_TempPt.X + 1,_Center - _TempInt,_TempPt.X + wbar_BarWidth - 1,_Center);
          end
        else
          begin
            _TempInt := Trunc((wbar_BarHeight div 2 - 1) * _TempVal);
            fImgOutput.Canvas.Rectangle(_TempPt.X + 1,_Center + 1,_TempPt.X + wbar_BarWidth - 1,_Center + _TempInt);
          end;

        // draw center line
        fImgOutput.Canvas.Pen.Color := wbar_OutlineColor;
        fImgOutput.Canvas.MoveTo(_TempPt.X - 2,_Center);
        fImgOutput.Canvas.LineTo(_TempPt.X + wbar_BarWidth + 2,_Center);

        // draw actual value
        fImgOutput.Canvas.Font.Color := wbar_ValFontColor;
        fImgOutput.Canvas.Brush.Style := bsClear;
        _TempStr := ValueToStr(fStoredState.WheelDeflections[Index],SCS_VALUE_TYPE_float,ffFixed,15,6,fFormatSettings) + ' m';
        fImgOutput.Canvas.TextOut(img_Spacing + Index * (wbar_BarWidth + img_Spacing) + wbar_BarWidth - fImgOutput.Canvas.TextWidth(_TempStr) - img_TextSPacing,fImgOutput.Canvas.TextHeight(_TempStr) + Origin + img_TextSpacing,_TempStr);

        // draw scale info
        fImgOutput.Canvas.Font.Color := wbar_CapFontColor;
        _TempStr := FloatToStr(Power(10,_Scale),fFormatSettings) + ' mm';
        fImgOutput.Canvas.TextOut(_TempPt.X + img_TextSpacing,_TempPt.Y + wbar_ValWindowHeight,_TempStr);
      end
    else
      begin
        // draw inactive cross
        fImgOutput.Canvas.Pen.Color := wbar_InactiveColor;
        fImgOutput.Canvas.MoveTo(_TempPt.X,_TempPt.Y + wbar_ValWindowHeight - 1);
        fImgOutput.Canvas.LineTo(_TempPt.X + wbar_BarWidth,_TempPt.Y + wbar_ValWindowHeight + wbar_BarHeight);
        fImgOutput.Canvas.MoveTo(_TempPt.X,_TempPt.Y + wbar_ValWindowHeight + wbar_BarHeight);
        fImgOutput.Canvas.LineTo(_TempPt.X + wbar_BarWidth,_TempPt.Y + wbar_ValWindowHeight - 1);
      end;
  end;


begin
// fill background
fImgOutput.Canvas.Brush.Style := bsSolid;
fImgOutput.Canvas.Brush.Color := img_BackgroundColor;
fImgOutput.Canvas.FillRect(Rect(0,0,fImgOutput.Width,fImgOutput.Height));

// draw speed, rpm and gear info
fImgOutput.Canvas.Font := fDefaultFont;
fImgOutput.Canvas.Font.Name := txt_FontName;
fImgOutput.Canvas.Font.Size := 8;
fImgOutput.Canvas.Font.Style := [];
fImgOutput.Canvas.Brush.Style := bsClear;
fImgOutput.Canvas.TextOut(txt_CapSpeedPosX,img_Spacing,'Speed:');
fImgOutput.Canvas.TextOut(txt_CapRPMPosX,img_Spacing,'Engine RPM:');
fImgOutput.Canvas.TextOut(txt_CapGearPosX,img_Spacing,'Gear:');
TempInt := img_Spacing + fImgOutput.Canvas.TextHeight('0') + img_TextSpacing;
fImgOutput.Canvas.Font.Size := 15;
fImgOutput.Canvas.Font.Style := [fsBold];
TempStr := IntToStr(Trunc(Abs(fStoredState.SpeedometerSpeed * 3.6))) + ' km/h';
fImgOutput.Canvas.TextOut(txt_CapSpeedPosX + 100 - fImgOutput.Canvas.TextWidth(TempStr),TempInt,TempStr);
TempStr := IntToStr(Trunc(fStoredState.RPM)) + ' rpm';
fImgOutput.Canvas.TextOut(txt_CapRPMPosX + 100 - fImgOutput.Canvas.TextWidth(TempStr),TempInt,TempStr);
If fStoredState.Gear > 0 then TempStr := 'D' + IntToStr(fStoredState.Gear)
  else If fStoredState.Gear < 0 then TempStr := 'R' + IntToStr(Abs(fStoredState.Gear))
    else TempStr := 'N';
fImgOutput.Canvas.TextOut(txt_CapGearPosX + 50 - fImgOutput.Canvas.TextWidth(TempStr),TempInt,TempStr);
TempInt := TempInt + fImgOutput.Canvas.TextHeight('0');

// draw inactive warning
If fStoredState.Running = 0 then
  begin
    fImgOutput.Canvas.Font.Size := 15;
    fImgOutput.Canvas.Font.Style := [fsBold];
    fImgOutput.Canvas.Font.Color := img_InactiveTextColor;
    fImgOutput.Canvas.Pen.Color := img_InactiveBoxColor;
    fImgOutput.Canvas.Brush.Style := bsSolid;
    fImgOutput.Canvas.Brush.Color := img_InactiveBoxColor;
    fImgOutput.Canvas.Rectangle(fImgOutput.Width - img_Spacing - img_InactiveBoxWidth,img_Spacing,fImgOutput.Width - img_Spacing,img_Spacing + img_InactiveBoxHeight);
    TempPt.X := (img_InactiveBoxWidth - fImgOutput.Canvas.TextWidth(img_InactiveText)) div 2;
    TempPt.Y := (img_InactiveBoxHeight - fImgOutput.Canvas.TextHeight(img_InactiveText)) div 2;
    fImgOutput.Canvas.TextOut(fImgOutput.Width - img_Spacing - 100 + TempPt.X,img_Spacing + TempPt.Y,img_InactiveText);
  end;

// draw splitter
TempInt := DrawSplitter(TempInt + img_Spacing);

// draw steering bar
TempInt := DrawSteeringBar(TempInt + img_Spacing);

// draw splitter
TempInt := DrawSplitter(TempInt + img_Spacing);

// draw throttle, break and clutch bars
TempInt := DrawBar(TempInt + img_Spacing,img_ThrottleColor1,img_ThrottleColor2,'Effective throttle:',fStoredState.Throttle);
TempInt := DrawBar(TempInt + img_Spacing,img_BreakColor1,img_BreakColor2,'Effective break:',fStoredState.Brake);
TempInt := DrawBar(TempInt + img_Spacing,img_ClutchColor1,img_ClutchColor2,'Effective clutch:',fStoredState.Clutch);

// draw splitter
TempInt := DrawSplitter(TempInt + img_Spacing);

For i := Low(fStoredState.WheelDeflections) to High(fStoredState.WheelDeflections) do
  j := DrawWheelDeflection(TempInt + img_Spacing,i,LongWord(i) < fStoredState.WheelCount);
TempInt := j;

// draw splitter
TempInt := DrawSplitter(TempInt + img_Spacing);

// draw other info
fImgOutput.Canvas.Font.Size := 8;
fImgOutput.Canvas.Font.Style := [];
fImgOutput.Canvas.Font.Color := clWindowText;
fImgOutput.Canvas.Brush.Style := bsClear;
TempStr := 'Truck placement: ' + ValueToStr(fStoredState.WSTruckPlacement,SCS_VALUE_TYPE_dplacement,ffFixed,15,6,fFormatSettings);
fImgOutput.Canvas.TextOut(img_Spacing,TempInt + img_Spacing,TempStr);
TempInt := TempInt + img_TextSpacing + fImgOutput.Canvas.TextHeight(TempStr);
TempStr := 'Linear velocity [m/s]: ' + ValueToStr(fStoredState.LinearVelocity,SCS_VALUE_TYPE_fVector,ffFixed,15,6,fFormatSettings);
fImgOutput.Canvas.TextOut(img_Spacing,TempInt + img_Spacing,TempStr);
TempInt := TempInt + img_TextSpacing + fImgOutput.Canvas.TextHeight(TempStr);
TempStr := 'Angular velocity [s^-1]: ' + ValueToStr(fStoredState.AngularVelocity,SCS_VALUE_TYPE_fVector,ffFixed,15,6,fFormatSettings);
fImgOutput.Canvas.TextOut(img_Spacing,TempInt + img_Spacing,TempStr);
TempInt := TempInt + img_TextSpacing + fImgOutput.Canvas.TextHeight(TempStr);
TempStr := 'Linear acceleration [m/s^2]: ' + ValueToStr(fStoredState.LinearAcceleration,SCS_VALUE_TYPE_fVector,ffFixed,15,6,fFormatSettings);
fImgOutput.Canvas.TextOut(img_Spacing,TempInt + img_Spacing,TempStr);
TempInt := TempInt + img_TextSpacing + fImgOutput.Canvas.TextHeight(TempStr);
TempStr := 'Angular acceleration [s^-2]: ' + ValueToStr(fStoredState.AngularAcceleration,SCS_VALUE_TYPE_fVector,ffFixed,15,6,fFormatSettings);
fImgOutput.Canvas.TextOut(img_Spacing,TempInt + img_Spacing,TempStr);
TempInt := TempInt + img_TextSpacing + fImgOutput.Canvas.TextHeight(TempStr);
TempStr := 'Cabin angular velocity [s^-1]: ' + ValueToStr(fStoredState.CabinAngularVelocity,SCS_VALUE_TYPE_fVector,ffFixed,15,6,fFormatSettings);
fImgOutput.Canvas.TextOut(img_Spacing,TempInt + img_Spacing,TempStr);
TempInt := TempInt + img_TextSpacing + fImgOutput.Canvas.TextHeight(TempStr);
TempStr := 'Cabin angular acceleration [s^-2]: ' + ValueToStr(fStoredState.CabinAngularAcceleration,SCS_VALUE_TYPE_fVector,ffFixed,15,6,fFormatSettings);
fImgOutput.Canvas.TextOut(img_Spacing,TempInt + img_Spacing,TempStr);
end;

{------------------------------------------------------------------------------}
{    TImgDrawer // Public methods                                              }
{------------------------------------------------------------------------------}

constructor TImgDrawer.Create(Width, Height: Integer; DefaultFont: TFont);
begin
inherited Create;
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,fFormatSettings);
fFormatSettings.DecimalSeparator := '.';
fImgNoData := Graphics.TBitmap.Create;
fImgNoData.Width := Width;
fImgNoData.Height := Height;
fImgOutput := Graphics.TBitmap.Create;
fImgOutput.Width := Width;
fImgOutput.Height := Height;
fDefaultFont := DefaultFont;
InitImages;
end;

//------------------------------------------------------------------------------

destructor TImgDrawer.Destroy;
begin
fImgOutput.Free;
fImgNoData.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TImgDrawer.Update(State: TSCSExm_TelemetryMemState);
begin
fStoredState := State;
DrawOutput;
end;

end.
