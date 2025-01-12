//  This requires elevated permissions or user permission to uinput
unit WaylandMouseAndKeyInput;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix,
  Classes,
  Controls,
  KeyInputIntf,
  MouseInputIntf,
  SysUtils,
  Unix;

type
  TWaylandMouseInput = class(TMouseInput)
  private
    MouseFD: cint;
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: integer); override;
    procedure DoUp(Button: TMouseButton); override;
    procedure DoScrollUp; override;
    procedure DoScrollDown; override;
  end;

  TWaylandKeyInput = class(TKeyInput)
  private
    KeyboardFD: cint;
  protected
    procedure DoDown(Key: word); override;
    procedure DoUp(Key: word); override;
  end;

function InitializeWaylandMouseInput: TMouseInput;
function InitializeWaylandKeyInput: TKeyInput;

implementation

const
  // Event types and ioctl commands
  EV_SYN = $00;
  EV_KEY = $01;
  EV_REL = $02;
  REL_X = $00;
  REL_Y = $01;
  REL_WHEEL = $08;
  SYN_REPORT = $00;

  BTN_LEFT = $110;
  BTN_RIGHT = $111;
  BTN_MIDDLE = $112;

  UI_SET_EVBIT = $40045564;
  UI_SET_KEYBIT = $40045565;
  UI_SET_RELBIT = $40045566;
  UI_DEV_CREATE = $5501;
  UI_DEV_DESTROY = $5502;

procedure Emit(fd: cint; etype, ecode, Value: cint);
var
  ie: record
    time: TTimeVal;
    etype, ecode, Value: cuint16;
    end;
begin
  FillChar(ie, SizeOf(ie), 0);
  ie.etype := etype;
  ie.ecode := ecode;
  ie.Value := Value;
  fpWrite(fd, ie, SizeOf(ie));
end;

{ TWaylandMouseInput }

procedure TWaylandMouseInput.DoDown(Button: TMouseButton);
var
  BtnCode: cint;
begin
  case Button of
    mbLeft: BtnCode := BTN_LEFT;
    mbRight: BtnCode := BTN_RIGHT;
    mbMiddle: BtnCode := BTN_MIDDLE;
    else
      Exit;
  end;
  Emit(MouseFD, EV_KEY, BtnCode, 1);
  Emit(MouseFD, EV_SYN, SYN_REPORT, 0);
end;

procedure TWaylandMouseInput.DoMove(ScreenX, ScreenY: integer);
begin
  Emit(MouseFD, EV_REL, REL_X, ScreenX);
  Emit(MouseFD, EV_REL, REL_Y, ScreenY);
  Emit(MouseFD, EV_SYN, SYN_REPORT, 0);
end;

procedure TWaylandMouseInput.DoUp(Button: TMouseButton);
var
  BtnCode: cint;
begin
  case Button of
    mbLeft: BtnCode := BTN_LEFT;
    mbRight: BtnCode := BTN_RIGHT;
    mbMiddle: BtnCode := BTN_MIDDLE;
    else
      Exit;
  end;
  Emit(MouseFD, EV_KEY, BtnCode, 0);
  Emit(MouseFD, EV_SYN, SYN_REPORT, 0);
end;

procedure TWaylandMouseInput.DoScrollUp;
begin
  Emit(MouseFD, EV_REL, REL_WHEEL, 1);
  Emit(MouseFD, EV_SYN, SYN_REPORT, 0);
end;

procedure TWaylandMouseInput.DoScrollDown;
begin
  Emit(MouseFD, EV_REL, REL_WHEEL, -1);
  Emit(MouseFD, EV_SYN, SYN_REPORT, 0);
end;

{ TWaylandKeyInput }

procedure TWaylandKeyInput.DoDown(Key: word);
begin
  Emit(KeyboardFD, EV_KEY, Key, 1);
  Emit(KeyboardFD, EV_SYN, SYN_REPORT, 0);
end;

procedure TWaylandKeyInput.DoUp(Key: word);
begin
  Emit(KeyboardFD, EV_KEY, Key, 0);
  Emit(KeyboardFD, EV_SYN, SYN_REPORT, 0);
end;

{ Initialize Input }

function InitializeWaylandMouseInput: TMouseInput;
begin
  Result := TWaylandMouseInput.Create;
end;

function InitializeWaylandKeyInput: TKeyInput;
begin
  Result := TWaylandKeyInput.Create;
end;

end.
