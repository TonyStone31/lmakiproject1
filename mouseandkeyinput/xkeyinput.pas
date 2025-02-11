{ XKeyInput

  Copyright (C) 2008 Tom Gregorovic

  This source is free software; you can redistribute it and/or modify it under the terms of the
  GNU General Public License as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software
  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit XKeyInput;

{$mode objfpc}{$H+}
{$linklib Xtst}

interface

uses
  Classes,
  Controls,
  Forms,
  KeyInputIntf, KeySym,
  SysUtils,
  X,
  XLib;

type
  { TXKeyInput }

  TXKeyInput = class(TKeyInput)
  private
    xDisplayConnection: PDisplay;
    capsLockBeginState: boolean;
    function VirtualKeyToXKeySym(Key: word): TKeySym;
  protected
    procedure DoDown(Key: word); override;
    procedure DoUp(Key: word); override;
    procedure capsLockGetSaveState; override;
    procedure capsLockRestoreState; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function InitializeKeyInput: TKeyInput;
function XTestFakeKeyEvent(dpy: PDisplay; keycode: dword; is_press: Boolean32; delay: dword): longint; cdecl; external;

implementation

uses
  LCLType;

function InitializeKeyInput: TKeyInput;
begin
  Result := TXKeyInput.Create;
end;

procedure TXKeyInput.capsLockGetSaveState;
var
  keyboardState: TXKeyboardState;
begin
  XGetKeyboardControl(xDisplayConnection, @keyboardState);
  capsLockBeginState := (keyboardState.led_mask and 1) <> 0;

  if capsLockBeginState then
  begin
    XTestFakeKeyEvent(xDisplayConnection, XKeysymToKeycode(xDisplayConnection, XK_Caps_Lock), True, 0);
    XTestFakeKeyEvent(xDisplayConnection, XKeysymToKeycode(xDisplayConnection, XK_Caps_Lock), False, 0);
    Sleep(200);
  end;
end;

procedure TXKeyInput.capsLockRestoreState;
var
  keyboardState: TXKeyboardState;
begin
  if capsLockBeginState then
  begin
    XGetKeyboardControl(xDisplayConnection, @keyboardState);
    if (keyboardState.led_mask and 1) = 0 then
    begin
      XTestFakeKeyEvent(xDisplayConnection, XKeysymToKeycode(xDisplayConnection, XK_Caps_Lock), True, 0);
      XTestFakeKeyEvent(xDisplayConnection, XKeysymToKeycode(xDisplayConnection, XK_Caps_Lock), False, 0);
      Sleep(200);
    end;
  end;
end;

function TXKeyInput.VirtualKeyToXKeySym(Key: word): TKeySym;
begin
  case Key of
    VK_BACK: Result := XK_BackSpace;
    VK_TAB: Result := XK_Tab;
    VK_CLEAR: Result := XK_Clear;
    VK_RETURN: Result := XK_Return;
    VK_SHIFT: Result := XK_Shift_L;
    VK_CONTROL: Result := XK_Control_R;
    VK_MENU: Result := XK_Alt_R;
    //VK_RMENU: Result := XK_Alt_L;
    VK_CAPITAL: Result := XK_Caps_Lock;

    VK_ESCAPE: Result := XK_Escape;
    VK_SPACE: Result := XK_space;
    VK_PRIOR: Result := XK_Prior;
    VK_NEXT: Result := XK_Next;
    VK_END: Result := XK_End;
    VK_HOME: Result := XK_Home;
    VK_LEFT: Result := XK_Left;
    VK_UP: Result := XK_Up;
    VK_RIGHT: Result := XK_Right;
    VK_DOWN: Result := XK_Down;
    VK_SELECT: Result := XK_Select;
    VK_PRINT: Result := XK_Print;
    VK_EXECUTE: Result := XK_Execute;

    VK_INSERT: Result := XK_Insert;
    VK_DELETE: Result := XK_Delete;
    VK_HELP: Result := XK_Help;
    VK_0: Result := XK_0;
    VK_1: Result := XK_1;
    VK_2: Result := XK_2;
    VK_3: Result := XK_3;
    VK_4: Result := XK_4;
    VK_5: Result := XK_5;
    VK_6: Result := XK_6;
    VK_7: Result := XK_7;
    VK_8: Result := XK_8;
    VK_9: Result := XK_9;

    VK_A: Result := XK_a;
    VK_B: Result := XK_b;
    VK_C: Result := XK_c;
    VK_D: Result := XK_d;
    VK_E: Result := XK_e;
    VK_F: Result := XK_f;
    VK_G: Result := XK_g;
    VK_H: Result := XK_h;
    VK_I: Result := XK_i;
    VK_J: Result := XK_j;
    VK_K: Result := XK_k;
    VK_L: Result := XK_l;
    VK_M: Result := XK_m;
    VK_N: Result := XK_n;
    VK_O: Result := XK_o;
    VK_P: Result := XK_p;
    VK_Q: Result := XK_q;
    VK_R: Result := XK_r;
    VK_S: Result := XK_s;
    VK_T: Result := XK_t;
    VK_U: Result := XK_u;
    VK_V: Result := XK_v;
    VK_W: Result := XK_w;
    VK_X: Result := XK_x;
    VK_Y: Result := XK_y;
    VK_Z: Result := XK_z;

    VK_NUMPAD0: Result := XK_KP_0;
    VK_NUMPAD1: Result := XK_KP_1;
    VK_NUMPAD2: Result := XK_KP_2;
    VK_NUMPAD3: Result := XK_KP_3;
    VK_NUMPAD4: Result := XK_KP_4;
    VK_NUMPAD5: Result := XK_KP_5;
    VK_NUMPAD6: Result := XK_KP_6;
    VK_NUMPAD7: Result := XK_KP_7;
    VK_NUMPAD8: Result := XK_KP_8;
    VK_NUMPAD9: Result := XK_KP_9;
    VK_MULTIPLY: Result := XK_KP_Multiply;
    VK_ADD: Result := XK_KP_Add;
    VK_SEPARATOR: Result := XK_KP_Separator;
    VK_SUBTRACT: Result := XK_KP_Subtract;
    VK_DECIMAL: Result := XK_KP_Decimal;
    VK_DIVIDE: Result := XK_KP_Divide;
    VK_F1: Result := XK_F1;
    VK_F2: Result := XK_F2;
    VK_F3: Result := XK_F3;
    VK_F4: Result := XK_F4;
    VK_F5: Result := XK_F5;
    VK_F6: Result := XK_F6;
    VK_F7: Result := XK_F7;
    VK_F8: Result := XK_F8;
    VK_F9: Result := XK_F9;
    VK_F10: Result := XK_F10;
    VK_F11: Result := XK_F11;
    VK_F12: Result := XK_F12;
    VK_F13: Result := XK_F13;
    VK_F14: Result := XK_F14;
    VK_F15: Result := XK_F15;
    VK_F16: Result := XK_F16;
    VK_F17: Result := XK_F17;
    VK_F18: Result := XK_F18;
    VK_F19: Result := XK_F19;
    VK_F20: Result := XK_F20;
    VK_F21: Result := XK_F21;
    VK_F22: Result := XK_F22;
    VK_F23: Result := XK_F23;
    VK_F24: Result := XK_F24;
    VK_NUMLOCK: Result := XK_Num_Lock;
    VK_SCROLL: Result := XK_Scroll_Lock;

    VK_LCL_TILDE: Result := XK_grave;
    VK_LCL_MINUS: Result := XK_minus;
    VK_LCL_EQUAL: Result := XK_equal;
    VK_LCL_OPEN_BRACKET: Result := XK_bracketleft;
    VK_LCL_CLOSE_BRACKET: Result := XK_bracketright;
    VK_LCL_BACKSLASH: Result := XK_backslash;
    VK_LCL_SEMI_COMMA: Result := XK_semicolon;
    VK_LCL_QUOTE: Result := XK_quoteright;
    VK_LCL_COMMA: Result := XK_comma;
    VK_LCL_POINT: Result := XK_period;
    VK_LCL_SLASH: Result := XK_slash;

    else
      Result := XK_VoidSymbol;
  end;

end;

{ TXKeyInput }

constructor TXKeyInput.Create;
begin
  inherited Create;
  xDisplayConnection := XOpenDisplay(nil);
end;

destructor TXKeyInput.Destroy;
begin
  if xDisplayConnection <> nil then
    XCloseDisplay(xDisplayConnection);
  inherited Destroy;
end;

procedure TXKeyInput.DoDown(Key: word);
var
  KeySym: TKeySym;
begin
  KeySym := VirtualKeyToXKeySym(Key);
  if (KeySym = XK_VoidSymbol) or (xDisplayConnection = nil) then Exit;
  XTestFakeKeyEvent(xDisplayConnection, XKeysymToKeycode(xDisplayConnection, KeySym), True, 0);
  XFlush(xDisplayConnection);
end;

procedure TXKeyInput.DoUp(Key: word);
var
  KeySym: TKeySym;
begin
  KeySym := VirtualKeyToXKeySym(Key);
  if (KeySym = XK_VoidSymbol) or (xDisplayConnection = nil) then Exit;
  // key release
  XTestFakeKeyEvent(xDisplayConnection, XKeysymToKeycode(xDisplayConnection, KeySym), False, 0);
  XFlush(xDisplayConnection);
end;


end.
