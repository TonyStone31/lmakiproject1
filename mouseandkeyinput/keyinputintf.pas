{ KeyInputIntf

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
unit KeyInputIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  { TKeyInput }

  TKeyInput = class
  protected
    procedure DoDown(Key: word); dynamic; abstract;
    procedure DoUp(Key: word); dynamic; abstract;
  public
    procedure Down(Key: word);
    procedure Up(Key: word);

    procedure Press(Key: word);
    procedure Press(StringValue: string);

    procedure Apply(Shift: TShiftState);
    procedure Unapply(Shift: TShiftState);
  end;

implementation

uses LCLType;

{ TKeyInput }

procedure TKeyInput.Down(Key: word);
begin
  DoDown(Key);
  Application.ProcessMessages;
end;

procedure TKeyInput.Up(Key: word);
begin
  DoUp(Key);
  Application.ProcessMessages;
end;

procedure TKeyInput.Press(Key: word);
begin
  Down(Key);
  Up(Key);
  Sleep(1);
end;

procedure TKeyInput.Press(StringValue: string);
var
  i: integer;
  needShiftKey: boolean = False;
  keySym: word;
  ch: char;
begin
  i := 1;
  while (i <= Length(StringValue)) do
  begin
    if StringValue[i] in ['A'..'Z', '~', '!', '@', '#', '$', '%', '^',
      '&', '*', '(', ')', '_', '+', '{', '}', '|', ':', '"', '<', '>', '?'] then
      needShiftKey := True;

    ch := UpCase(StringValue[i]);
    case ch of
      'A'..'Z': keySym := VK_A + (Ord(ch) - Ord('A'));
      '0'..'9': keySym := VK_0 + (Ord(ch) - Ord('0'));

      '!': keySym := VK_1;
      '@': keySym := VK_2;
      '#': keySym := VK_3;
      '$': keySym := VK_4;
      '%': keySym := VK_5;
      '^': keySym := VK_6;
      '&': keySym := VK_7;
      '*': keySym := VK_8;
      '(': keySym := VK_9;
      ')': keySym := VK_0;

      '~', '`': keySym := VK_LCL_TILDE;
      '_', '-': keySym := VK_LCL_MINUS; //$2d;
      '+', '=': keySym := VK_LCL_EQUAL; //$3d;
      '{', '[': keySym := VK_LCL_OPEN_BRACKET; //$5b;
      '}', ']': keySym := VK_LCL_CLOSE_BRACKET; //$5d;
      '|', '\': keySym := VK_LCL_BACKSLASH; //$5c;
      ':', ';': keySym := VK_LCL_SEMI_COMMA; //$3b;
      '"', '''': keySym := VK_LCL_QUOTE; //$27;
      '<', ',': keySym := VK_LCL_COMMA; //$2c;
      '>', '.': keySym := VK_LCL_POINT; //$2e;
      '?', '/': keySym := VK_LCL_SLASH; //$2f;

      #32: keySym := VK_SPACE;
      #9: keySym := VK_TAB;

      {$IFDEF Windows}
      #13: keySym := VK_RETURN; // On Windows, use CR
      {$ELSE}
      #10: keySym := VK_RETURN; // On Linux/Mac, use LF
      {$ENDIF}
    else
      keySym := VK_SPACE; // Fallback for unmapped characters
    end;

    if needShiftKey then Apply([ssShift]);
    Press(keySym);

    //if (needShiftKey) or (keySym = VK_RETURN) then // Testing something on Windows
    if (needShiftKey) then
    begin
      Unapply([ssShift]);
      needShiftKey := False;
    end;

    Inc(i);
  end;
end;

procedure TKeyInput.Apply(Shift: TShiftState);
begin
  if ssCtrl in Shift then Down(VK_CONTROL);
  if ssAlt in Shift then Down(VK_MENU);
  if ssShift in Shift then Down(VK_SHIFT);
end;

procedure TKeyInput.Unapply(Shift: TShiftState);
begin
  if ssShift in Shift then Up(VK_SHIFT);
  if ssCtrl in Shift then Up(VK_CONTROL);
  if ssAlt in Shift then Up(VK_MENU);
end;

end.