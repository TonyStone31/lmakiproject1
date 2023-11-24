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
  Classes, SysUtils, Forms, LazUTF8;

type
  { TKeyInput }

  TKeyInput = class
  private

  protected
    procedure DoDown(Key: word); dynamic; abstract;
    procedure DoUp(Key: word); dynamic; abstract;
    function CharToKeySym(ch: char): word;
    function NeedsShift(ch: char): boolean;
    procedure PressStringUCchar(CharValue: string);
  public
    procedure Down(Key: word);
    procedure Up(Key: word);

    procedure Press(Key: word);
    procedure PressString(StringValue: string);
    procedure PressStringUC(StringValue: string);

    procedure Apply(Shift: TShiftState);
    procedure Unapply(Shift: TShiftState);
    procedure PressUnicodeChar(unicode: cardinal);
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
  //Sleep(1);
end;

function TKeyInput.NeedsShift(ch: char): boolean;
begin
  Result := ch in ['A'..'Z', '~', '!', '@', '#', '$', '%', '^', '&',
    '*', '(', ')', '_', '+', '{', '}', '|', ':', '"', '<', '>', '?'];
end;

function TKeyInput.CharToKeySym(ch: char): word;
begin
  ch := UpCase(ch);
  case ch of
    'A'..'Z': Result := VK_A + (Ord(ch) - Ord('A'));
    '0'..'9': Result := VK_0 + (Ord(ch) - Ord('0'));
    '!': Result := VK_1;
    '@': Result := VK_2;
    '#': Result := VK_3;
    '$': Result := VK_4;
    '%': Result := VK_5;
    '^': Result := VK_6;
    '&': Result := VK_7;
    '*': Result := VK_8;
    '(': Result := VK_9;
    ')': Result := VK_0;
    '~', '`': Result := VK_LCL_TILDE;
    '_', '-': Result := VK_LCL_MINUS;
    '+', '=': Result := VK_LCL_EQUAL;
    '{', '[': Result := VK_LCL_OPEN_BRACKET;
    '}', ']': Result := VK_LCL_CLOSE_BRACKET;
    '|', '\': Result := VK_LCL_BACKSLASH;
    ':', ';': Result := VK_LCL_SEMI_COMMA;
    '"', '''': Result := VK_LCL_QUOTE;
    '<', ',': Result := VK_LCL_COMMA;
    '>', '.': Result := VK_LCL_POINT;
    '?', '/': Result := VK_LCL_SLASH;
    #32: Result := VK_SPACE;
    #9: Result := VK_TAB;
    {$IFDEF Windows}
    #13: Result := VK_RETURN;
      {$ELSE}
    #10: Result := VK_RETURN;
      {$ENDIF}
    else
      Result := VK_SPACE; // Default fallback
  end;
end;

procedure TKeyInput.PressUnicodeChar(unicode: cardinal);
var
  unicodestring: string;
  j: integer;
  keyCode: word;
begin
  unicodestring := IntToHex(unicode, 4); // Convert to hex string

  {$IFDEF Linux}
  // Linux implementation using Ctrl+Shift+U
  Apply([ssCtrl, ssShift]);
  Press(VK_U); // Press 'U' after Ctrl+Shift
  //Sleep(5);
  for j := 1 to Length(unicodeString) do begin
    if unicodeString[j] in ['0'..'9'] then
      keyCode := VK_0 + Ord(unicodeString[j]) - Ord('0')
    else if unicodeString[j] in ['A'..'F'] then
      keyCode := VK_A + Ord(unicodeString[j]) - Ord('A');
    Press(keyCode);
  end;
  Unapply([ssCtrl, ssShift]);
    {$ENDIF}

  {$IFDEF Windows}
  // Windows implementation using Alt + Numpad
  Apply([ssAlt]);
  for j := 1 to Length(unicodeString) do begin
    keyCode := Ord(unicodeString[j]);
    Press(keyCode); // Simulate key press for each digit
  end;
  Unapply([ssAlt]);
    {$ENDIF}

  {$IFDEF Darwin}
 // macOS
  // I need a Mac to test this on.
  Apply([ssAlt]);
  for j := 1 to Length(unicodeString) do begin
    if unicodeString[j] in ['0'..'9'] then
      keyCode := VK_0 + Ord(unicodeString[j]) - Ord('0')
    else if unicodeString[j] in ['A'..'F'] then
      keyCode := VK_A + Ord(unicodeString[j]) - Ord('A');
    Press(keyCode);
  end;
  Unapply([ssAlt]);
    {$ENDIF}
end;

procedure TKeyInput.PressString(StringValue: string);
var
  p: PChar;
  unicode: cardinal;
  CPLen: integer;
  keySym: word;
begin
  p := PChar(StringValue);
  while p^ <> #0 do
  begin
    unicode := UTF8CodepointToUnicode(p, CPLen);

    if (CPLen = 1) and (unicode < 128) then
    begin
      keySym := CharToKeySym(char(unicode));
      if NeedsShift(char(unicode)) then Apply([ssShift]);
      Press(keySym);
      if NeedsShift(char(unicode)) then Unapply([ssShift]);
    end
    else if unicode > 0 then
    begin
      PressUnicodeChar(unicode);
    end;

    Inc(p, CPLen);
  end;
end;

procedure TKeyInput.PressStringUC(StringValue: string);
var
  p: PChar;
  unicode: cardinal;
  CPLen: integer;
begin
  p := PChar(StringValue);
  repeat
    unicode := UTF8CodepointToUnicode(p, CPLen);
    if unicode > 0 then PressUnicodeChar(unicode);
    Inc(p, CPLen);
  until (CPLen = 0) or (unicode = 0);
end;

procedure TKeyInput.PressStringUCchar(CharValue: string);
var
  unicode: cardinal;
  CPLen: integer;
begin
  unicode := UTF8CodepointToUnicode(PChar(CharValue), CPLen);
  if unicode > 0 then PressUnicodeChar(unicode);
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
