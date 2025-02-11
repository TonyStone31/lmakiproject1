{ CarbonKeyInput

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
unit CarbonKeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  FPCMacOSAll, CarbonProc,
  KeyInputIntf;

type

  { TCarbonKeyInput }

  TCarbonKeyInput = class(TKeyInput)
  private
    capsLockBeginState: Boolean;
    function IsCapsLockOn: Boolean;
  protected
    procedure DoDown(Key: Word); override;
    procedure DoUp(Key: Word); override;
    procedure capsLockGetSaveState; override;
    procedure capsLockRestoreState; override;
  end;

function InitializeKeyInput: TKeyInput;

implementation

uses
  LCLType;

function InitializeKeyInput: TKeyInput;
begin
  Result := TCarbonKeyInput.Create;
end;

procedure SendKeyInput(Key: Word; Down: Boolean);
var
  Char: Word;
begin
  Char := 0;
  if Key in [VK_A .. VK_Z] then
  begin
    Char := Ord('A') + Key - VK_A;
    Key := 0;
  end;
  if Key in [VK_0 .. VK_9] then
  begin
    Key := VK_NUMPAD0 + Key - VK_0;
  end;
  CGPostKeyboardEvent(Char, VirtualKeyCodeToMac(Key), Integer(Down));
end;

{ TCarbonKeyInput }

function TCarbonKeyInput.IsCapsLockOn: Boolean;
var
  ModifierState: CGEventFlags;
begin
  ModifierState := CGEventSourceFlagsState(kCGEventSourceStateHIDSystemState);
  Result := (ModifierState and kCGEventFlagMaskAlphaShift) <> 0;
end;

procedure TCarbonKeyInput.DoDown(Key: Word);
begin
  SendKeyInput(Key, True);
end;

procedure TCarbonKeyInput.DoUp(Key: Word);
begin
  SendKeyInput(Key, False);
end;

procedure TCarbonKeyInput.capsLockGetSaveState;
begin
  capsLockBeginState := IsCapsLockOn;

  if capsLockBeginState then
  begin
    SendKeyInput(VK_CAPITAL, True);
    SendKeyInput(VK_CAPITAL, False);
  end;
end;

procedure TCarbonKeyInput.capsLockRestoreState;
begin
  if capsLockBeginState and not IsCapsLockOn then
  begin
    SendKeyInput(VK_CAPITAL, True);
    SendKeyInput(VK_CAPITAL, False);
  end;
end;

end.
