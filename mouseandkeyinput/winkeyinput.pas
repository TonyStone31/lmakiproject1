{ WinKeyInput

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
unit WinKeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  Windows, JwaWinUser,
  KeyInputIntf;
  
type

  { TWinKeyInput }

  TWinKeyInput = class(TKeyInput)
  private
    capsLockBeginState: Boolean;
    function GetKeyStateVK(Key: Byte): Boolean;
  protected
    procedure DoDown(Key: Word); override;
    procedure DoUp(Key: Word); override;
    procedure capsLockGetSaveState; override;
    procedure capsLockRestoreState; override;
  end;
  
function InitializeKeyInput: TKeyInput;

implementation

function InitializeKeyInput: TKeyInput;
begin
  Result := TWinKeyInput.Create;
end;

procedure SendKeyInput(Flag: DWORD; Key: Word);
var
  Input: TInput;
begin
  FillChar(Input, SizeOf(Input), 0);
  Input.type_ := INPUT_KEYBOARD;
  Input.ki.dwFlags := Flag;
  Input.ki.wVk := Key;

  SendInput(1, @Input, SizeOf(Input));
end;


{ TWinKeyInput }

procedure TWinKeyInput.DoDown(Key: Word);
begin
  SendKeyInput(0, Key);
end;

procedure TWinKeyInput.DoUp(Key: Word);
begin
  SendKeyInput(KEYEVENTF_KEYUP, Key);
end;

procedure TWinKeyInput.capsLockGetSaveState;
begin
  capsLockBeginState := GetKeyStateVK(VK_CAPITAL);

  if capsLockBeginState then
  begin
    SendKeyInput(0, VK_CAPITAL);
    SendKeyInput(KEYEVENTF_KEYUP, VK_CAPITAL);
  end;
end;

procedure TWinKeyInput.capsLockRestoreState;
begin
  if capsLockBeginState and not GetKeyStateVK(VK_CAPITAL) then
  begin
    SendKeyInput(0, VK_CAPITAL);
    SendKeyInput(KEYEVENTF_KEYUP, VK_CAPITAL);
  end;
end;

end.

