{ MouseAndKeyInput

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
unit MouseAndKeyInput;

interface

uses
  MouseInputIntf,
  KeyInputIntf,
  {$IFDEF WINDOWS}
  WinMouseInput,
  WinKeyInput,
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF LCLcarbon}
    CarbonMouseInput,
    CarbonKeyInput,
    {$ELSE}
    WaylandMouseAndKeyInput,
    XMouseInput,
    XKeyInput,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils;

var
  MouseInput: TMouseInput;
  KeyInput: TKeyInput;

implementation

function IsWayland: Boolean;
begin
  Result := GetEnvironmentVariable('WAYLAND_DISPLAY') <> '';
end;

initialization

  if IsWayland then
  begin
    MouseInput := InitializeWaylandMouseInput;
    KeyInput := InitializeWaylandKeyInput;
  end
  else
  begin
    MouseInput := InitializeMouseInput; // Default to X11
    KeyInput := InitializeKeyInput;
  end;

finalization

  FreeAndNil(MouseInput);
  FreeAndNil(KeyInput);

end.

