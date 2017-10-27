unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, jwawinuser, windows, jwawinreg;

type

  { TSelectionForm }

  TSelectionForm = class(TForm)
    ChangeItem: TMenuItem;
    ChangeMonitorItem: TMenuItem;
    LaunchOnStartup: TMenuItem;
    QuitItem: TMenuItem;
    SelectionShape: TShape;
    TrayPopup: TPopupMenu;
    TrayIcon: TTrayIcon;

    procedure ChangeItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LaunchOnStartupClick(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SelectionForm: TSelectionForm;

  keyNeeded: DWORD;

  kbdHook: HHOOK;
  mouseHook: HHOOK;

  takingScreenShot: boolean;
  selectingScreenShotArea: boolean;
  changingShortcutCombination: boolean;

  currTopLeftX, currTopLeftY: longInt;

  currMonitor: ^TMonitor;

implementation

{$R *.lfm}

{ TSelectionForm }

function isBitSet(const AValueToCheck, ABitIndex: LongInt): Boolean;
begin
  Result := AValueToCheck and (1 shl ABitIndex) <> 0;
end;

function kbdHookProc(nCode: longInt; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KBInfoStruct: ^KBDLLHOOKSTRUCT;
  vKeyCode: LONG;

  isCtrlDown: boolean;
  isAltDown: boolean;

  drawReg: HRGN;
begin
  if (nCode < 0) or ((wParam <> WM_KEYDOWN) and (wParam <> WM_SYSKEYDOWN)) then
  begin
    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  KBInfoStruct := PKBDLLHOOKSTRUCT(lParam);

  vKeyCode := KBInfoStruct^.vkCode;

  if (vKeyCode = VK_ESCAPE) then
  begin
    SelectionForm.SelectionShape.Visible:=false;
    SelectionForm.Refresh;
    SelectionForm.Repaint;
    SelectionForm.Visible:=false;

    selectingScreenShotArea := false;
    takingScreenShot := false;

    SelectionForm.SelectionShape.Width:=0;
    SelectionForm.SelectionShape.Height:=0;

    drawReg := CreateRectRgn(0,0,0,0);

    SetWindowRgn(SelectionForm.Handle, drawReg, true);
  end;

  if ((changingShortcutCombination) and (vKeyCode = VK_RETURN)) then
  begin
    changingShortcutCombination := false;
    SelectionForm.TrayIcon.BalloonTitle:='Changing shortcut combination';
    SelectionForm.TrayIcon.BalloonHint:='Shortcut was succesfuly changed';
    SelectionForm.TrayIcon.ShowBalloonHint;
    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  if (changingShortcutCombination) then
  begin
    keyNeeded := vKeyCode;
    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  isCtrlDown := (isBitSet(GetKeyState(VK_CONTROL), 15));
  isAltDown := (isBitSet(GetKeyState(VK_MENU), 15));

  if ((isCtrlDown) and (isAltDown) and (vKeyCode = keyNeeded)) and (not(selectingScreenShotArea)) then
  begin
    if ((SelectionForm.Left <> Screen.DesktopLeft) or (SelectionForm.Top <> Screen.DesktopTop)) then
    begin
      SelectionForm.Left := Screen.DesktopLeft;
      SelectionForm.Top := Screen.DesktopTop;
    end;

    SelectionForm.Width:=Screen.DesktopWidth;
    SelectionForm.Height:=Screen.DesktopHeight;

    drawReg := CreateRectRgn(0,0,SelectionForm.Width,SelectionForm.Height);
    SetWindowRgn(SelectionForm.Handle, drawReg, true);

    SelectionForm.Visible:=true;
    // this second check is weird, i know. but i have no idea to why after setting visible to false, my Left field is being cut in half..
    if ((SelectionForm.Left <> Screen.DesktopLeft) or (SelectionForm.Top <> Screen.DesktopTop)) then
    begin
      SelectionForm.Left := Screen.DesktopLeft;
      SelectionForm.Top := Screen.DesktopTop;
    end;

    takingScreenShot := true;
  end;

  result := CallNextHookEx(0, nCode, wParam, lParam);
end;

function mouseHookProc(nCode: longInt; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  screenContext: HDC;
  compatibleContext: HDC;

  screenWidth: longInt;
  screenHeight: longInt;

  screenBitmap: HBITMAP;
  tempScreenBitmap: HBITMAP;

  drawReg: HRGN;

  mouseScreen, mouseClient: TPoint;

  copyBitMapPointTopLeft, tempShapePoint: TPoint;
begin
  if ((nCode < 0) or ((wParam <> WM_LBUTTONDOWN) and (wParam <> WM_MOUSEMOVE) and (wParam <> WM_LBUTTONUP)) or not(takingScreenShot)) then
  begin
    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  mouseScreen.x := Mouse.CursorPos.x;
  mouseScreen.y := Mouse.CursorPos.y;

  mouseClient := SelectionForm.ScreenToClient(mouseScreen);




  if (wParam = WM_LBUTTONDOWN) then
  begin
    SelectionForm.SelectionShape.Visible:=true;

    currTopLeftX := mouseClient.x;
    currTopLeftY := mouseClient.y;

    SelectionForm.SelectionShape.Left:=mouseClient.x;
    SelectionForm.SelectionShape.Top:=mouseClient.y;

    selectingScreenShotArea := true;
  end
  else if ((wParam = WM_LBUTTONUP) and (selectingScreenShotArea)) then
  begin
    SelectionForm.SelectionShape.Visible:=false;
    SelectionForm.Refresh;
    SelectionForm.Repaint;
    SelectionForm.Visible:=false;

    screenContext := GetDC(0);
    compatibleContext := CreateCompatibleDC(screenContext);

    screenWidth := SelectionForm.SelectionShape.Width;
    screenHeight := SelectionForm.SelectionShape.Height;

    screenBitmap := CreateCompatibleBitmap(screenContext, screenWidth, screenHeight);
    tempScreenBitmap := SelectObject(compatibleContext, screenBitmap);

    tempShapePoint.x:=SelectionForm.SelectionShape.Left;
    tempShapePoint.y:=SelectionForm.SelectionShape.Top;

    copyBitMapPointTopLeft := SelectionForm.ClientToScreen(tempShapePoint);
    BitBlt(compatibleContext, 0, 0, screenWidth, screenHeight, screenContext, copyBitMapPointTopLeft.x, copyBitMapPointTopLeft.y, SRCCOPY);

    screenBitmap := SelectObject(compatibleContext, tempScreenBitmap);

    OpenClipboard(0);
    EmptyClipboard();
    SetClipboardData(CF_BITMAP, screenBitmap);
    CloseClipboard();

    DeleteDC(compatibleContext);
    ReleaseDC(0, screenContext);
    DeleteObject(screenBitmap);

    selectingScreenShotArea := false;
    takingScreenShot := false;

    SelectionForm.SelectionShape.Width:=0;
    SelectionForm.SelectionShape.Height:=0;

    drawReg := CreateRectRgn(0,0,0,0);

    SetWindowRgn(SelectionForm.Handle, drawReg, true);
  end
  else if ((wParam = WM_MOUSEMOVE) and (selectingScreenShotArea)) then
  begin
    if (currTopLeftX < mouseClient.x) then
    begin
      SelectionForm.SelectionShape.Left:=currTopLeftX;
    end
    else
    begin
      SelectionForm.SelectionShape.Left:=mouseClient.x;
    end;

    if (currTopLeftY < mouseClient.y) then
    begin
      SelectionForm.SelectionShape.Top:=currTopLeftY;
    end
    else
    begin
      SelectionForm.SelectionShape.Top:=mouseClient.y;
    end;

    SelectionForm.SelectionShape.Width:=Abs(currTopLeftX - mouseClient.x);
    SelectionForm.SelectionShape.Height:=Abs(currTopLeftY - mouseClient.y);

    drawReg := CreateRectRgn(SelectionForm.SelectionShape.Left,
                             SelectionForm.SelectionShape.Top,
                             SelectionForm.SelectionShape.Left + SelectionForm.SelectionShape.Width,
                             SelectionForm.SelectionShape.Top + SelectionForm.SelectionShape.Height
                             );

    SetWindowRgn(SelectionForm.Handle, drawReg, true);
  end;

  result := CallNextHookEx(0, nCode, wParam, lParam);
end;

procedure TSelectionForm.FormCreate(Sender: TObject);
var
  drawReg: HRGN;
  exePath: string;

  messageBoxReturn: longInt;

  hStartupKey: HKEY;
  hOldStartupKey: HKEY;
  hCheckStartupKey: HKEY;

  hFirstTimeRunKey: HKEY;
  hCreatedFirstTimeRunKey: HKEY;
begin
  drawReg := CreateRectRgn(0,0,0,0);

  SetWindowRgn(Handle, drawReg, true);

  Canvas.Brush.Color:=clWhite;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Style:=psClear;

  keyNeeded := VK_C;

  kbdHook := SetWindowsHookEx(WH_KEYBOARD_LL, @kbdHookProc, HInstance, 0);
  mouseHook := SetWindowsHookEx(WH_MOUSE_LL, @mouseHookProc, HInstance, 0);

  takingScreenShot := false;
  selectingScreenShotArea := false;
  changingShortcutCombination := false;

  if (RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run', 0, KEY_READ, hCheckStartupKey) = ERROR_SUCCESS) then
  begin
    // startup enabled
    if (RegQueryValueEx(hCheckStartupKey, 'ScreenCropper', nil, nil, nil, nil) = ERROR_FILE_NOT_FOUND) then
    begin
      // didn't find the startup value, it's not enabled then!!
      LaunchOnStartup.Checked:=false;
    end
    else
    begin
      LaunchOnStartup.Checked:=true;
    end;
  end
  else
  begin
    LaunchOnStartup.Checked:=false;
  end;

  if (RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\ScreenCropper', 0, KEY_READ, hCreatedFirstTimeRunKey) <> ERROR_SUCCESS) then
  begin
    // running for the first time!

    // let's see if we already have a screated registry file for stratup
    if (RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run', 0, KEY_READ, hOldStartupKey) = ERROR_SUCCESS) then
    begin
      if (RegQueryValueEx(hOldStartupKey, 'ScreenCropper', nil, nil, nil, nil) = ERROR_FILE_NOT_FOUND) then
      begin
        // can't open startup registry for our exe, should we create it?
        messageBoxReturn := MessageBox(0, 'Do you want Screen Cropper to be run on Windows startup?', 'Screen Cropper', MB_YESNO or MB_ICONINFORMATION);

        exePath := Application.ExeName;
        if (messageBoxReturn = IDYES) then
        begin
          // add key to autorun reg
          if (RegCreateKey(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run', hStartupKey) <> ERROR_SUCCESS) then
          begin
            // could not create reg key..
            MessageBox(0, 'Could not create registry key for Screen Cropper!', 'Screen Cropper', MB_OK or MB_ICONINFORMATION)
          end
          else
          begin
            RegSetValueEx(hStartupKey, 'ScreenCropper', 0, REG_SZ, LPBYTE(exePath), (Length(exePath)+1)*2);
          end;
        end;

      end;
    end;

    RegCreateKey(HKEY_CURRENT_USER, 'Software\ScreenCropper', hFirstTimeRunKey);
  end;
end;

procedure TSelectionForm.ChangeItemClick(Sender: TObject);
begin
  changingShortcutCombination := true;

  TrayIcon.BalloonTitle:='Changing shortcut combination';
  TrayIcon.BalloonHint:='Perform any combination of keys and press Enter';
  TrayIcon.ShowBalloonHint;
end;

procedure TSelectionForm.FormDestroy(Sender: TObject);
begin
  UnhookWindowsHookEx(kbdHook);
  UnhookWindowsHookEx(mouseHook);
end;

procedure TSelectionForm.LaunchOnStartupClick(Sender: TObject);
var
  startupKey: HKEY;
  newStartupKey: HKEY;

  exePath: string;
begin
  exePath := Application.ExeName;

  RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run', 0, KEY_READ or KEY_WRITE, startupKey);
  if (LaunchOnStartup.Checked) then
  begin
    // remove ScreenCropper value from Software\Microsoft\Windows\CurrentVersion\Run
    RegDeleteValue(startupKey, 'ScreenCropper');
  end
  else
  begin
    // else add it
    if (RegCreateKey(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run', newStartupKey) <> ERROR_SUCCESS) then
    begin
      // could not create reg key..
      MessageBox(0, 'Could not create registry key for Screen Cropper!', 'Screen Cropper', MB_OK or MB_ICONINFORMATION)
    end
    else
    begin
      RegSetValueEx(newStartupKey, 'ScreenCropper', 0, REG_SZ, LPBYTE(exePath), (Length(exePath)+1)*2);
    end;
  end;

  LaunchOnStartup.Checked:=not(LaunchOnStartup.Checked);
end;

procedure TSelectionForm.QuitItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.

