@echo off

:: Add the following registry key to enable ANSI escape codes:
:: [HKEY_CURRENT_USER\Console]
:: "VirtualTerminalLevel"=dword:00000001
echo Adding registry key
reg add HKCU\Console /v VirtualTerminalLevel /t REG_DWORD /d 1 /f > nul

:: Permanently set the environment variable MAVL_COLOR to signal to the compiler driver
echo Setting environment variable
setx MAVL_COLOR ON > nul

echo.
echo [92mColor output has been enabled.[0m
echo [91mYou might need to restart your terminal session for the changes to take effect.[0m

pause > nul