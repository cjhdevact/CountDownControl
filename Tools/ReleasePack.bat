@echo off
echo 任意键进行 CountDownControl 发布打包...
pause > nul
call "%~dp0PackOut.bat"
call "%~dp0SignBin.bat"
call "%~dp0Pack7z.bat"
call "%~dp0GetHash.bat"
echo.
echo 完成！
echo 任意键退出...
pause > nul