'****************************************************************************
'    CountDownControl
'    Copyright (C) 2024 CJH.
'
'    This program is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    This program is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with this program.  If not, see <http://www.gnu.org/licenses/>.
'****************************************************************************
'/*****************************************************\
'*                                                     *
'*     CountDownControl - Form2.vb                     *
'*                                                     *
'*     Copyright (c) CJH.                              *
'*                                                     *
'*     The Settings form.                              *
'*                                                     *
'\*****************************************************/
Imports System.Runtime.InteropServices
Imports System.Drawing
Imports Microsoft.Win32

Public Class Form2
    <DllImport("dwmapi.dll")> _
    Public Shared Function DwmSetWindowAttribute(ByVal hwnd As IntPtr, ByVal attr As DwmWindowAttribute, ByRef attrValue As Integer, ByVal attrSize As Integer) As Integer
    End Function
    'Public Shared Function EnableDarkModeForWindow(ByVal hWnd As IntPtr, ByVal enable As Boolean) As Boolean
    '    Dim darkMode As Integer
    '    darkMode = enable
    '    Dim hr As Integer
    '    Dim i As Integer
    '    'hr = DwmSetWindowAttribute(hWnd, DwmWindowAttribute.UseImmersiveDarkMode, darkMode, sizeof(i))
    '    Return hr >= 0
    'End Function
    Public Shared Function EnableDarkModeForWindow(ByVal hWnd As IntPtr, ByVal enable As Boolean) As Boolean
        Dim attrValue As Integer = If(enable, 1, 0)
        Return (Form1.DwmSetWindowAttribute(hWnd, DwmWindowAttribute.UseImmersiveDarkMode, attrValue, 4) >= 0)
    End Function

    Public Enum DwmWindowAttribute As UInt32
        NCRenderingEnabled = 1
        NCRenderingPolicy
        TransitionsForceDisabled
        AllowNCPaint
        CaptionButtonBounds
        NonClientRtlLayout
        ForceIconicRepresentation
        Flip3DPolicy
        ExtendedFrameBounds
        HasIconicBitmap
        DisallowPeek
        ExcludedFromPeek
        Cloak
        Cloaked
        FreezeRepresentation
        PassiveUpdateMode
        UseHostBackdropBrush
        UseImmersiveDarkMode = 20
        WindowCornerPreference = 33
        BorderColor
        CaptionColor
        TextColor
        VisibleFrameBorderThickness
        SystemBackdropType
        Last
    End Enum

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        On Error GoTo errcode
        If TextBox1.Text = "" Then
            MessageBox.Show("隐藏时间不能为空！", "错误", MessageBoxButtons.OK, MessageBoxIcon.Error)
        ElseIf TextBox1.Text = "0" Then
            MessageBox.Show("隐藏时间不能为0！", "错误", MessageBoxButtons.OK, MessageBoxIcon.Error)
        Else
            If ComboBox1.SelectedIndex = 0 Then '秒
                Form1.Timer2.Interval = TextBox1.Text * 1000
                Form1.Timer2.Enabled = True
                Form1.NotifyIcon1.Visible = True
                Form1.NotifyIcon1.ShowBalloonTip(7000, "倒计时小工具", "倒计时小工具当前已隐藏到系统托盘，双击托盘图标或在设定的时间（" & TextBox1.Text & "秒）之后重新显示。", ToolTipIcon.Info)
                Form1.Hide()
                Me.Close()
            ElseIf ComboBox1.SelectedIndex = 1 Then '分
                Form1.Timer2.Interval = TextBox1.Text * 1000 * 60
                Form1.Timer2.Enabled = True
                Form1.NotifyIcon1.Visible = True
                Form1.NotifyIcon1.ShowBalloonTip(7000, "倒计时小工具", "倒计时小工具当前已隐藏到系统托盘，双击托盘图标或在设定的时间（" & TextBox1.Text & "分钟）之后重新显示。", ToolTipIcon.Info)
                Form1.Hide()
                Me.Close()
            ElseIf ComboBox1.SelectedIndex = 2 Then '时
                Form1.Timer2.Interval = TextBox1.Text * 1000 * 60 * 60
                Form1.Timer2.Enabled = True
                Form1.NotifyIcon1.Visible = True
                Form1.NotifyIcon1.ShowBalloonTip(7000, "倒计时小工具", "倒计时小工具当前已隐藏到系统托盘，双击托盘图标或在设定的时间（" & TextBox1.Text & "小时）之后重新显示。", ToolTipIcon.Info)
                Form1.Hide()
                Me.Close()
            End If
        End If
        Exit Sub
errcode:
        MsgBox(Err.Description, MsgBoxStyle.Critical, "错误")
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Me.Close()
    End Sub

    Private Sub Form2_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'DateTimePicker1.Value = Now
        Dim disi As Graphics = Me.CreateGraphics()
        Label20.Width = Me.Width - disi.DpiX * 0.01 * 50
        'TextBox2.Height = TextBox2.Height * disi.DpiY * 0.01
        TextBox2.Height = Me.Height * 0.245
        OpenFileDialog1.Filter = "所有支持的文件 (*.png;*.jpg;*.jpeg;*.jpe;*.jfif;*.bmp;*.dib;*.gif;*.tif;*.tiff;*.ico)|*.png;*.jpg;*.jpeg;*.jpe;*.jfif;*.bmp;*.dib;*.gif;*.tif;*.tiff;*.ico|" _
                              & "PNG 图像 (*.png)|*.png|JPEG 文件 (*.jpg;*.jpeg;*.jpe;*.jfif)|*.jpg;*.jpeg;*.jpe;*.jfif|" _
                              & "BMP 文件 (*.bmp;*.dib)|*.bmp;*.dib|GIF 图像 (*.gif)|*.gif|TIFF 文件 (*.tif;*.tiff)|*.tif;*.tiff|图标文件(*.ico)|*.ico|所有文件 (*.*)|*.*"
        If Form1.TopMost = True Then
            Me.CheckBox1.Checked = True
        Else
            Me.CheckBox1.Checked = False
        End If
        If Form1.UseMoveV = 1 Then
            Me.CheckBox2.Checked = True
        Else
            Me.CheckBox2.Checked = False
        End If

        If Form1.SaveLoc = 1 Then
            Me.CheckBox3.Checked = True
        Else
            Me.CheckBox3.Checked = False
        End If

        If Form1.MySize = 1 Then
            Me.CheckBox4.Checked = True
            If Form1.DisbFuState = 0 Then
                TextBox3.Enabled = True
                TextBox4.Enabled = True
                Button9.Enabled = True
            End If

        Else
            Me.CheckBox4.Checked = False
            If Form1.DisbFuState = 0 Then
                TextBox3.Enabled = False
                TextBox4.Enabled = False
                Button9.Enabled = False
            End If

        End If

        '如果预先关联事件， Me.CheckBox1.Checked = Ture / Flase 操作会触发事件，导致操作相反
        'AddHandler CheckBox1.CheckedChanged, AddressOf CheckBox1_CheckedChanged
        'AddHandler CheckBox2.CheckedChanged, AddressOf CheckBox2_CheckedChanged
        'AddHandler CheckBox3.CheckedChanged, AddressOf CheckBox3_CheckedChanged
        'AddHandler CheckBox4.CheckedChanged, AddressOf CheckBox4_CheckedChanged

        ComboBox1.SelectedIndex = 0
        ComboBox1.SelectedText = "秒"

        If Form1.UnSupportDarkSys = 1 Then
            If Form1.appcolor > 0 Then
                ComboBox2.SelectedIndex = Form1.appcolor - 1
            End If
            If Form1.appcolor = 1 Then
                ComboBox2.SelectedText = "浅色"
            ElseIf Form1.appcolor = 2 Then
                ComboBox2.SelectedText = "深色"
            End If
        Else
            ComboBox2.SelectedIndex = Form1.appcolor
            If Form1.appcolor = 0 Then
                ComboBox2.SelectedText = "跟随系统"
            ElseIf Form1.appcolor = 1 Then
                ComboBox2.SelectedText = "浅色"
            ElseIf Form1.appcolor = 2 Then
                ComboBox2.SelectedText = "深色"
            End If
        End If

        If ComboBox3.SelectedIndex = 0 Then
            ComboBox3.SelectedText = "%H:%m"
        ElseIf ComboBox3.SelectedIndex = 1 Then
            ComboBox3.SelectedText = "%H:%m:%s"
        ElseIf ComboBox3.SelectedIndex = 2 Then
            ComboBox3.SelectedText = "%d:%H:%m:%s"
        ElseIf ComboBox3.SelectedIndex = 3 Then
            ComboBox3.SelectedText = "%d日 %H时%m分%s秒"
        ElseIf ComboBox3.SelectedIndex = 4 Then
            ComboBox3.SelectedText = "%d %H:%m:%s"
        ElseIf ComboBox3.SelectedIndex = 5 Then
            ComboBox3.SelectedText = "（自定义）"
        End If


        ComboBox4.SelectedIndex = Form1.TimeTheme
        If Form1.TimeTheme = 0 Then
            ComboBox4.SelectedText = "圆角"
        ElseIf Form1.TimeTheme = 1 Then
            ComboBox4.SelectedText = "经典"
        ElseIf Form1.TimeTheme = 2 Then
            ComboBox4.SelectedText = "自定义背景"
        End If

        Label1.Text = "倒计时小工具 版本：" & My.Application.Info.Version.ToString & vbCrLf & "版权所有 © 2024 CJH。"
        Call formatcolorcurset()
    End Sub
    Private Sub TextBox1_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox1.KeyPress
        Dim strlimit As String
        strlimit = "0123456789"
        Dim keychar As Char = e.KeyChar
        If InStr(strlimit, keychar) <> 0 Or e.KeyChar = Microsoft.VisualBasic.ChrW(8) Then
            'If keychar = "." And InStr(TextBox1.Text, keychar) <> 0 Then
            'e.Handled = True
            'Else
            e.Handled = False
            'End If
        Else
            e.Handled = True
        End If
    End Sub
    Sub formatcolorcurset()
        If Form1.crmd = 0 Then
            EnableDarkModeForWindow(Me.Handle, True)
            Me.BackColor = Color.FromArgb(32, 32, 32)
            Me.ForeColor = Color.White
            Me.Button1.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button1.ForeColor = Color.White
            Me.Button2.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button2.ForeColor = Color.White
            Me.Button4.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button4.ForeColor = Color.White
            Me.Button5.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button5.ForeColor = Color.White
            Me.Button6.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button6.ForeColor = Color.White
            Me.Button7.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button7.ForeColor = Color.White
            Me.Button8.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button8.ForeColor = Color.White
            Me.Button9.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button9.ForeColor = Color.White
            Me.Button10.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button10.ForeColor = Color.White
            Me.Button11.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button11.ForeColor = Color.White
            Me.Button12.BackColor = Color.FromArgb(32, 32, 32)
            Me.Button12.ForeColor = Color.White
            Me.TextBox1.BackColor = Color.FromArgb(32, 32, 32)
            Me.TextBox1.ForeColor = Color.White
            Me.TextBox2.BackColor = Color.FromArgb(32, 32, 32)
            Me.TextBox2.ForeColor = Color.White
            Me.TextBox3.BackColor = Color.FromArgb(32, 32, 32)
            Me.TextBox3.ForeColor = Color.White
            Me.TextBox4.BackColor = Color.FromArgb(32, 32, 32)
            Me.TextBox4.ForeColor = Color.White
            Me.TextBox5.BackColor = Color.FromArgb(32, 32, 32)
            Me.TextBox5.ForeColor = Color.White
            Me.ComboBox1.BackColor = Color.FromArgb(32, 32, 32)
            Me.ComboBox1.ForeColor = Color.White
            Me.ComboBox2.BackColor = Color.FromArgb(32, 32, 32)
            Me.ComboBox2.ForeColor = Color.White
            Me.ComboBox3.BackColor = Color.FromArgb(32, 32, 32)
            Me.ComboBox3.ForeColor = Color.White
            Me.ComboBox4.BackColor = Color.FromArgb(32, 32, 32)
            Me.ComboBox4.ForeColor = Color.White
            Me.CheckBox1.BackColor = Color.FromArgb(32, 32, 32)
            Me.CheckBox1.ForeColor = Color.White
            Me.CheckBox2.BackColor = Color.FromArgb(32, 32, 32)
            Me.CheckBox2.ForeColor = Color.White
            Me.CheckBox3.BackColor = Color.FromArgb(32, 32, 32)
            Me.CheckBox3.ForeColor = Color.White
            Me.CheckBox4.BackColor = Color.FromArgb(32, 32, 32)
            Me.CheckBox4.ForeColor = Color.White
        Else
            EnableDarkModeForWindow(Me.Handle, False)
            Me.BackColor = Color.White
            Me.ForeColor = Color.Black
            Me.Button1.BackColor = Color.Transparent
            Me.Button1.ForeColor = Color.Black
            Me.Button2.BackColor = Color.Transparent
            Me.Button2.ForeColor = Color.Black
            Me.Button4.BackColor = Color.Transparent
            Me.Button4.ForeColor = Color.Black
            Me.Button5.BackColor = Color.Transparent
            Me.Button5.ForeColor = Color.Black
            Me.Button6.BackColor = Color.Transparent
            Me.Button6.ForeColor = Color.Black
            Me.Button7.BackColor = Color.Transparent
            Me.Button7.ForeColor = Color.Black
            Me.Button8.BackColor = Color.Transparent
            Me.Button8.ForeColor = Color.Black
            Me.Button9.BackColor = Color.Transparent
            Me.Button9.ForeColor = Color.Black
            Me.Button10.BackColor = Color.Transparent
            Me.Button10.ForeColor = Color.Black
            Me.Button11.BackColor = Color.Transparent
            Me.Button11.ForeColor = Color.Black
            Me.Button12.BackColor = Color.Transparent
            Me.Button12.ForeColor = Color.Black
            Me.TextBox1.BackColor = Color.White
            Me.TextBox1.ForeColor = Color.Black
            Me.TextBox2.BackColor = Color.White
            Me.TextBox2.ForeColor = Color.Black
            Me.TextBox3.BackColor = Color.White
            Me.TextBox3.ForeColor = Color.Black
            Me.TextBox4.BackColor = Color.White
            Me.TextBox4.ForeColor = Color.Black
            Me.TextBox5.BackColor = Color.White
            Me.TextBox5.ForeColor = Color.Black
            Me.ComboBox1.BackColor = Color.White
            Me.ComboBox1.ForeColor = Color.Black
            Me.ComboBox2.BackColor = Color.White
            Me.ComboBox2.ForeColor = Color.Black
            Me.ComboBox3.BackColor = Color.White
            Me.ComboBox3.ForeColor = Color.Black
            Me.ComboBox4.BackColor = Color.White
            Me.ComboBox4.ForeColor = Color.Black
            Me.CheckBox1.BackColor = Color.White
            Me.CheckBox1.ForeColor = Color.Black
            Me.CheckBox2.BackColor = Color.White
            Me.CheckBox2.ForeColor = Color.Black
            Me.CheckBox3.BackColor = Color.White
            Me.CheckBox3.ForeColor = Color.Black
            Me.CheckBox4.BackColor = Color.White
            Me.CheckBox4.ForeColor = Color.Black
        End If
    End Sub

    Private Sub CheckBox1_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles CheckBox1.CheckedChanged
        If CheckBox1.Checked = False Then
            'CheckBox1.Checked = False
            Form1.TopMost = False
            If Form1.UnSaveData = 0 Then
                AddReg("Software\CJH\CountDownControl\Settings", "AllowTopMost", 0, RegistryValueKind.DWord, "HKCU")
            End If

        Else
            'CheckBox1.Checked = True
            Form1.TopMost = True
            If Form1.UnSaveData = 0 Then
                AddReg("Software\CJH\CountDownControl\Settings", "AllowTopMost", 1, RegistryValueKind.DWord, "HKCU")
            End If

        End If
    End Sub

    Private Sub CheckBox2_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles CheckBox2.CheckedChanged
        If CheckBox2.Checked = False Then
            'CheckBox2.Checked = False
            Form1.UseMoveV = 0
            If Form1.UnSaveData = 0 Then
                AddReg("Software\CJH\CountDownControl\Settings", "EnableDrag", 0, RegistryValueKind.DWord, "HKCU")
            End If

        Else
            'CheckBox2.Checked = True
            Form1.UseMoveV = 1
            If Form1.UnSaveData = 0 Then
                AddReg("Software\CJH\CountDownControl\Settings", "EnableDrag", 1, RegistryValueKind.DWord, "HKCU")
            End If

        End If
    End Sub

    Private Sub ComboBox2_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles ComboBox2.SelectedIndexChanged
        If ComboBox2.SelectedIndex = 0 Then
            If Form1.UnSupportDarkSys = 1 Then
                If Form1.UnSaveData = 0 Then
                    AddReg("Software\CJH\CountDownControl\Settings", "ColorMode", 1, Microsoft.Win32.RegistryValueKind.DWord, "HKCU")
                End If

                Form1.crmd = 1 'Light
                Form1.appcolor = 1
                Call Me.formatcolorcurset()
                Call Form1.formatcolorcur()
                Call GPLForm.formatcolorcursetmsg()
            Else
                Form1.appcolor = 0
                If Form1.UnSaveData = 0 Then
                    AddReg("Software\CJH\CountDownControl\Settings", "ColorMode", 0, Microsoft.Win32.RegistryValueKind.DWord, "HKCU")
                End If

                'Get System Color
                Dim regkey As RegistryKey = Registry.CurrentUser.OpenSubKey("SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize", True)
                Dim sysacr As Integer
                If (Not regkey Is Nothing) Then
                    'If (If((regkey.GetValue("") Is Nothing), Nothing, regkey.GetValue("").ToString) <> "AppsUseLightTheme") Then
                    'End If
                    sysacr = regkey.GetValue("AppsUseLightTheme", -1)
                    If sysacr = 0 Then
                        Form1.crmd = 0
                    ElseIf sysacr = 1 Then
                        Form1.crmd = 1
                    Else
                        Form1.crmd = 1
                    End If
                Else
                    Form1.crmd = 1
                End If
                regkey.Close()
                Call Me.formatcolorcurset()
                Call Form1.formatcolorcur()
            End If
        ElseIf ComboBox2.SelectedIndex = 1 Then
            If Form1.UnSupportDarkSys = 1 Then
                If Form1.UnSaveData = 0 Then
                    AddReg("Software\CJH\CountDownControl\Settings", "ColorMode", 2, Microsoft.Win32.RegistryValueKind.DWord, "HKCU")
                End If

                Form1.crmd = 0 'Dark
                Form1.appcolor = 2
                Call Me.formatcolorcurset()
                Call Form1.formatcolorcur()
                Call GPLForm.formatcolorcursetmsg()
            Else
                If Form1.UnSaveData = 0 Then
                    AddReg("Software\CJH\CountDownControl\Settings", "ColorMode", 1, Microsoft.Win32.RegistryValueKind.DWord, "HKCU")
                End If

                Form1.crmd = 1 'Light
                Form1.appcolor = 1
                Call Me.formatcolorcurset()
                Call Form1.formatcolorcur()
                Call GPLForm.formatcolorcursetmsg()
            End If
        ElseIf ComboBox2.SelectedIndex = 2 Then
            If Form1.UnSaveData = 0 Then
                AddReg("Software\CJH\CountDownControl\Settings", "ColorMode", 2, Microsoft.Win32.RegistryValueKind.DWord, "HKCU")
            End If

            Form1.crmd = 0 'Dark
            Form1.appcolor = 2
            Call Me.formatcolorcurset()
            Call Form1.formatcolorcur()
            Call GPLForm.formatcolorcursetmsg()
        End If
    End Sub

    Private Sub Button3_Click(sender As System.Object, e As System.EventArgs) Handles Button3.Click
        If (MessageBox.Show("确定退出倒计时小工具吗？", "倒计时小工具", MessageBoxButtons.YesNo, MessageBoxIcon.Question, MessageBoxDefaultButton.Button2) = DialogResult.Yes) Then
            Form1.Timer1.Enabled = False
            Form1.Timer2.Enabled = False
            'End
            Application.Exit()
        End If
    End Sub

    Public Sub ComboBox3_SelectedIndexChanged(sender As System.Object, e As System.EventArgs)
        Dim disi As Graphics = Me.CreateGraphics()
        Try
            'Dim a As Integer
            If Me.ComboBox3.SelectedIndex = 0 Then
                Form1.TimeF = Me.ComboBox3.Text
                Try
                    Form1.Label1.Text = Format(Now(), Form1.TimeF)
                Catch ex As Exception
                    Form1.TimeF = "距离XX还有" & vbCrLf & "%d天 %H小时 %m分钟 %s秒"
                    Me.ComboBox3.SelectedIndex = 5
                    Me.ComboBox3.SelectedText = "（自定义）"
                    If Form1.UnSaveData = 0 Then
                        AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 5, RegistryValueKind.DWord, "HKCU")
                    End If
                    MsgBox(ex.Message & vbCrLf & "时间格式化失败，已重置为默认格式。", MsgBoxStyle.Critical, "错误")
                End Try
                Form1.Timer1.Interval = 1000
                If Form1.DisbFuState = 0 Then
                    TextBox2.Enabled = False
                    Button4.Enabled = False
                End If
                'Form1.GetTimeFormSize(38, 120)
                'a = Form1.Width - Form1.CaW
                'If a <> 0 Then
                '    Form1.Location = New Point(Form1.Location.X + a / 2, Form1.Location.Y)
                '    If Form1.SaveLoc = 1 Then
                '        RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                '        RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                '    End If
                'End If
                'a = 0
                If Form1.MySize = 0 Then
                    Dim aa As SizeF
                    Dim b As Graphics = Graphics.FromImage(New Bitmap(1, 1))
                    'ab = b.MeasureString(Form1.Label1.Text, Form1.Label1.Font)
                    aa = TextRenderer.MeasureText(Form1.Label1.Text, Form1.Label1.Font)
                    Dim c As Integer
                    If aa.Width <= 42 Then
                        Form1.GetTimeFormSize(38, aa.Width + 8)
                    ElseIf 400 <= aa.Width Then
                        Form1.GetTimeFormSize(aa.Height, aa.Width + 10)
                    Else
                        Form1.GetTimeFormSize(38, aa.Width + 6)
                    End If
                    c = Form1.Width - Form1.CaW
                    If Not (Form1.SaveLoc = 1 And Form1.IsBootV = 1) Then
                        If c <> 0 Then
                            Form1.Location = New Point(Form1.Location.X + c / 2, Form1.Location.Y)
                            If Form1.SaveLoc = 1 Then
                                If Form1.UnSaveData = 0 Then
                                    RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                                    RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                                End If

                            End If
                        End If
                    End If

                    c = 0
                    Form1.SetTimeFormSize(aa.Height, aa.Width)
                    'If disi.DpiY < 100 Then
                    '    If aa.Width <= 42 Then
                    '        Form1.SetTimeFormSize(aa.Height + 3, aa.Width + 12)
                    '    ElseIf 400 <= aa.Width Then
                    '        Form1.SetTimeFormSize(aa.Height + 3, aa.Width + 14)
                    '    Else
                    '        Form1.SetTimeFormSize(aa.Height + 3, aa.Width + 16)
                    '    End If
                    'ElseIf 100 < disi.DpiY < 135 Then
                    '    Form1.SetTimeFormSize(aa.Height - 17 * disi.DpiY * 0.01, aa.Width - 13 - 15 * disi.DpiY * 0.01)
                    'ElseIf 160 > disi.DpiY > 135 Then
                    '    Form1.SetTimeFormSize(aa.Height - 26 * disi.DpiY * 0.01, aa.Width - 60 - 60 * disi.DpiY * 0.01)
                    'ElseIf disi.DpiY > 160 Then
                    '    Form1.SetTimeFormSize(aa.Height - 30 * disi.DpiY * 0.01, aa.Width - 120 - 800 * disi.DpiY * 0.01)
                    'End If

                    'If disi.DpiY < 100 Then
                    '    If aa.Width <= 42 Then
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 + disi.DpiY * 0.01 * 20, aa.Width + 8)
                    '    ElseIf 400 <= aa.Width Then
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 + disi.DpiY * 0.01 * 20, aa.Width + 10)
                    '    Else
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 + disi.DpiY * 0.01 * 20, aa.Width + 6)
                    '    End If
                    'ElseIf 100 < disi.DpiY < 130 Then
                    '    If aa.Width <= 42 Then
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 + disi.DpiY * 0.01 * 20, aa.Width + 8)
                    '    ElseIf 400 <= aa.Width Then
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 + disi.DpiY * 0.01 * 20, aa.Width + 10)
                    '    Else
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 + disi.DpiY * 0.01 * 20, aa.Width + 6)
                    '    End If
                    'ElseIf 130 < disi.DpiY < 150 Then
                    '    If aa.Width <= 42 Then
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 + disi.DpiY * 0.01 * 3, aa.Width + 8)
                    '    ElseIf 400 <= aa.Width Then
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 + disi.DpiY * 0.01 * 3, aa.Width + 10)
                    '    Else
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 + disi.DpiY * 0.01 * 3, aa.Width + 6)
                    '    End If
                    'Else
                    '    If aa.Width <= 42 Then
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 - disi.DpiY * 0.01 * 5, aa.Width + 8)
                    '    ElseIf 400 <= aa.Width Then
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 - disi.DpiY * 0.01 * 5, aa.Width + 10)
                    '    Else
                    '        Form1.SetTimeFormSize(Form1.Label1.Font.Size * disi.DpiY * 0.01 - disi.DpiY * 0.01 * 5, aa.Width + 6)
                    '    End If
                    'End If

                End If

                If Form1.UnSaveData = 0 Then
                    AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 0, RegistryValueKind.DWord, "HKCU")
                End If

                ' Form1.SetTimeFormSize(38, 120)
                Form1.AutoSize = False
                Form1.Label1.AutoSize = False

            ElseIf Me.ComboBox3.SelectedIndex = 1 Then
                Form1.TimeF = Me.ComboBox3.Text
                Try
                    Form1.Label1.Text = Format(Now(), Form1.TimeF)
                Catch ex As Exception
                    Form1.TimeF = "距离XX还有" & vbCrLf & "%d天 %H小时 %m分钟 %s秒"
                    Me.ComboBox3.SelectedIndex = 5
                    Me.ComboBox3.SelectedText = "（自定义）"
                    If Form1.UnSaveData = 0 Then
                        AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 5, RegistryValueKind.DWord, "HKCU")
                    End If
                    MsgBox(ex.Message & vbCrLf & "时间格式化失败，已重置为默认格式。", MsgBoxStyle.Critical, "错误")
                End Try
                Form1.Timer1.Interval = 1000
                If Form1.DisbFuState = 0 Then
                    TextBox2.Enabled = False
                    Button4.Enabled = False
                End If
                'Form1.GetTimeFormSize(38, 90)
                'a = Form1.Width - Form1.CaW
                'If a <> 0 Then
                '    Form1.Location = New Point(Form1.Location.X + a / 2, Form1.Location.Y)
                '    If Form1.SaveLoc = 1 Then
                '        RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                '        RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                '    End If
                'End If
                'a = 0
                'AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 1, RegistryValueKind.DWord, "HKCU")
                'Form1.SetTimeFormSize(38, 90)
                If Form1.MySize = 0 Then
                    Dim aa As SizeF
                    Dim b As Graphics = Graphics.FromImage(New Bitmap(1, 1))
                    aa = TextRenderer.MeasureText(Form1.Label1.Text, Form1.Label1.Font)
                    Dim c As Integer
                    If aa.Width <= 42 Then
                        Form1.GetTimeFormSize(38, aa.Width + 8)
                    ElseIf 400 <= aa.Width Then
                        Form1.GetTimeFormSize(aa.Height, aa.Width + 10)
                    Else
                        Form1.GetTimeFormSize(38, aa.Width + 6)
                    End If
                    c = Form1.Width - Form1.CaW
                    'If Not (Form1.SaveLoc = 1 And Form1.IsBootV = 1) Then
                    '    If c <> 0 Then
                    '        Form1.Location = New Point(Form1.Location.X + c / 2, Form1.Location.Y)
                    '        If Form1.SaveLoc = 1 Then
                    '            If Form1.UnSaveData = 0 Then
                    '                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                    '                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                    '            End If

                    '        End If
                    '    End If
                    'End If

                    c = 0
                    Form1.SetTimeFormSize(aa.Height, aa.Width)
                    'If aa.Width <= 42 Then
                    '    Form1.SetTimeFormSize(aa.Height, aa.Width + 8)
                    'ElseIf 400 <= aa.Width Then
                    '    Form1.SetTimeFormSize(aa.Height, aa.Width + 10)
                    'Else
                    '    Form1.SetTimeFormSize(aa.Height, aa.Width + 6)
                    'End If
                End If

                If Form1.UnSaveData = 0 Then
                    AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 1, RegistryValueKind.DWord, "HKCU")
                End If

                Form1.AutoSize = False
                Form1.Label1.AutoSize = False

            ElseIf Me.ComboBox3.SelectedIndex = 2 Then
                Form1.TimeF = Me.ComboBox3.Text
                Try
                    Form1.Label1.Text = Format(Now(), Form1.TimeF)
                Catch ex As Exception
                    Form1.TimeF = "距离XX还有" & vbCrLf & "%d天 %H小时 %m分钟 %s秒"
                    Me.ComboBox3.SelectedIndex = 5
                    Me.ComboBox3.SelectedText = "（自定义）"
                    If Form1.UnSaveData = 0 Then
                        AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 5, RegistryValueKind.DWord, "HKCU")
                    End If
                    MsgBox(ex.Message & vbCrLf & "时间格式化失败，已重置为默认格式。", MsgBoxStyle.Critical, "错误")
                End Try
                Form1.Timer1.Interval = 1000
                If Form1.DisbFuState = 0 Then
                    TextBox2.Enabled = False
                    Button4.Enabled = False
                End If
                'Form1.GetTimeFormSize(38, 120)
                'a = Form1.Width - Form1.CaW
                'If a <> 0 Then
                '    Form1.Location = New Point(Form1.Location.X + a / 2, Form1.Location.Y)
                '    If Form1.SaveLoc = 1 Then
                '        RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                '        RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                '    End If
                'End If
                'a = 0
                'AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 2, RegistryValueKind.DWord, "HKCU")
                'Form1.SetTimeFormSize(38, 120)
                If Form1.MySize = 0 Then
                    Dim aa As SizeF
                    Dim b As Graphics = Graphics.FromImage(New Bitmap(1, 1))
                    aa = TextRenderer.MeasureText(Form1.Label1.Text, Form1.Label1.Font)
                    Dim c As Integer
                    If aa.Width <= 42 Then
                        Form1.GetTimeFormSize(38, aa.Width + 8)
                    ElseIf 400 <= aa.Width Then
                        Form1.GetTimeFormSize(aa.Height, aa.Width + 10)
                    Else
                        Form1.GetTimeFormSize(38, aa.Width + 6)
                    End If
                    c = Form1.Width - Form1.CaW
                    'If Not (Form1.SaveLoc = 1 And Form1.IsBootV = 1) Then
                    '    If c <> 0 Then
                    '        Form1.Location = New Point(Form1.Location.X + c / 2, Form1.Location.Y)
                    '        If Form1.SaveLoc = 1 Then
                    '            If Form1.UnSaveData = 0 Then
                    '                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                    '                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                    '            End If

                    '        End If
                    '    End If
                    'End If

                    c = 0
                    Form1.SetTimeFormSize(aa.Height, aa.Width)
                End If

                If Form1.UnSaveData = 0 Then
                    AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 2, RegistryValueKind.DWord, "HKCU")
                End If

                Form1.AutoSize = False
                Form1.Label1.AutoSize = False

            ElseIf Me.ComboBox3.SelectedIndex = 3 Then
                Form1.TimeF = Me.ComboBox3.Text
                Try
                    Form1.Label1.Text = Format(Now(), Form1.TimeF)
                Catch ex As Exception
                    Form1.TimeF = "距离XX还有" & vbCrLf & "%d天 %H小时 %m分钟 %s秒"
                    Me.ComboBox3.SelectedIndex = 5
                    Me.ComboBox3.SelectedText = "（自定义）"
                    If Form1.UnSaveData = 0 Then
                        AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 5, RegistryValueKind.DWord, "HKCU")
                    End If
                    MsgBox(ex.Message & vbCrLf & "时间格式化失败，已重置为默认格式。", MsgBoxStyle.Critical, "错误")
                End Try
                Form1.Timer1.Interval = 1000
                If Form1.DisbFuState = 0 Then
                    TextBox2.Enabled = False
                    Button4.Enabled = False
                End If
                'Form1.GetTimeFormSize(38, 90)
                'a = Form1.Width - Form1.CaW
                'If a <> 0 Then
                '    Form1.Location = New Point(Form1.Location.X + a / 2, Form1.Location.Y)
                '    If Form1.SaveLoc = 1 Then
                '        RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                '        RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                '    End If
                'End If
                'a = 0
                'AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 3, RegistryValueKind.DWord, "HKCU")
                'Form1.SetTimeFormSize(38, 90)
                If Form1.MySize = 0 Then
                    Dim aa As SizeF
                    Dim b As Graphics = Graphics.FromImage(New Bitmap(1, 1))
                    aa = TextRenderer.MeasureText(Form1.Label1.Text, Form1.Label1.Font)
                    Dim c As Integer
                    If aa.Width <= 42 Then
                        Form1.GetTimeFormSize(38, aa.Width + 8)
                    ElseIf 400 <= aa.Width Then
                        Form1.GetTimeFormSize(aa.Height, aa.Width + 10)
                    Else
                        Form1.GetTimeFormSize(38, aa.Width + 6)
                    End If
                    c = Form1.Width - Form1.CaW
                    'If Not (Form1.SaveLoc = 1 And Form1.IsBootV = 1) Then
                    '    If c <> 0 Then
                    '        Form1.Location = New Point(Form1.Location.X + c / 2, Form1.Location.Y)
                    '        If Form1.SaveLoc = 1 Then
                    '            If Form1.UnSaveData = 0 Then
                    '                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                    '                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                    '            End If

                    '        End If
                    '    End If
                    'End If

                    c = 0
                    Form1.SetTimeFormSize(aa.Height, aa.Width)
                End If

                If Form1.UnSaveData = 0 Then
                    AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 3, RegistryValueKind.DWord, "HKCU")
                End If

                Form1.AutoSize = False
                Form1.Label1.AutoSize = False

            ElseIf Me.ComboBox3.SelectedIndex = 4 Then
                Form1.TimeF = Me.ComboBox3.Text
                Try
                    Form1.Label1.Text = Format(Now(), Form1.TimeF)
                Catch ex As Exception
                    Form1.TimeF = "距离XX还有" & vbCrLf & "%d天 %H小时 %m分钟 %s秒"
                    Me.ComboBox3.SelectedIndex = 5
                    Me.ComboBox3.SelectedText = "（自定义）"
                    If Form1.UnSaveData = 0 Then
                        AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 5, RegistryValueKind.DWord, "HKCU")
                    End If
                    MsgBox(ex.Message & vbCrLf & "时间格式化失败，已重置为默认格式。", MsgBoxStyle.Critical, "错误")
                End Try
                Form1.Timer1.Interval = 1000
                If Form1.DisbFuState = 0 Then
                    TextBox2.Enabled = False
                    Button4.Enabled = False
                End If
                'Form1.GetTimeFormSize(38, 400)
                'a = Form1.Width - Form1.CaW
                'If a <> 0 Then
                '    Form1.Location = New Point(Form1.Location.X + a / 2, Form1.Location.Y)
                'End If
                'a = 0
                'AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 4, RegistryValueKind.DWord, "HKCU")
                'Form1.SetTimeFormSize(38, 400)
                If Form1.MySize = 0 Then
                    Dim aa As SizeF
                    Dim b As Graphics = Graphics.FromImage(New Bitmap(1, 1))
                    aa = TextRenderer.MeasureText(Form1.Label1.Text, Form1.Label1.Font)
                    Dim c As Integer
                    If aa.Width <= 42 Then
                        Form1.GetTimeFormSize(38, aa.Width + 8)
                    ElseIf 400 <= aa.Width Then
                        Form1.GetTimeFormSize(aa.Height, aa.Width + 10)
                    Else
                        Form1.GetTimeFormSize(38, aa.Width + 6)
                    End If
                    c = Form1.Width - Form1.CaW
                    'If Not (Form1.SaveLoc = 1 And Form1.IsBootV = 1) Then
                    '    If c <> 0 Then
                    '        Form1.Location = New Point(Form1.Location.X + c / 2, Form1.Location.Y)
                    '        If Form1.SaveLoc = 1 Then
                    '            If Form1.UnSaveData = 0 Then
                    '                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                    '                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                    '            End If

                    '        End If
                    '    End If
                    'End If

                    c = 0
                    Form1.SetTimeFormSize(aa.Height, aa.Width)
                End If

                If Form1.UnSaveData = 0 Then
                    AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 4, RegistryValueKind.DWord, "HKCU")
                End If

                Form1.AutoSize = False
                Form1.Label1.AutoSize = False
            Else
                If Form1.UnSaveData = 0 Then
                    AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 5, RegistryValueKind.DWord, "HKCU")
                End If

                If Form1.DisbFuState = 0 Then
                    TextBox2.Enabled = True
                    Button4.Enabled = True
                End If
                Call Button4_Click(sender, e)
                'Form1.AutoSize = True
            End If
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical, "错误")
        End Try
    End Sub
    Public Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        Try
            Form1.TimeF = Me.TextBox2.Text
            'Form1.Label1.AutoSize = True
            Try
                Form1.Label1.Text = Format(Now(), Form1.TimeF)
            Catch ex As Exception
                Form1.TimeF = "距离XX还有" & vbCrLf & "%d天 %H小时 %m分钟 %s秒"
                Me.TextBox2.Text = "距离XX还有" & vbCrLf & "%d天 %H小时 %m分钟 %s秒"
                If Form1.UnSaveData = 0 Then
                    RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomFormat", "距离XX还有" & vbCrLf & "%d天 %H小时 %m分钟 %s秒", RegistryValueKind.String, "HKCU")
                End If
                MsgBox(ex.Message & vbCrLf & "时间格式化失败，已重置为默认格式。", MsgBoxStyle.Critical, "错误")
            End Try
            If Form1.TimeF.Contains("%k") = True Then
                Form1.Timer1.Interval = 100
            Else
                Form1.Timer1.Interval = 1000
            End If
            If Form1.MySize = 0 Then
                Dim a As SizeF
                Dim b As Graphics = Graphics.FromImage(New Bitmap(1, 1))
                a = TextRenderer.MeasureText(Form1.Label1.Text, Form1.Label1.Font)
                Dim c As Integer
                If a.Width <= 42 Then
                    Form1.GetTimeFormSize(38, a.Width + 8)
                ElseIf 400 <= a.Width Then
                    Form1.GetTimeFormSize(a.Height, a.Width + 26)
                Else
                    Form1.GetTimeFormSize(38, a.Width + 6)
                End If
                c = Form1.Width - Form1.CaW
                'If c <> 0 Then
                '    Form1.Location = New Point(Form1.Location.X + c / 2, Form1.Location.Y)
                '    If Form1.SaveLoc = 1 Then
                '        If Form1.UnSaveData = 0 Then
                '            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                '            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                '        End If

                '    End If
                'End If
                c = 0
                Form1.SetTimeFormSize(a.Height + 3, a.Width + 3)
            End If

            'Form1.Label1.AutoSize = False
            If Form1.UnSaveData = 0 Then
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomFormat", TextBox2.Text, RegistryValueKind.String, "HKCU")
            End If

        Catch ex As Exception
            MsgBox("设置自定义格式失败，已重置为默认格式。", MsgBoxStyle.Critical, "错误")
            Me.TextBox2.Text = "距离XX还有" & vbCrLf & "%d天 %H小时 %m分钟 %s秒"
        End Try
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        If MessageBox.Show("                                 " & vbCrLf & "确定要恢复默认设置吗？" & vbCrLf & "执行该操作会把设置恢复到默认的状态，并删除自定义内容，此操作无法撤销。" & vbCrLf & vbCrLf & "你确定要继续吗？" & vbCrLf & "                                 ", "警告 - 恢复默认设置", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation) = Windows.Forms.DialogResult.Yes Then
            '如果预先关联事件， Me.CheckBox1.Checked = Ture / Flase 操作会触发事件，导致操作相反
            'RemoveHandler CheckBox1.CheckedChanged, AddressOf CheckBox1_CheckedChanged
            'RemoveHandler CheckBox2.CheckedChanged, AddressOf CheckBox2_CheckedChanged
            'RemoveHandler CheckBox3.CheckedChanged, AddressOf CheckBox3_CheckedChanged
            'RemoveHandler CheckBox4.CheckedChanged, AddressOf CheckBox4_CheckedChanged
            Try
                AddReg("Software\CJH\CountDownControl\Settings", "ColorMode", 0, Microsoft.Win32.RegistryValueKind.DWord, "HKCU")


                If Form1.UnSupportDarkSys = 1 Then
                    Form1.appcolor = 1
                    Form1.crmd = 1
                Else
                    Form1.appcolor = 0
                    'Get System Color
                    Dim regkey As RegistryKey = Registry.CurrentUser.OpenSubKey("SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize", True)
                    Dim sysacr As Integer
                    Try
                        If (Not regkey Is Nothing) Then
                            'If (If((regkey.GetValue("") Is Nothing), Nothing, regkey.GetValue("").ToString) <> "AppsUseLightTheme") Then
                            'End If
                            sysacr = regkey.GetValue("AppsUseLightTheme", -1)
                            If sysacr = 0 Then
                                Form1.crmd = 0
                            ElseIf sysacr = 1 Then
                                Form1.crmd = 1
                            Else
                                Form1.crmd = 1
                            End If
                        Else
                            Form1.crmd = 1
                        End If
                    Catch ex As Exception
                        Form1.crmd = 1
                    End Try
                    regkey.Close()
                End If
                ComboBox2.SelectedIndex = 0
                ComboBox1.SelectedIndex = 0

                AddReg("Software\CJH\CountDownControl\Settings", "EnableDrag", 1, Microsoft.Win32.RegistryValueKind.DWord, "HKCU")


                Form1.UseMoveV = 1
                CheckBox1.Checked = True

                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "AllowTopMost", 1, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormat", 5, RegistryValueKind.DWord, "HKCU")


                Form1.TopMost = True
                CheckBox2.Checked = True

                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "SaveLocations", 1, RegistryValueKind.DWord, "HKCU")

                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownYear", 3000, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownMonth", 1, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownDay", 1, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownHour", 23, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownMinute", 59, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownSecond", 59, RegistryValueKind.DWord, "HKCU")
                Me.DateTimePicker1.Value = New Date(3000, 1, 1, 23, 59, 59, 0)
                Form1.SetDate = DateTimePicker1.Value
                Form1.SaveLoc = 1
                CheckBox3.Checked = True

                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "UseCustomSize", 0, RegistryValueKind.DWord, "HKCU")


                Form1.MySize = 0
                CheckBox4.Checked = False

                RegKeyModule.DelReg("Software\CJH\CountDownControl\Settings", "CustomHeight", "HKCU")
                RegKeyModule.DelReg("Software\CJH\CountDownControl\Settings", "CustomWidth", "HKCU")



                Form1.Label1.Font = New System.Drawing.Font("Segoe UI", 21.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point)
                Me.FontDialog1.Font = Form1.Label1.Font

                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFont", Form1.Label1.Font.Name, RegistryValueKind.String, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontPx", Form1.Label1.Font.Size, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontItalic", Form1.Label1.Font.Italic, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontBold", Form1.Label1.Font.Bold, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontUnderLine", Form1.Label1.Font.Underline, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontStrikeout", Form1.Label1.Font.Strikeout, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.DelReg("Software\CJH\CountDownControl\Settings", "TimeFormX", "HKCU")
                RegKeyModule.DelReg("Software\CJH\CountDownControl\Settings", "TimeFormY", "HKCU")


                ComboBox3.SelectedIndex = 5
                Call ComboBox3_SelectedIndexChanged(sender, e)

                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomFormat", "距离XX还有" & vbCrLf & "yyyy-m-d HH:mm:ss", RegistryValueKind.String, "HKCU")

                Call Button4_Click(sender, e)

                If Form1.crmd = 1 Then
                    Form1.Label1.ForeColor = Color.Black
                Else
                    Form1.Label1.ForeColor = Color.White
                End If

                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontR", Form1.Label1.ForeColor.R, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontG", Form1.Label1.ForeColor.G, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontB", Form1.Label1.ForeColor.B, RegistryValueKind.DWord, "HKCU")

                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeTheme", 0, RegistryValueKind.DWord, "HKCU")
                Form1.TimeTheme = 0
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomThemePath", "", RegistryValueKind.String, "HKCU")



                TextBox1.Text = ""
                TextBox2.Text = "距离XX还有" & vbCrLf & "%d天 %H小时 %m分钟 %s秒"
                TextBox3.Text = Form1.Width
                TextBox4.Text = Form1.Height
                TextBox5.Text = ""

                Form1.MySize = 0

                AddReg("Software\CJH\CountDownControl\Settings", "UseCustomSize", 0, RegistryValueKind.DWord, "HKCU")


                TextBox3.Enabled = False
                TextBox4.Enabled = False
                Button9.Enabled = False

                'Dim aa As SizeF
                'Dim b As Graphics = Graphics.FromImage(New Bitmap(1, 1))
                'aa = TextRenderer.MeasureText(Form1.Label1.Text, Form1.Label1.Font)
                'Dim c As Integer
                'Form1.SetTimeFormSize(aa.Height, aa.Width)
                'c = Form1.Width - Form1.CaW
                'If Not (Form1.SaveLoc = 1 And Form1.IsBootV = 1) Then
                '    If c <> 0 Then
                '        Form1.Location = New Point(Form1.Location.X + c / 2, Form1.Location.Y)
                '    End If
                'End If

                Form1.Location = New Size((System.Windows.Forms.SystemInformation.PrimaryMonitorSize.Width - Form1.Width) / 2, 5)


                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormOpacity", 100, RegistryValueKind.DWord, "HKCU")


                Form1.CustOpacity = 100
                Label17.Text = "99%"
                Form1.Opacity = 0.99
                TrackBar1.Value = 99

                Call Form1.formatcolorcur()
                Call formatcolorcurset()
                Call GPLForm.formatcolorcursetmsg()


                '如果预先关联事件， Me.CheckBox1.Checked = Ture / Flase 操作会触发事件，导致操作相反
                'AddHandler CheckBox1.CheckedChanged, AddressOf CheckBox1_CheckedChanged
                'AddHandler CheckBox2.CheckedChanged, AddressOf CheckBox2_CheckedChanged
                'AddHandler CheckBox3.CheckedChanged, AddressOf CheckBox3_CheckedChanged
                'AddHandler CheckBox4.CheckedChanged, AddressOf CheckBox4_CheckedChanged
            Catch ex As Exception
                MsgBox("恢复默认设置失败。" & vbCrLf & ex.Message, MsgBoxStyle.Critical, "错误")
            End Try
        End If
    End Sub

    Private Sub Button6_Click(sender As System.Object, e As System.EventArgs) Handles Button6.Click
        Form1.NotifyIcon1.Visible = True
        Form1.NotifyIcon1.ShowBalloonTip(7000, "倒计时小工具", "倒计时小工具当前已隐藏到系统托盘，双击托盘图标重新显示。", ToolTipIcon.Info)
        Form1.Hide()
        Me.Close()
    End Sub

    Private Sub CheckBox3_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckBox3.CheckedChanged
        If CheckBox3.CheckState = False Then
            ' CheckBox3.Checked = False
            Form1.SaveLoc = 0
            If Form1.UnSaveData = 0 Then
                AddReg("Software\CJH\CountDownControl\Settings", "SaveLocations", 0, RegistryValueKind.DWord, "HKCU")
            End If

        Else
            ' CheckBox3.Checked = True
            Form1.SaveLoc = 1
            If Form1.UnSaveData = 0 Then
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                AddReg("Software\CJH\CountDownControl\Settings", "SaveLocations", 1, RegistryValueKind.DWord, "HKCU")
            End If

        End If
    End Sub

    Private Sub Button7_Click(sender As System.Object, e As System.EventArgs) Handles Button7.Click
        If FontDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Form1.Label1.Font = FontDialog1.Font
            'New System.Drawing.Font("微软雅黑", 11.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(134, Byte))
            If Form1.UnSaveData = 0 Then
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFont", Form1.Label1.Font.Name, RegistryValueKind.String, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontPx", Form1.Label1.Font.Size, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontItalic", Form1.Label1.Font.Italic, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontBold", Form1.Label1.Font.Bold, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontUnderLine", Form1.Label1.Font.Underline, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontStrikeout", Form1.Label1.Font.Strikeout, RegistryValueKind.DWord, "HKCU")
            End If

            If Form1.MySize = 0 Then
                Dim aa As SizeF
                Dim b As Graphics = Graphics.FromImage(New Bitmap(1, 1))
                aa = TextRenderer.MeasureText(Form1.Label1.Text, Form1.Label1.Font)
                Dim c As Integer
                If aa.Width <= 42 Then
                    Form1.GetTimeFormSize(38, aa.Width + 8)
                ElseIf 400 <= aa.Width Then
                    Form1.GetTimeFormSize(aa.Height, aa.Width + 10)
                Else
                    Form1.GetTimeFormSize(38, aa.Width + 6)
                End If
                c = Form1.Width - Form1.CaW
                If c <> 0 Then
                    Form1.Location = New Point(Form1.Location.X + c / 2, Form1.Location.Y)
                    If Form1.SaveLoc = 1 Then
                        If Form1.UnSaveData = 0 Then
                            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                        End If

                    End If
                End If
                c = 0
                If aa.Width <= 42 Then
                    Form1.SetTimeFormSize(aa.Height, aa.Width + 8)
                ElseIf 400 <= aa.Width Then
                    Form1.SetTimeFormSize(aa.Height, aa.Width + 10)
                Else
                    Form1.SetTimeFormSize(aa.Height, aa.Width + 6)
                End If
            End If
        End If
    End Sub

    Private Sub Button8_Click(sender As System.Object, e As System.EventArgs) Handles Button8.Click
        If ColorDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Form1.Label1.ForeColor = ColorDialog1.Color
            If Form1.UnSaveData = 0 Then
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontR", Form1.Label1.ForeColor.R, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontG", Form1.Label1.ForeColor.G, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFontB", Form1.Label1.ForeColor.B, RegistryValueKind.DWord, "HKCU")
            End If

        End If
    End Sub

    Private Sub PictureBox1_Click(sender As System.Object, e As System.EventArgs) Handles PictureBox1.Click
        MessageBox.Show("以下字符代表其对应的值：" & vbCrLf & _
                        "%d  倒计时剩余天数" & vbCrLf & _
                        "%H  倒计时剩余小时数" & vbCrLf & _
                        "%m  倒计时剩余分钟数" & vbCrLf & _
                        "%s  倒计时剩余秒数" & vbCrLf & _
                        "%k  倒计时剩余毫秒数", _
                        "帮助", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub

    Private Sub CheckBox4_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles CheckBox4.CheckedChanged
        'If Form1.MySize = 1 Then
        If Me.CheckBox4.Checked = False Then
            'CheckBox4.CheckState = False
            Form1.MySize = 0
            If Form1.UnSaveData = 0 Then
                AddReg("Software\CJH\CountDownControl\Settings", "UseCustomSize", 0, RegistryValueKind.DWord, "HKCU")
            End If

            If Form1.DisbFuState = 0 Then
                TextBox3.Enabled = False
                TextBox4.Enabled = False
                Button9.Enabled = False
            End If


            Dim aa As SizeF
            Dim b As Graphics = Graphics.FromImage(New Bitmap(1, 1))
            aa = TextRenderer.MeasureText(Form1.Label1.Text, Form1.Label1.Font)
            Dim c As Integer
            If aa.Width <= 42 Then
                Form1.GetTimeFormSize(38, aa.Width + 8)
            ElseIf 400 <= aa.Width Then
                Form1.GetTimeFormSize(aa.Height, aa.Width + 10)
            Else
                Form1.GetTimeFormSize(38, aa.Width + 6)
            End If
            c = Form1.Width - Form1.CaW
            If Not (Form1.SaveLoc = 1 And Form1.IsBootV = 1) Then
                'If c <> 0 Then
                '    Form1.Location = New Point(Form1.Location.X + c / 2, Form1.Location.Y)
                '    If Form1.SaveLoc = 1 Then
                '        If Form1.UnSaveData = 0 Then
                '            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                '            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                '        End If

                '    End If
                'End If
            End If

            c = 0
            Form1.SetTimeFormSize(aa.Height, aa.Width)
        Else
            'CheckBox4.CheckState = True
            Form1.MySize = 1
            If Form1.UnSaveData = 0 Then
                AddReg("Software\CJH\CountDownControl\Settings", "UseCustomSize", 1, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomHeight", TextBox4.Text, RegistryValueKind.DWord, "HKCU")
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomWidth", TextBox3.Text, RegistryValueKind.DWord, "HKCU")
            End If
            If Form1.DisbFuState = 0 Then
                TextBox3.Enabled = True
                TextBox4.Enabled = True
                Button9.Enabled = True
            End If

        End If
    End Sub
    Private Sub TextBox3_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox3.KeyPress
        Dim strlimit As String
        strlimit = "0123456789"
        Dim keychar As Char = e.KeyChar
        If InStr(strlimit, keychar) <> 0 Or e.KeyChar = Microsoft.VisualBasic.ChrW(8) Then
            'If keychar = "." And InStr(TextBox1.Text, keychar) <> 0 Then
            'e.Handled = True
            'Else
            e.Handled = False
            'End If
        Else
            e.Handled = True
        End If
    End Sub
    Private Sub TextBox4_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox4.KeyPress
        Dim strlimit As String
        strlimit = "0123456789"
        Dim keychar As Char = e.KeyChar
        If InStr(strlimit, keychar) <> 0 Or e.KeyChar = Microsoft.VisualBasic.ChrW(8) Then
            'If keychar = "." And InStr(TextBox1.Text, keychar) <> 0 Then
            'e.Handled = True
            'Else
            e.Handled = False
            'End If
        Else
            e.Handled = True
        End If
    End Sub

    Private Sub Button9_Click(sender As System.Object, e As System.EventArgs) Handles Button9.Click
        Try
            If TextBox3.Text = 0 And TextBox4.Text = 0 Then
                MsgBox("设置自定义大小失败。" & vbCrLf & "大小不能为0。", MsgBoxStyle.Critical, "错误")
            ElseIf TextBox3.Text = "" And TextBox4.Text = "" Then
                MsgBox("设置自定义大小失败。" & vbCrLf & "大小不能为空。", MsgBoxStyle.Critical, "错误")
            Else
                Dim c As Integer
                c = TextBox3.Text - Form1.Width

                Form1.Width = TextBox3.Text
                Form1.Height = TextBox4.Text
                If Form1.UnSaveData = 0 Then
                    RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomHeight", Form1.Height, RegistryValueKind.DWord, "HKCU")
                    RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomWidth", Form1.Width, RegistryValueKind.DWord, "HKCU")
                End If


                If Form1.TimeTheme = 0 Then
                    If Form1.Width >= 250 Then
                        If Form1.crmd = 0 Then
                            Form1.BackgroundImage = My.Resources.bkgdark400
                        Else
                            Form1.BackgroundImage = My.Resources.bkg400
                        End If
                    ElseIf Form1.Width <= 70 Then
                        If Form1.crmd = 0 Then
                            Form1.BackgroundImage = My.Resources.bkgdark50
                        Else
                            Form1.BackgroundImage = My.Resources.bkg50
                        End If
                    Else
                        If Form1.crmd = 0 Then
                            Form1.BackgroundImage = My.Resources.bkgdark
                        Else
                            Form1.BackgroundImage = My.Resources.bkg
                        End If
                    End If
                End If

                If Not (Form1.SaveLoc = 1 And Form1.IsBootV = 1) Then
                    If c <> 0 Then
                        Form1.Location = New Point(Form1.Location.X + c / 2, Form1.Location.Y)
                        If Form1.SaveLoc = 1 Then
                            If Form1.UnSaveData = 0 Then
                                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormX", Form1.Location.X, RegistryValueKind.DWord, "HKCU")
                                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormY", Form1.Location.Y, RegistryValueKind.DWord, "HKCU")
                            End If
                        End If
                    End If
                End If

                c = 0
            End If
        Catch ex As Exception
            MsgBox("设置自定义大小失败。" & vbCrLf & ex.Message, MsgBoxStyle.Critical, "错误")
        End Try
    End Sub

    Private Sub ComboBox4_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles ComboBox4.SelectedIndexChanged
        If ComboBox4.SelectedIndex = 0 Then
            If Form1.UnSaveData = 0 Then
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeTheme", 0, RegistryValueKind.DWord, "HKCU")
            End If
            If Form1.DisbFuState = 0 Then
                Button10.Enabled = False
                Button11.Enabled = False
                TextBox5.Enabled = False
            End If

            Form1.TimeTheme = 0
            Call Form1.formatcolorcur()
            If Form1.DisbFuState = 0 Then
                TrackBar1.Enabled = False
            End If
            Label17.Text = "99%"
            TrackBar1.Value = 99
            Form1.Opacity = 0.99
        ElseIf ComboBox4.SelectedIndex = 1 Then
            If Form1.UnSaveData = 0 Then
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeTheme", 1, RegistryValueKind.DWord, "HKCU")
            End If
            If Form1.DisbFuState = 0 Then
                Button10.Enabled = False
                Button11.Enabled = False
                TextBox5.Enabled = False
            End If

            Form1.TimeTheme = 1
            Call Form1.formatcolorcur()
            If Form1.DisbFuState = 0 Then
                TrackBar1.Enabled = False
            End If
            Label17.Text = "70%"
            TrackBar1.Value = 70
            Form1.BackgroundImage = Nothing
            Form1.Opacity = 0.7
        ElseIf ComboBox4.SelectedIndex = 2 Then
            If Form1.UnSaveData = 0 Then
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeTheme", 2, RegistryValueKind.DWord, "HKCU")
            End If
            If Form1.DisbFuState = 0 Then
                Button10.Enabled = True
                Button11.Enabled = True
                TextBox5.Enabled = True
            End If

            Form1.TimeTheme = 2
            Call Form1.formatcolorcur()
            If Form1.DisbFuState = 0 Then
                TrackBar1.Enabled = True
            End If

            Form1.Opacity = Form1.CustOpacity * 0.01
            Label17.Text = Form1.CustOpacity & "%"
            TrackBar1.Value = Form1.CustOpacity
        Else
            If Form1.UnSaveData = 0 Then
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeTheme", 3, RegistryValueKind.DWord, "HKCU")
            End If
            If Form1.DisbFuState = 0 Then
                Button10.Enabled = False
                Button11.Enabled = False
                TextBox5.Enabled = False
            End If

            Form1.TimeTheme = 3
            Call Form1.formatcolorcur()
            If Form1.DisbFuState = 0 Then
                TrackBar1.Enabled = True
            End If
            Form1.Opacity = Form1.CustOpacity * 0.01
            Label17.Text = Form1.CustOpacity & "%"
            TrackBar1.Value = Form1.CustOpacity
            Form1.BackgroundImage = Nothing
            Form1.TransparencyKey = Color.FromArgb(255, 0, 255)
        End If
    End Sub

    Private Sub Button11_Click(sender As System.Object, e As System.EventArgs) Handles Button11.Click
        If MessageBox.Show("确定要清除自定义背景吗？" & vbCrLf & "这将恢复背景到默认圆角主题。", "提示", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = Windows.Forms.DialogResult.Yes Then
            TextBox5.Text = ""
            If Form1.Width >= 250 Then
                If Form1.crmd = 0 Then
                    Form1.BackgroundImage = My.Resources.bkgdark400
                Else
                    Form1.BackgroundImage = My.Resources.bkg400
                End If
            ElseIf Form1.Width <= 70 Then
                If Form1.crmd = 0 Then
                    Form1.BackgroundImage = My.Resources.bkgdark50
                Else
                    Form1.BackgroundImage = My.Resources.bkg50
                End If
            Else
                If Form1.crmd = 0 Then
                    Form1.BackgroundImage = My.Resources.bkgdark
                Else
                    Form1.BackgroundImage = My.Resources.bkg
                End If
            End If
            If Form1.crmd = 0 Then
                Form1.TransparencyKey = Color.FromArgb(1, 1, 1)
            Else
                Form1.TransparencyKey = Color.FromArgb(184, 184, 184)
            End If

            OpenFileDialog1.FileName = ""
            If Form1.UnSaveData = 0 Then
                RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomThemePath", "", RegistryValueKind.String, "HKCU")
            End If

        End If
    End Sub

    Private Sub Button10_Click(sender As System.Object, e As System.EventArgs) Handles Button10.Click
        If OpenFileDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            If IO.File.Exists(OpenFileDialog1.FileName) Then
                Try
                    Form1.BackgroundImage = Image.FromFile(OpenFileDialog1.FileName)
                    If Form1.UnSaveData = 0 Then
                        RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomThemePath", OpenFileDialog1.FileName, RegistryValueKind.String, "HKCU")
                    End If

                    TextBox5.Text = OpenFileDialog1.FileName
                    Form1.TransparencyKey = Color.FromArgb(255, 0, 255)
                Catch ex As Exception
                    MsgBox("加载自定义背景图片失败。已恢复默认图片。" & vbCrLf & ex.Message, MsgBoxStyle.Critical, "错误")
                    OpenFileDialog1.FileName = ""
                    TextBox5.Text = ""
                    If Form1.crmd = 0 Then
                        Form1.TransparencyKey = Color.FromArgb(1, 1, 1)
                    Else
                        Form1.TransparencyKey = Color.FromArgb(184, 184, 184)
                    End If
                    If Form1.Width >= 250 Then
                        If Form1.crmd = 0 Then
                            Form1.BackgroundImage = My.Resources.bkgdark400
                        Else
                            Form1.BackgroundImage = My.Resources.bkg400
                        End If
                    ElseIf Form1.Width <= 70 Then
                        If Form1.crmd = 0 Then
                            Form1.BackgroundImage = My.Resources.bkgdark50
                        Else
                            Form1.BackgroundImage = My.Resources.bkg50
                        End If
                    Else
                        If Form1.crmd = 0 Then
                            Form1.BackgroundImage = My.Resources.bkgdark
                        Else
                            Form1.BackgroundImage = My.Resources.bkg
                        End If
                    End If
                    If Form1.UnSaveData = 0 Then
                        RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CustomThemePath", "", RegistryValueKind.String, "HKCU")
                    End If

                End Try
            End If
        End If
    End Sub

    Private Sub TrackBar1_Scroll(sender As System.Object, e As System.EventArgs) Handles TrackBar1.Scroll
        Form1.CustOpacity = Me.TrackBar1.Value
        If Form1.UnSaveData = 0 Then
            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "TimeFormOpacity", Form1.CustOpacity, RegistryValueKind.DWord, "HKCU")
        End If

        Label17.Text = Form1.CustOpacity & "%"
        Form1.Opacity = Form1.CustOpacity * 0.01
    End Sub

    Private Sub LinkLabel1_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        System.Diagnostics.Process.Start("https://github.com/cjhdevact/CountDownControl")
    End Sub

    Private Sub LinkLabel2_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel2.LinkClicked
        System.Diagnostics.Process.Start("https://github.com/cjhdevact/CountDownControl/issues")
    End Sub

    Private Sub LinkLabel3_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel3.LinkClicked
        If MessageBox.Show("                                 " & vbCrLf & "确定要删除自定义配置并退出程序吗？" & vbCrLf & "执行该操作会删除本机倒计时小工具的自定义设置并退出，相当于清除在本机的设置，此操作无法撤销。" & vbCrLf & vbCrLf & "你确定要继续吗？" & vbCrLf & "                                 ", "警告 - 删除自定义配置并退出", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation) = Windows.Forms.DialogResult.Yes Then
            If Form1.UnSaveData = 0 Then
                RegKeyModule.DelKey("Software\CJH\CountDownControl", True, "HKCU")
            End If
            End
        End If
    End Sub

    Private Sub LinkLabel4_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel4.LinkClicked
        GPLForm.ShowDialog()
    End Sub

    Private Sub LinkLabel5_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel5.LinkClicked
        MessageBox.Show("当前支持的命令行：" & vbCrLf & "/safemode 以安全模式加载，不读取设置也不保存设置。当程序由于配置原因无法正常启动，可以使用该命令行启动后恢复默认设置。" & vbCrLf & "/noproflie 不使用配置文件。" & vbCrLf & "/nosaveprofile 读取设置但不保存设置" & vbCrLf & vbCrLf & "部分功能可能因为策略设置而不可用。命令行的内容要优先于策略设置，为所有用户设置的策略优先级高于针对单一用户设置的策略。", "帮助", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub

    Private Sub Button12_Click(sender As System.Object, e As System.EventArgs) Handles Button12.Click
        If (MessageBox.Show("确定重启倒计时小工具吗？", "倒计时小工具", MessageBoxButtons.YesNo, MessageBoxIcon.Question, MessageBoxDefaultButton.Button2) = DialogResult.Yes) Then
            System.Diagnostics.Process.Start(Application.ExecutablePath)
            Form1.Timer1.Enabled = False
            Form1.Timer2.Enabled = False
            'End
            Application.Exit()
        End If
    End Sub

    Private Sub TextBox_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
        Dim strlimit As String
        strlimit = "0123456789"
        Dim keychar As Char = e.KeyChar
        If InStr(strlimit, keychar) <> 0 Or e.KeyChar = Microsoft.VisualBasic.ChrW(8) Then
            'If keychar = "." And InStr(TextBox1.Text, keychar) <> 0 Then
            'e.Handled = True
            'Else
            e.Handled = False
            'End If
        Else
            e.Handled = True
        End If
    End Sub

    Private Sub TextBox7_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
        Call TextBox_KeyPress(sender, e)
    End Sub

    Private Sub TextBox8_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
        Call TextBox_KeyPress(sender, e)
    End Sub

    Private Sub TextBox6_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
        Call TextBox_KeyPress(sender, e)
    End Sub

    Private Sub DateTimePicker1_ValueChanged(sender As System.Object, e As System.EventArgs) Handles DateTimePicker1.ValueChanged
        'If TextBox6.Text = "" Or TextBox7.Text = "" Or TextBox8.Text = "" Then
        '    MessageBox.Show("目标时间格式不正确！" & vbCrLf, _
        '             "倒计时小工具 - 错误", MessageBoxButtons.OK, MessageBoxIcon.Error)
        '    Exit Sub
        'End If
        'If Integer.Parse(TextBox6.Text) > 24 Or Integer.Parse(TextBox7.Text) > 60 Or Integer.Parse(TextBox8.Text) > 60 Then
        '    MessageBox.Show("目标时间格式不正确！" & vbCrLf, _
        '             "倒计时小工具 - 错误", MessageBoxButtons.OK, MessageBoxIcon.Error)
        '    Exit Sub
        'End If
        Form1.SetDate = DateTimePicker1.Value
        'Form1.SetHour = Integer.Parse(TextBox6.Text)
        'Form1.SetMinute = Integer.Parse(TextBox7.Text)
        'Form1.SetSecond = Integer.Parse(TextBox8.Text)
        If Form1.UnSaveData = 0 Then
            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownYear", Form1.SetDate.Year, RegistryValueKind.DWord, "HKCU")
            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownMonth", Form1.SetDate.Month, RegistryValueKind.DWord, "HKCU")
            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownDay", Form1.SetDate.Day, RegistryValueKind.DWord, "HKCU")
            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownHour", Form1.SetDate.Hour, RegistryValueKind.DWord, "HKCU")
            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownMinute", Form1.SetDate.Minute, RegistryValueKind.DWord, "HKCU")
            RegKeyModule.AddReg("Software\CJH\CountDownControl\Settings", "CountDownSecond", Form1.SetDate.Second, RegistryValueKind.DWord, "HKCU")
        End If
    End Sub
End Class