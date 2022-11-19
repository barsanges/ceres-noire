REM  *****  BASIC  *****

Sub LaunchCeresNoire

	GlobalScope.BasicLibraries.LoadLibrary("ScriptForge")
	Dim FSO As Object
	Set FSO = CreateScriptService("FileSystem")
	FSO.FileNaming = "SYS"

	MySheet = ThisComponent.CurrentController.ActiveSheet

	MySheet.getCellRangeByName("E2:E9999").ClearContents(7)

	low = MySheet.getCellRangeByName("B1").Value
	up = MySheet.getCellRangeByName("B2").Value
	nstamps = MySheet.getCellRangeByName("B3").Value

	inventory = "price;quantity"
	For idx = 10 To 750
		value = MySheet.getCellRangeByName("B" & idx).Value
		number = MySheet.getCellRangeByName("C" & idx).Value
		if value > 0 Then
			inventory = inventory & chr(13) & CStr(value) & ";" & number
		Else
			Exit For
		End If
	Next
	' CStr ne permet apparemment pas (?) de préciser le séparateur décimal à utiliser
	' on le fait donc à la main :
	inventory = replace(inventory, ",", ".")
	low = replace(CStr(low), ",", ".")
	up = replace(CStr(up), ",", ".")

	cmd = FSO.BuildPath(FSO.HomeFolder, "..\AppData\Local\ceres-noire\cn-0.2.0.exe") & " " & low & " " & up & " " & nstamps & " -s " & chr(34) & inventory & chr(34)
	res = ShellRun(cmd)
	splitted = SF_String.SplitLines(res)

	idx = 2
	For Each elem in splitted
		TmpCel = MySheet.getCellRangeByName("E" & idx)
		TmpCel.String = elem
		idx = idx + 1
	Next

End Sub

Public Function ShellRun(sCmd As String) As String
' https://ask.libreoffice.org/t/how-to-get-the-stdout-result-of-a-basic-shell-order/20030/2

    ' Run a shell command, returning the output as a string
    Dim oShell As Object
    oShell = CreateObject("WScript.Shell")

    ' Run command
    Dim oExec As Object
    Dim oOutput As Object

    oExec = oShell.Exec(sCmd)
    oOutput = oExec.StdOut

    ' Handle the results as they are written to and read from the StdOut object
    Dim s As String
    Dim sLine As String
    While Not oOutput.AtEndOfStream
        sLine = oOutput.ReadLine
        If sLine <> "" Then s = s & sLine & chr(10)
    Wend

    ShellRun = s

End Function