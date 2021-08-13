
$PIDS = New-Object System.Collections.Generic.List[System.Object];

For ($i=0; $i -le $args[0]; $i++) {
   Start-Process -FilePath "D:\abey-wallet\testfiles\CleanClient.exe";
   

}

Sleep($args[1]);

Stop-Process -processname CleanClient