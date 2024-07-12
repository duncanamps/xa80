(Get-ChildItem -Path . -Recurse) | % {$_.LastWriteTime = (Get-Date)}
